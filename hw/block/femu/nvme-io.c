
#include "./nvme.h"
#include <pthread.h>

static uint16_t nvme_io_cmd(FemuCtrl *n, NvmeCmd *cmd, NvmeRequest *req);

//by HH: ZNS function ////////////////////////////////////////////////////
#include "zns/zns.h"

static inline uint32_t zns_zone_idx(NvmeNamespace *ns, uint64_t slba)
{
    FemuCtrl *n = ns->ctrl;

    return (n->zone_size_log2 > 0 ? slba >> n->zone_size_log2 : slba /
            n->zone_size);
}

static inline NvmeZone *zns_get_zone_by_slba(NvmeNamespace *ns, uint64_t slba)
{
    FemuCtrl *n = ns->ctrl;
    uint32_t zone_idx = zns_zone_idx(ns, slba);

    assert(zone_idx < n->num_zones);
    return &n->zone_array[zone_idx];
}

static void zns_assign_zone_state(NvmeNamespace *ns, NvmeZone *zone,
                                  NvmeZoneState state)
{
    FemuCtrl *n = ns->ctrl;

    if (QTAILQ_IN_USE(zone, entry)) {
        switch (zns_get_zone_state(zone)) {
        case NVME_ZONE_STATE_EXPLICITLY_OPEN:
            QTAILQ_REMOVE(&n->exp_open_zones, zone, entry);
            break;
        case NVME_ZONE_STATE_IMPLICITLY_OPEN:
            QTAILQ_REMOVE(&n->imp_open_zones, zone, entry);
            break;
        case NVME_ZONE_STATE_CLOSED:
            QTAILQ_REMOVE(&n->closed_zones, zone, entry);
            break;
        case NVME_ZONE_STATE_FULL:
            QTAILQ_REMOVE(&n->full_zones, zone, entry);
        default:
            ;
        }
    }

    zns_set_zone_state(zone, state);

    switch (state) {
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        QTAILQ_INSERT_TAIL(&n->exp_open_zones, zone, entry);
        break;
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        QTAILQ_INSERT_TAIL(&n->imp_open_zones, zone, entry);
        break;
    case NVME_ZONE_STATE_CLOSED:
        QTAILQ_INSERT_TAIL(&n->closed_zones, zone, entry);
        break;
    case NVME_ZONE_STATE_FULL:
        QTAILQ_INSERT_TAIL(&n->full_zones, zone, entry);
    case NVME_ZONE_STATE_READ_ONLY:
        break;
    default:
        zone->d.za = 0;
    }
}

static void zns_auto_transition_zone(NvmeNamespace *ns, int debug)
{
    FemuCtrl *n = ns->ctrl;
    NvmeZone *zone;

    pthread_mutex_lock(&lock_nr_open);
    if (n->max_open_zones &&
        n->nr_open_zones == n->max_open_zones) {
        zone = QTAILQ_FIRST(&n->imp_open_zones);
        if (zone) {
            h_log_zone("here4 n->nr_open_zones:%d = n->max_open_zones:%d, debug:%d\n",
                        n->nr_open_zones, n->max_open_zones, debug);
             /* Automatically close this implicitly open zone */
            QTAILQ_REMOVE(&n->imp_open_zones, zone, entry);
            zns_aor_dec_open_debug(ns, 1);

            zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_CLOSED);
            //h_log_zone("nr_open--(%d), zonewp(0x%lx) %d\n", ns->ctrl->nr_open_zones, zone->w_ptr, debug);
        }
    }
    pthread_mutex_unlock(&lock_nr_open);

}

static int zns_aor_check(NvmeNamespace *ns, uint32_t act, uint32_t opn)
{
    FemuCtrl *n = ns->ctrl;
    if (n->max_active_zones != 0 && n->nr_active_zones + act > n->max_active_zones)
    {
        printf("too many active! max_active: %d, nr_active: %d\n", n->max_active_zones, n->nr_active_zones+act);
        return NVME_ZONE_TOO_MANY_ACTIVE | NVME_DNR;
    }
    if (n->max_open_zones != 0 && n->nr_open_zones + opn > n->max_open_zones)
    {
        printf("too many open! max_open: %d, nr_open: %d\n", n->max_open_zones, n->nr_open_zones);
        return NVME_ZONE_TOO_MANY_OPEN | NVME_DNR;
    }

    return NVME_SUCCESS;
}

static uint16_t zns_auto_open_zone(NvmeNamespace *ns, NvmeZone *zone)
{
    uint16_t status = NVME_SUCCESS;
    uint8_t zs = zns_get_zone_state(zone);

    if (zs == NVME_ZONE_STATE_EMPTY) {
        zns_auto_transition_zone(ns, 1);
        status = zns_aor_check(ns, 1, 1);
    } else if (zs == NVME_ZONE_STATE_CLOSED) {
        zns_auto_transition_zone(ns, 2);
        status = zns_aor_check(ns, 0, 1);
    }

    return status;
}

static uint64_t zns_advance_zone_wp(NvmeNamespace *ns, NvmeZone *zone,
                                    uint32_t nlb, int debug)
{
    uint64_t result = zone->w_ptr;
    uint8_t zs;

    zone->w_ptr += nlb;

    if (zone->w_ptr < zns_zone_wr_boundary(zone)) {
        zs = zns_get_zone_state(zone);
        switch (zs) {
        case NVME_ZONE_STATE_EMPTY:
            zns_aor_inc_active(ns);
            //h_log_zone("nr_active++(%d), zonewp(0x%lx) %d\n", ns->ctrl->nr_active_zones, zone->w_ptr, debug);
            /* fall through */
        case NVME_ZONE_STATE_CLOSED:
            zns_aor_inc_open(ns);
            //h_log_zone("nr_open++(%d), advzonewp(0x%lx) %d ", ns->ctrl->nr_open_zones, zone->w_ptr, debug);
            zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_IMPLICITLY_OPEN);
        }
    }

    return result;
}

static uint16_t zns_map_dptr(FemuCtrl *n, size_t len, NvmeRequest *req)
{
    uint64_t prp1, prp2;

    switch (req->cmd.psdt) {
    case NVME_PSDT_PRP:
        prp1 = le64_to_cpu(req->cmd.dptr.prp1);
        prp2 = le64_to_cpu(req->cmd.dptr.prp2);

        return nvme_map_prp(&req->qsg, &req->iov, prp1, prp2, len, n);
    default:
        return NVME_INVALID_FIELD;
    }
}

static uint16_t zns_check_zone_state_for_write(NvmeZone *zone)
{
    uint16_t status;

    switch (zns_get_zone_state(zone)) {
    case NVME_ZONE_STATE_RESERVED:
    case NVME_ZONE_STATE_EMPTY:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_CLOSED:
        status = NVME_SUCCESS;
        break;
    case NVME_ZONE_STATE_FULL:
        printf("*********ZONE STATUS FULL********\n");
        status = NVME_ZONE_FULL;
        break;
    case NVME_ZONE_STATE_OFFLINE:
        printf("*********ZONE OFFLINE********\n");
        status = NVME_ZONE_OFFLINE;
        break;
    case NVME_ZONE_STATE_READ_ONLY:
        printf("*********ZONE READ ONLY********\n");
        status = NVME_ZONE_READ_ONLY;
        break;
    default:
        assert(false);
    }

    return status;
}

static uint16_t zns_check_zone_write(FemuCtrl *n, NvmeNamespace *ns,
                                      NvmeZone *zone, uint64_t slba,
                                      uint32_t nlb, bool append)
{
    uint16_t status;

    if (unlikely((slba + nlb) > zns_zone_wr_boundary(zone))) {
        printf("*********ZONE NVME_ZONE_BOUNDARY_ERROR Error*********\n");
        status = NVME_ZONE_BOUNDARY_ERROR;
    } else {
        status = zns_check_zone_state_for_write(zone);
    }

    if (status != NVME_SUCCESS)
    { 
        printf("slba: 0x%lx, nlb: 0x%x\n", slba, nlb);
    }
    else
    {
        assert(zns_wp_is_valid(zone));
        if (append)
        {
            if (unlikely(slba != zone->d.zslba)) {
                 printf("ZONE INVALID FIELD Error! slba(0x%lx) == zone->d.zslba(0x%lx)\n", slba, zone->d.zslba);
                status = NVME_INVALID_FIELD;
            }
            if (zns_l2b(ns, nlb) > (n->page_size << n->zasl)) {
                 printf("ZONE INVALID FIELD Error! byte(%ld) > (%d)n->page_size<<(%d)n->zasl(%d)\n",
                        zns_l2b(ns, nlb), n->page_size, n->zasl, (n->page_size << n->zasl));
                status = NVME_INVALID_FIELD;
            }
        }
        //by HH: need to fix, currently no wptr check
        // else if (unlikely(slba != zone->w_ptr))
        // {
        //     printf(" *********ZONE INVALID WRITE Error*********\n");
        //     printf("slba: 0x%lx / wptr: 0x%lx\n", slba, zone->w_ptr);
        //     status = NVME_ZONE_INVALID_WRITE;
        // }
    }

    return status;
}

static uint16_t zns_check_zone_state_for_read(NvmeZone *zone)
{
    uint16_t status;

    switch (zns_get_zone_state(zone)) {
    case NVME_ZONE_STATE_RESERVED:
    case NVME_ZONE_STATE_EMPTY:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_FULL:
    case NVME_ZONE_STATE_CLOSED:
    case NVME_ZONE_STATE_READ_ONLY:
        status = NVME_SUCCESS;
        break;
    case NVME_ZONE_STATE_OFFLINE:
        status = NVME_ZONE_OFFLINE;
        break;
    default:
        assert(false);
    }

    return status;
}

static uint16_t zns_check_zone_read(NvmeNamespace *ns, uint64_t slba,
                                    uint32_t nlb)
{
    FemuCtrl *n = ns->ctrl;
    NvmeZone *zone = zns_get_zone_by_slba(ns, slba);
    uint64_t bndry = zns_zone_rd_boundary(ns, zone);
    uint64_t end = slba + nlb;
    uint16_t status;

    status = zns_check_zone_state_for_read(zone);
    if (status != NVME_SUCCESS) {
        ;
    } else if (unlikely(end > bndry)) {
        if (!n->cross_zone_read) {
            status = NVME_ZONE_BOUNDARY_ERROR;
        } else {
            /*
             * Read across zone boundary - check that all subsequent
             * zones that are being read have an appropriate state.
             */
            do {
                zone++;
                status = zns_check_zone_state_for_read(zone);
                if (status != NVME_SUCCESS) {
                    break;
                }
            } while (end > zns_zone_rd_boundary(ns, zone));
        }
    }

    return status;
}

static void zns_finalize_zoned_write(NvmeNamespace *ns, NvmeRequest *req,
                                     bool failed)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)&req->cmd;
    NvmeZone *zone;
    NvmeZonedResult *res = (NvmeZonedResult *)&req->cqe;
    uint64_t slba;
    uint32_t nlb;

    slba = le64_to_cpu(rw->slba);
    nlb = le16_to_cpu(rw->nlb) + 1;
    zone = zns_get_zone_by_slba(ns, slba);
    zone->d.wp += nlb;

    if (failed) {
        res->slba = 0;
    }

    if (zone->d.wp == zns_zone_wr_boundary(zone)) {
        switch (zns_get_zone_state(zone)) {
        case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        case NVME_ZONE_STATE_EXPLICITLY_OPEN:
            pthread_mutex_lock(&lock_nr_open);
            zns_aor_dec_open_debug(ns, 2);
            pthread_mutex_unlock(&lock_nr_open);            
            //h_log_zone("write slc: nr_open--(%d)\n", ns->ctrl->nr_open_zones);

            /* fall through */
        case NVME_ZONE_STATE_CLOSED:
            zns_aor_dec_active(ns);
            //h_log_zone("nr_active--(%d), zonewp(0x%lx)\n", ns->ctrl->nr_active_zones, zone->w_ptr);
            /* fall through */
        case NVME_ZONE_STATE_EMPTY:

            zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_FULL);

            /* fall through */
        case NVME_ZONE_STATE_FULL:
            
            break;
        default:
            assert(false);
        }
    }
}
//////////////////////////////////////////////////////////////////////////

static void nvme_update_sq_eventidx(const NvmeSQueue *sq)
{
    if (sq->eventidx_addr_hva) {
        *((uint32_t *)(sq->eventidx_addr_hva)) = sq->tail;
        return;
    }

    if (sq->eventidx_addr) {
        nvme_addr_write(sq->ctrl, sq->eventidx_addr, (void *)&sq->tail,
                        sizeof(sq->tail));
    }
}

static inline void nvme_copy_cmd(NvmeCmd *dst, NvmeCmd *src)
{
#if defined(__AVX__)
    __m256i *d256 = (__m256i *)dst;
    const __m256i *s256 = (const __m256i *)src;

    _mm256_store_si256(&d256[0], _mm256_load_si256(&s256[0]));
    _mm256_store_si256(&d256[1], _mm256_load_si256(&s256[1]));
#elif defined(__SSE2__)
    __m128i *d128 = (__m128i *)dst;
    const __m128i *s128 = (const __m128i *)src;

    _mm_store_si128(&d128[0], _mm_load_si128(&s128[0]));
    _mm_store_si128(&d128[1], _mm_load_si128(&s128[1]));
    _mm_store_si128(&d128[2], _mm_load_si128(&s128[2]));
    _mm_store_si128(&d128[3], _mm_load_si128(&s128[3]));
#else
    *dst = *src;
#endif
}

#include "bbssd/ftl.h"
static void nvme_process_sq_io(void *opaque, int index_poller)
{
    NvmeSQueue *sq = opaque;
    FemuCtrl *n = sq->ctrl;

    struct ssd *ssd = n->ssd;
    struct ssdparams *spp = &ssd->sp;

    uint16_t status;
    hwaddr addr;
    NvmeCmd cmd;
    NvmeRequest *req;
    int processed = 0;
    int randvalue = 0;

    nvme_update_sq_tail(sq);
    while (!(nvme_sq_empty(sq))) {
        if (sq->phys_contig) {
            addr = sq->dma_addr + sq->head * n->sqe_size;
            nvme_copy_cmd(&cmd, (void *)&(((NvmeCmd *)sq->dma_addr_hva)[sq->head]));
        } else {
            addr = nvme_discontig(sq->prp_list, sq->head, n->page_size,
                                  n->sqe_size);
            nvme_addr_read(n, addr, (void *)&cmd, sizeof(cmd));
        }
        nvme_inc_sq_head(sq);

        req = QTAILQ_FIRST(&sq->req_list);
        QTAILQ_REMOVE(&sq->req_list, req, entry);
        memset(&req->cqe, 0, sizeof(req->cqe));
        /* Coperd: record req->stime at earliest convenience */
        req->expire_time = req->stime = qemu_clock_get_ns(QEMU_CLOCK_REALTIME);
        req->cqe.cid = cmd.cid;
        req->cmd_opcode = cmd.opcode;
        memcpy(&req->cmd, &cmd, sizeof(NvmeCmd));

       //* by HH
        if(cmd.opcode == NVME_CMD_WRITE ||cmd.opcode == NVME_CMD_ZONE_APPEND)
        {
            req->slba = cmd.cdw10 | ((uint64_t)cmd.cdw11<<32);
            req->cmd.cdw10 = req->slba & 0xFFFFFFFF;
            req->cmd.cdw11 = req->slba >> 32;
//printf("nvme-io write: oricmd slba:0x%lx, nlb:0x%x, cmd.cdw10:0x%x, cmd.cdw11:0x%x\n",
    //req->slba, req->nlb, cmd.cdw10, cmd.cdw11);
//printf("slba: 0x%lx nlb:0x%x\n", req->slba, req->nlb);

            //NvmeZone *zone;
            NvmeZone *ori_zone;
            //uint16_t zone_index=0;
            uint64_t req_slba = 0;

            ori_zone = zns_get_zone_by_slba(n->namespaces, req->slba);
            if (zns_check_zone_write(n, n->namespaces, ori_zone, req->slba, cmd.cdw12, false)) {
                femu_err("*********nvme sq ZONE check Error*********\n");
                break;
            }

            // zone = n->zone_array;
            // // move to end of the SLC region
            // while (zone->d.zone_flash_type != SLC || zone->d.zs>>4 == 0xD || zone->d.zs>>4 == 0xE || zone->d.zs>>4 == 0xF )
            // {
            //     if(zone_index >= n->num_zones)
            //     {
            //         zone = n->zone_array;
            //         break;
            //     }
            //     zone_index++;
            //     zone++;
            // }
            uint64_t line_size = (uint32_t)spp->secs_per_pg*(uint32_t)spp->pgs_per_blk*(uint32_t)spp->nchs*(uint32_t)spp->luns_per_ch;
            uint64_t line_cap = slc_line_boundary*line_size;
            pthread_mutex_lock(&lock_slc_wp);

            if(H_TEST_LOG)
            {
                if(req->slba > (100*n->zone_array->d.zcap) )
                {
                    randvalue = 5;
                }
            }

            if( (randvalue > 3) || cmd.opcode == NVME_CMD_ZONE_APPEND || slc_line_boundary == 0
                || ( ((slc_wp + cmd.cdw12 + 1)) >= (line_cap - (2*n->num_zones) - line_size) )
                || IN_SLC_GC )
            {
                //* SLC FULL, to Overprovisioning?
                slctbl *tbl = rslc.mapslc;
                tbl += ((req->slba)/n->zone_capacity);
                //slc_mapping *map_tbl = tbl->slcmap;

                //if((slc_wp + cmd.cdw12) < (zone->d.zslba + zone->d.zcap))
                if( (randvalue < 4) && cmd.opcode != NVME_CMD_ZONE_APPEND
                     && (slc_line_boundary > 0) && ((slc_wp + cmd.cdw12 + 1)) < (line_cap-line_size) && !IN_SLC_GC )
                {
                    h_log_provision("Over-provisioning? zone[%ld] SLC Data: %ld, DataRemain=%ld\n",
                        ((req->slba)/n->zone_capacity), tbl->num_slc_data, tbl->num_slc_data%3);
                    
                    if(tbl->num_slc_data%3 == 0)
                    {
                        pthread_mutex_unlock(&lock_slc_wp);
                        h_log_provision("tbl_Data:%ld, no over-provision!, to TLC\n", tbl->num_slc_data%3);
                    }
                    else
                    {
                         h_log_provision("Over-provision!, zone_slba:0x%lx, zone_cap: 0x%lx\n",
                             zone->d.zslba, zone->d.zcap);
                         h_log_provision("wp: 0x%lx, req nlb: 0x%x, cmd nlb: 0x%x, req.slba: 0x%lx\n",
                             slc_wp, req->cmd.cdw12, cmd.cdw12, req->slba); 

                        req_slba = req->slba;

                        //cmd.cdw10 = slc_wp & 0xFFFFFFFF;
                        //cmd.cdw11 = slc_wp >> 32;

                        slc_wp += cmd.cdw12+1;
                        pthread_mutex_unlock(&lock_slc_wp);

                        req->cmd.cdw15 = 0x89; //slc flag

                        ori_zone->d.wp += cmd.cdw12 + 1;

                        //* by HH: ori-zone open
                        zns_auto_transition_zone(n->namespaces, 3);
                        zns_advance_zone_wp(n->namespaces, ori_zone, cmd.cdw12+1, 1);
                        //*

                        if (ori_zone->d.wp == zns_zone_wr_boundary(ori_zone))
                        {
                            //printf("here1 orizone_wp:0x%lx, Zboundary:0x%lx\n",
                                   // ori_zone->d.wp, zns_zone_wr_boundary(ori_zone));
                            switch (zns_get_zone_state(ori_zone))
                            {
                            case NVME_ZONE_STATE_IMPLICITLY_OPEN:
                            case NVME_ZONE_STATE_EXPLICITLY_OPEN:
                                pthread_mutex_lock(&lock_nr_open);
                                zns_aor_dec_open_debug(n->namespaces, 3);
                                pthread_mutex_unlock(&lock_nr_open);
                                //h_log_zone("write slc: nr_open--, ori_zone->d.wp(0x%lx)\n",ori_zone->d.wp);

                                /* fall through */
                            case NVME_ZONE_STATE_CLOSED:
                                zns_aor_dec_active(n->namespaces);
                                //h_log_zone("write slc: nr_active--\n");
                                /* fall through */
                            case NVME_ZONE_STATE_EMPTY:

                                zns_assign_zone_state(n->namespaces, ori_zone, NVME_ZONE_STATE_FULL);

                                /* fall through */
                            case NVME_ZONE_STATE_FULL:
                                
                                break;
                            default:
                                assert(false);
                            }
                        }

                        req->slba = cmd.cdw10 | ((uint64_t)cmd.cdw11<<32);
                        req->cmd.cdw10 = cmd.cdw10;
                        req->cmd.cdw11 = cmd.cdw11;
                        set_mapslc_ent(ssd, ((req_slba)/n->zone_capacity), req->slba, cmd.cdw12, req_slba);
                    }
                }
                else
                {
                    //if(randvalue > 3) printf("cold data! to TLC\n");
                    pthread_mutex_unlock(&lock_slc_wp);
                    h_log_provision("Cannot write to SLC region\n");
                }
            }
            else
            {
                req_slba = req->slba;
                
                //cmd.cdw10 = slc_wp & 0xFFFFFFFF;
                //cmd.cdw11 = slc_wp >> 32;

                slc_wp += cmd.cdw12+1; 
                pthread_mutex_unlock(&lock_slc_wp);

                req->cmd.cdw15 = 0x89; //slc flag

                ori_zone->d.wp += cmd.cdw12 + 1;

                //* by HH: ori-zone open
                zns_auto_transition_zone(n->namespaces, 4);
                zns_advance_zone_wp(n->namespaces, ori_zone, cmd.cdw12+1, 2);
                //*

                if (ori_zone->d.wp == zns_zone_wr_boundary(ori_zone))
                {
                    switch (zns_get_zone_state(ori_zone))
                    {
                    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
                    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
                        //h_log_zone("write slc: nr_open--, ori_zone->d.wp(0x%lx)\n",ori_zone->d.wp);
                        pthread_mutex_lock(&lock_nr_open);
                        zns_aor_dec_open_debug(n->namespaces, 4);                        
                        pthread_mutex_unlock(&lock_nr_open);


                        /* fall through */
                    case NVME_ZONE_STATE_CLOSED:
                        //h_log_zone("write slc: nr_active--\n");
                        zns_aor_dec_active(n->namespaces);
                        
                        /* fall through */
                    case NVME_ZONE_STATE_EMPTY:

                        zns_assign_zone_state(n->namespaces, ori_zone, NVME_ZONE_STATE_FULL);

                        /* fall through */
                    case NVME_ZONE_STATE_FULL:
                        
                        break;
                    default:
                        assert(false);
                    }
                }

                req->slba = cmd.cdw10 | ((uint64_t)cmd.cdw11<<32);
                req->cmd.cdw10 = cmd.cdw10;
                req->cmd.cdw11 = cmd.cdw11;

                set_mapslc_ent(ssd, ((req_slba)/n->zone_capacity), req->slba, cmd.cdw12, req_slba);
            }

        }
        else if(cmd.opcode == NVME_CMD_READ)
        {
            //cmd.cdw10 += TLC_START_LBA & 0xFFFFFFFF;
            //cmd.cdw11 += TLC_START_LBA >> 32;

            req->slba = cmd.cdw10 | ((uint64_t)cmd.cdw11<<32);
            req->cmd.cdw10 = req->slba & 0xFFFFFFFF;
            req->cmd.cdw11 = req->slba >> 32;
//printf("nvme-io read: oricmd slba:0x%lx, nlb:0x%x, cmd.cdw10:0x%x, cmd.cdw11:0x%x\n",
    //req->slba, req->nlb, cmd.cdw10, cmd.cdw11);
            slctbl *tbl = rslc.mapslc;

            tbl += ((req->slba)/n->zone_capacity);

            h_log_readcmd("read target slba: 0x%lx\n", req->slba);

            slc_mapping *map_tbl = tbl->slcmap;

            for(int i=0; i < tbl->num_slc_data; i++)
            {
                h_log_readcmd("map_tbl[%d] slc_addr:0x%lx, nlb: 0x%x, map_tbl_target_slba:0x%lx, req->slba:0x%lx\n",
                    i, map_tbl->zdslba, map_tbl->zdnlb, map_tbl->target_addr, req->slba);
                if( (map_tbl->target_addr <= req->slba) &&
                    ((map_tbl->target_addr + map_tbl->zdnlb) >= req->slba) && map_tbl->isvalid == true)
                {
                    cmd.cdw10 = (map_tbl->zdslba + req->slba - map_tbl->target_addr) & 0xFFFFFFFF;
                    cmd.cdw11 = (map_tbl->zdslba + req->slba - map_tbl->target_addr) >> 32;

                    req->slba = cmd.cdw10 | ((uint64_t)cmd.cdw11<<32);
                    req->cmd.cdw10 = req->slba & 0xFFFFFFFF;
                    req->cmd.cdw11 = req->slba >> 32;

                    i = tbl->num_slc_data;
                }
                //printf("nvme-io read: slccmd slba:0x%lx, nlb:0x%x\n", req->slba, req->nlb);
                map_tbl++;
            }
        }

        if (n->print_log) {
            femu_debug("%s,cid:%d\n", __func__, cmd.cid);
        }
        status = nvme_io_cmd(n, &cmd, req);
        if (1 && status == NVME_SUCCESS) {
            req->status = status;
            int rc = femu_ring_enqueue(n->to_ftl[index_poller], (void *)&req, 1);
            if (rc != 1) {
                femu_err("enqueue failed, ret=%d\n", rc);
            }
        } else if (status == NVME_SUCCESS) {
            /* Normal I/Os that don't need delay emulation */
            req->status = status;
        } else {
            printf("Error IO processed!\n");
            sleep(10000);
        }

        processed++;
    }

    nvme_update_sq_eventidx(sq);
    sq->completed += processed;
}

static void nvme_post_cqe(NvmeCQueue *cq, NvmeRequest *req)
{
    FemuCtrl *n = cq->ctrl;
    NvmeSQueue *sq = req->sq;
    NvmeCqe *cqe = &req->cqe;
    uint8_t phase = cq->phase;
    hwaddr addr;

    if (n->print_log) {
        femu_debug("%s,req,lba:%lu,lat:%lu\n", n->devname, req->slba, req->reqlat);
    }
    cqe->status = cpu_to_le16((req->status << 1) | phase);
    cqe->sq_id = cpu_to_le16(sq->sqid);
    cqe->sq_head = cpu_to_le16(sq->head);

    if (cq->phys_contig) {
        addr = cq->dma_addr + cq->tail * n->cqe_size;
        ((NvmeCqe *)cq->dma_addr_hva)[cq->tail] = *cqe;
    } else {
        addr = nvme_discontig(cq->prp_list, cq->tail, n->page_size, n->cqe_size);
        nvme_addr_write(n, addr, (void *)cqe, sizeof(*cqe));
    }

    nvme_inc_cq_tail(cq);
}

static void nvme_process_cq_cpl(void *arg, int index_poller)
{
    FemuCtrl *n = (FemuCtrl *)arg;
    NvmeCQueue *cq = NULL;
    NvmeRequest *req = NULL;
    struct rte_ring *rp = n->to_ftl[index_poller];
    pqueue_t *pq = n->pq[index_poller];
    uint64_t now;
    int processed = 0;
    int rc;

    if (BBSSD(n)) {
        rp = n->to_poller[index_poller];
    }

    while (femu_ring_count(rp)) {
        req = NULL;
        rc = femu_ring_dequeue(rp, (void *)&req, 1);
        if (rc != 1) {
            femu_err("dequeue from to_poller request failed\n");
        }
        assert(req);

        pqueue_insert(pq, req);
    }

    while ((req = pqueue_peek(pq))) {
        now = qemu_clock_get_ns(QEMU_CLOCK_REALTIME);
        if (now < req->expire_time) {
            break;
        }

        cq = n->cq[req->sq->sqid];
        if (!cq->is_active)
            continue;
        nvme_post_cqe(cq, req);
        QTAILQ_INSERT_TAIL(&req->sq->req_list, req, entry);
        pqueue_pop(pq);
        processed++;
        n->nr_tt_ios++;
        if (now - req->expire_time >= 20000) {
            n->nr_tt_late_ios++;
            if (n->print_log) {
                femu_debug("%s,diff,pq.count=%lu,%" PRId64 ", %lu/%lu\n",
                           n->devname, pqueue_size(pq), now - req->expire_time,
                           n->nr_tt_late_ios, n->nr_tt_ios);
            }
        }
        n->should_isr[req->sq->sqid] = true;
    }

    if (processed == 0)
        return;

    switch (n->multipoller_enabled) {
    case 1:
        nvme_isr_notify_io(n->cq[index_poller]);
        break;
    default:
        for (int i = 1; i <= n->num_io_queues; i++) {
            if (n->should_isr[i]) {
                nvme_isr_notify_io(n->cq[i]);
                n->should_isr[i] = false;
            }
        }
        break;
    }
}

static void *nvme_poller(void *arg)
{
    FemuCtrl *n = ((NvmePollerThreadArgument *)arg)->n;
    int index = ((NvmePollerThreadArgument *)arg)->index;

    switch (n->multipoller_enabled) {
    case 1:
        while (1) {
            if ((!n->dataplane_started)) {
                usleep(1000);
                continue;
            }

            NvmeSQueue *sq = n->sq[index];
            NvmeCQueue *cq = n->cq[index];
            if (sq && sq->is_active && cq && cq->is_active) {
                nvme_process_sq_io(sq, index);
            }
            nvme_process_cq_cpl(n, index);
        }
        break;
    default:
        while (1) {
            if ((!n->dataplane_started)) {
                usleep(1000);
                continue;
            }

            for (int i = 1; i <= n->num_io_queues; i++) {
                NvmeSQueue *sq = n->sq[i];
                NvmeCQueue *cq = n->cq[i];
                if (sq && sq->is_active && cq && cq->is_active) {
                    nvme_process_sq_io(sq, index);
                }
            }
            nvme_process_cq_cpl(n, index);
        }
        break;
    }

    return NULL;
}

static int cmp_pri(pqueue_pri_t next, pqueue_pri_t curr)
{
    return (next > curr);
}

static pqueue_pri_t get_pri(void *a)
{
    return ((NvmeRequest *)a)->expire_time;
}

static void set_pri(void *a, pqueue_pri_t pri)
{
    ((NvmeRequest *)a)->expire_time = pri;
}

static size_t get_pos(void *a)
{
    return ((NvmeRequest *)a)->pos;
}

static void set_pos(void *a, size_t pos)
{
    ((NvmeRequest *)a)->pos = pos;
}

void nvme_create_poller(FemuCtrl *n)
{
    n->should_isr = g_malloc0(sizeof(bool) * (n->num_io_queues + 1));

    n->num_poller = n->multipoller_enabled ? n->num_io_queues : 1;
    /* Coperd: we put NvmeRequest into these rings */
    n->to_ftl = malloc(sizeof(struct rte_ring *) * (n->num_poller + 1));
    for (int i = 1; i <= n->num_poller; i++) {
        n->to_ftl[i] = femu_ring_create(FEMU_RING_TYPE_MP_SC, FEMU_MAX_INF_REQS);
        if (!n->to_ftl[i]) {
            femu_err("failed to create ring (n->to_ftl) ...\n");
            abort();
        }
        assert(rte_ring_empty(n->to_ftl[i]));
    }

    n->to_poller = malloc(sizeof(struct rte_ring *) * (n->num_poller + 1));
    for (int i = 1; i <= n->num_poller; i++) {
        n->to_poller[i] = femu_ring_create(FEMU_RING_TYPE_MP_SC, FEMU_MAX_INF_REQS);
        if (!n->to_poller[i]) {
            femu_err("failed to create ring (n->to_poller) ...\n");
            abort();
        }
        assert(rte_ring_empty(n->to_poller[i]));
    }

    n->pq = malloc(sizeof(pqueue_t *) * (n->num_poller + 1));
    for (int i = 1; i <= n->num_poller; i++) {
        n->pq[i] = pqueue_init(FEMU_MAX_INF_REQS, cmp_pri, get_pri, set_pri,
                               get_pos, set_pos);
        if (!n->pq[i]) {
            femu_err("failed to create pqueue (n->pq) ...\n");
            abort();
        }
    }

    n->poller = malloc(sizeof(QemuThread) * (n->num_poller + 1));
    NvmePollerThreadArgument *args = malloc(sizeof(NvmePollerThreadArgument) *
                                            (n->num_poller + 1));
    for (int i = 1; i <= n->num_poller; i++) {
        args[i].n = n;
        args[i].index = i;
        qemu_thread_create(&n->poller[i], "nvme-poller", nvme_poller, &args[i],
                           QEMU_THREAD_JOINABLE);
        femu_debug("nvme-poller [%d] created ...\n", i - 1);
    }
}

uint16_t nvme_rw(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd, NvmeRequest *req, bool is_append)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint16_t ctrl = le16_to_cpu(rw->control);
    uint32_t nlb  = le16_to_cpu(rw->nlb) + 1;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint64_t prp1 = le64_to_cpu(rw->prp1);
    uint64_t prp2 = le64_to_cpu(rw->prp2);
    const uint8_t lba_index = NVME_ID_NS_FLBAS_INDEX(ns->id_ns.flbas);
    const uint16_t ms = le16_to_cpu(ns->id_ns.lbaf[lba_index].ms);
    const uint8_t data_shift = ns->id_ns.lbaf[lba_index].lbads;
    uint64_t data_size = (uint64_t)nlb << data_shift;
    uint64_t data_offset = slba << data_shift;
    //uint64_t data_offset;
    uint64_t meta_size = nlb * ms;
    uint64_t elba = slba + nlb;
    uint16_t err;
    int ret;

    req->is_write = (rw->opcode == NVME_CMD_WRITE || rw->opcode ==NVME_CMD_ZONE_APPEND) ? 1 : 0;

    NvmeZone *zone;

    err = femu_nvme_rw_check_req(n, ns, cmd, req, slba, elba, nlb, ctrl,
                                 data_size, meta_size);
    if (err)
    {
        femu_err("nvme_rw_check error\n");
        return err;
    }

    if (nvme_map_prp(&req->qsg, &req->iov, prp1, prp2, data_size, n))
    {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, prp1), 0, ns->id);
        femu_err("map prp error\n");
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    assert((nlb << data_shift) == req->qsg.size);

    req->slba = slba;
    req->status = NVME_SUCCESS;
    req->nlb = nlb;

    // by HH: ZNS IO check ////////////////////////////////////////////////
    zone = zns_get_zone_by_slba(ns, slba);

    //if (nvme_check_mdts(n, data_size)) {
    if(n->mdts && data_size > n->page_size<<n->mdts) {
        femu_err("hoonhwi:*********ZONE Check MDTS Error*********\n");
        return NVME_INVALID_FIELD;
    }
    

    //if (zns_check_bounds(ns, slba, nlb)) {
    if ( unlikely(UINT64_MAX - slba < nlb || slba + nlb > le64_to_cpu(ns->id_ns.nsze)) ) {
        femu_err("hoonhwi:*********ZONE Check Bounds Error*********\n");
        return NVME_INVALID_FIELD;
    }

    if(req->is_write)
    {
        if(req->cmd.cdw15 != 0x89)
        {
            if (zns_check_zone_write(n, ns, zone, slba, nlb, is_append)) {
                femu_err("*********nvme rw ZONE check Error*********\n");
                femu_err("slba: 0x%lx, nlb: 0x%x\n", slba, nlb);
                return NVME_INVALID_FIELD;
            }

            if (zns_auto_open_zone(ns, zone)) {
                femu_err("*********ZONE Open Error*********\n");
                return NVME_INVALID_FIELD;
            }

            if (is_append) {
                slba = zone->w_ptr;
                data_offset = zns_l2b(ns, slba);
            }

            NvmeZonedResult *res = (NvmeZonedResult *)&req->cqe;
            res->slba = zns_advance_zone_wp(ns, zone, nlb, 3);         

            //data_offset = zns_l2b(ns, slba);

            if (zns_map_dptr(n, data_size, req)) {
                femu_err("hoonhwi:*********ZONE Map DPTR Error*********\n");
                return NVME_INVALID_FIELD;
            }
        }
    }
    else
    {
        if (zns_check_zone_read(ns, slba, nlb)) {
            femu_err("hoonhwi:*********ZONE check Error*********\n");
            return NVME_INVALID_FIELD;
        }

        if (zns_map_dptr(n, data_size, req)) {
            femu_err("hoonhwi:*********ZONE Map DPTR Error*********\n");
            return NVME_INVALID_FIELD;
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    ret = backend_rw(n->mbe, &req->qsg, &data_offset, req->is_write);
    
    if (ret) {
        femu_err("backend rw error\n");
        return NVME_DNR;
    }

    if(req->is_write && req->cmd.cdw15 != 0x89)
    {
        zns_finalize_zoned_write(ns, req, false); 
    }

    return NVME_SUCCESS;
}

static uint16_t nvme_dsm(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                         NvmeRequest *req)
{
    uint32_t dw10 = le32_to_cpu(cmd->cdw10);
    uint32_t dw11 = le32_to_cpu(cmd->cdw11);
    uint64_t prp1 = le64_to_cpu(cmd->dptr.prp1);
    uint64_t prp2 = le64_to_cpu(cmd->dptr.prp2);

    if (dw11 & NVME_DSMGMT_AD) {
        uint16_t nr = (dw10 & 0xff) + 1;

        uint64_t slba;
        uint32_t nlb;
        NvmeDsmRange range[nr];

        if (dma_write_prp(n, (uint8_t *)range, sizeof(range), prp1, prp2)) {
            nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                                offsetof(NvmeCmd, dptr.prp1), 0, ns->id);
            return NVME_INVALID_FIELD | NVME_DNR;
        }

        req->status = NVME_SUCCESS;
        for (int i = 0; i < nr; i++) {
            slba = le64_to_cpu(range[i].slba);
            nlb = le32_to_cpu(range[i].nlb);
            if (slba + nlb > le64_to_cpu(ns->id_ns.nsze)) {
                nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                                    offsetof(NvmeCmd, cdw10), slba + nlb, ns->id);
                return NVME_LBA_RANGE | NVME_DNR;
            }

            bitmap_clear(ns->util, slba, nlb);
        }
    }

    return NVME_SUCCESS;
}

static uint16_t nvme_compare(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                             NvmeRequest *req)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint32_t nlb  = le16_to_cpu(rw->nlb) + 1;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint64_t prp1 = le64_to_cpu(rw->prp1);
    uint64_t prp2 = le64_to_cpu(rw->prp2);

    uint64_t elba = slba + nlb;
    uint8_t lba_index = NVME_ID_NS_FLBAS_INDEX(ns->id_ns.flbas);
    uint8_t data_shift = ns->id_ns.lbaf[lba_index].lbads;
    uint64_t data_size = nlb << data_shift;
    uint64_t offset  = ns->start_block + (slba << data_shift);

    if ((slba + nlb) > le64_to_cpu(ns->id_ns.nsze)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                            offsetof(NvmeRwCmd, nlb), elba, ns->id);
        return NVME_LBA_RANGE | NVME_DNR;
    }
    if (n->id_ctrl.mdts && data_size > n->page_size * (1 << n->id_ctrl.mdts)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, nlb), nlb, ns->id);
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if (nvme_map_prp(&req->qsg, &req->iov, prp1, prp2, data_size, n)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, prp1), 0, ns->id);
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if (find_next_bit(ns->uncorrectable, elba, slba) < elba) {
        return NVME_UNRECOVERED_READ;
    }

    for (int i = 0; i < req->qsg.nsg; i++) {
        uint32_t len = req->qsg.sg[i].len;
        uint8_t tmp[2][len];

        nvme_addr_read(n, req->qsg.sg[i].base, tmp[1], len);
        if (memcmp(tmp[0], tmp[1], len)) {
            qemu_sglist_destroy(&req->qsg);
            return NVME_CMP_FAILURE;
        }
        offset += len;
    }

    qemu_sglist_destroy(&req->qsg);

    return NVME_SUCCESS;
}

static uint16_t nvme_flush(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                           NvmeRequest *req)
{
    return NVME_SUCCESS;
}

static uint16_t nvme_write_zeros(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                                 NvmeRequest *req)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint32_t nlb  = le16_to_cpu(rw->nlb) + 1;

    if ((slba + nlb) > ns->id_ns.nsze) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                            offsetof(NvmeRwCmd, nlb), slba + nlb, ns->id);
        return NVME_LBA_RANGE | NVME_DNR;
    }

    return NVME_SUCCESS;
}

static uint16_t nvme_write_uncor(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                                 NvmeRequest *req)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint32_t nlb  = le16_to_cpu(rw->nlb) + 1;

    if ((slba + nlb) > ns->id_ns.nsze) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                            offsetof(NvmeRwCmd, nlb), slba + nlb, ns->id);
        return NVME_LBA_RANGE | NVME_DNR;
    }

    bitmap_set(ns->uncorrectable, slba, nlb);

    return NVME_SUCCESS;
}

static uint16_t nvme_io_cmd(FemuCtrl *n, NvmeCmd *cmd, NvmeRequest *req)
{
    NvmeNamespace *ns;
    uint32_t nsid = le32_to_cpu(cmd->nsid);

    if (nsid == 0 || nsid > n->num_namespaces) {
        femu_err("%s, NVME_INVALID_NSID %" PRIu32 "\n", __func__, nsid);
        return NVME_INVALID_NSID | NVME_DNR;
    }

    req->ns = ns = &n->namespaces[nsid - 1];

    switch (cmd->opcode) {
    case NVME_CMD_FLUSH:
        if (!n->id_ctrl.vwc || !n->features.volatile_wc) {
            return NVME_SUCCESS;
        }
        return nvme_flush(n, ns, cmd, req);
    case NVME_CMD_DSM:
        if (NVME_ONCS_DSM & n->oncs) {
            return nvme_dsm(n, ns, cmd, req);
        }
        return NVME_INVALID_OPCODE | NVME_DNR;
    case NVME_CMD_COMPARE:
        if (NVME_ONCS_COMPARE & n->oncs) {
            return nvme_compare(n, ns, cmd, req);
        }
        return NVME_INVALID_OPCODE | NVME_DNR;
    case NVME_CMD_WRITE_ZEROES:
        if (NVME_ONCS_WRITE_ZEROS & n->oncs) {
            return nvme_write_zeros(n, ns, cmd, req);
        }
        return NVME_INVALID_OPCODE | NVME_DNR;
    case NVME_CMD_WRITE_UNCOR:
        if (NVME_ONCS_WRITE_UNCORR & n->oncs) {
            return nvme_write_uncor(n, ns, cmd, req);
        }
        return NVME_INVALID_OPCODE | NVME_DNR;
    default:
        if (n->ext_ops.io_cmd) {
            return n->ext_ops.io_cmd(n, ns, cmd, req);
        }

        femu_err("%s, NVME_INVALID_OPCODE\n", __func__);
        return NVME_INVALID_OPCODE | NVME_DNR;
    }
}

void nvme_post_cqes_io(void *opaque)
{
    NvmeCQueue *cq = opaque;
    NvmeRequest *req, *next;
    int64_t cur_time, ntt = 0;
    int processed = 0;

    QTAILQ_FOREACH_SAFE(req, &cq->req_list, entry, next) {
        if (nvme_cq_full(cq)) {
            break;
        }

        cur_time = qemu_clock_get_ns(QEMU_CLOCK_REALTIME);
        if (cq->cqid != 0 && cur_time < req->expire_time) {
            ntt = req->expire_time;
            break;
        }

        nvme_post_cqe(cq, req);
        processed++;
    }

    if (ntt == 0) {
        ntt = qemu_clock_get_ns(QEMU_CLOCK_REALTIME) + CQ_POLLING_PERIOD_NS;
    }

    /* Only interrupt guest when we "do" complete some I/Os */
    if (processed > 0) {
        nvme_isr_notify_io(cq);
    }
}
