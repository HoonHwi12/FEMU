#include "../nvme.h"
#include "./ftl.h"

//by HH //////////////////////////////////////
#include "../zns/zns.h"

#define MIN_DISCARD_GRANULARITY     (4 * KiB)
#define NVME_DEFAULT_ZONE_SIZE      (128 * MiB)
#define NVME_DEFAULT_MAX_AZ_SIZE    (128 * KiB)
///////////////////////////////////////////////

// by HH ////////////////////////////////////////////////////////////////////////
static inline uint32_t zns_zone_idx(NvmeNamespace *ns, uint64_t slba)
{
    FemuCtrl *n = ns->ctrl;

    return (n->zone_size_log2 > 0 ? slba >> n->zone_size_log2 : slba /
            n->zone_size);
}

static int zns_init_zone_geometry(NvmeNamespace *ns, Error **errp)
{    
    FemuCtrl *n = ns->ctrl;
    uint64_t zone_size, zone_cap;
    uint32_t lbasz = 1 << zns_ns_lbads(ns);

    if (n->zone_size_bs) {
        zone_size = n->zone_size_bs;
    } else {
        zone_size = NVME_DEFAULT_ZONE_SIZE;
    }

    if (n->zone_cap_bs) {
        zone_cap = n->zone_cap_bs;
    } else {
        zone_cap = zone_size;
    }
    
    if (zone_cap > zone_size) {
        femu_err("zone capacity %luB > zone size %luB", zone_cap, zone_size);
        return -1;
    }
    if (zone_size < lbasz) {
        femu_err("zone size %luB too small, must >= %uB", zone_size, lbasz);
        return -1;
    }
    if (zone_cap < lbasz) {
        femu_err("zone capacity %luB too small, must >= %uB", zone_cap, lbasz);
        return -1;
    }

    n->zone_size = zone_size / lbasz;
    n->zone_capacity = zone_cap / lbasz;
    n->num_zones = ns->size / lbasz / n->zone_size;

    if (n->max_open_zones > n->num_zones) {
        femu_err("max_open_zones value %u exceeds the number of zones %u",
                 n->max_open_zones, n->num_zones);
        return -1;
    }
    if (n->max_active_zones > n->num_zones) {
        femu_err("max_active_zones value %u exceeds the number of zones %u",
                 n->max_active_zones, n->num_zones);
        return -1;
    }

    if (n->zd_extension_size) {
        if (n->zd_extension_size & 0x3f) {
            femu_err("zone descriptor extension size must be multiples of 64B");
            return -1;
        }
        if ((n->zd_extension_size >> 6) > 0xff) {
            femu_err("zone descriptor extension size is too large");
            return -1;
        }
    }

    return 0;
}

static void zns_init_zoned_state(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    uint64_t start = 0, zone_size = n->zone_size;
    uint64_t capacity = n->num_zones * zone_size;
    NvmeZone *zone;
    int i;

    n->zone_array = g_new0(NvmeZone, n->num_zones);
    if (n->zd_extension_size) {
        n->zd_extensions = g_malloc0(n->zd_extension_size * n->num_zones);
    }

    QTAILQ_INIT(&n->exp_open_zones);
    QTAILQ_INIT(&n->imp_open_zones);
    QTAILQ_INIT(&n->closed_zones);
    QTAILQ_INIT(&n->full_zones);

    zone = n->zone_array;

    NUM_SLC_BLK = 0;
    TLC_START_LBA = 0;

    for (i = 0; i < n->num_zones; i++, zone++) {
        if (start + zone_size > capacity) {
            zone_size = capacity - start;
        }
        zns_set_zone_state(zone, NVME_ZONE_STATE_EMPTY);
        zone->d.za = 0;
        zone->d.zcap = n->zone_capacity;
        zone->d.zslba = start;
        zone->d.wp = start;
        zone->d.zone_flash_type = n->flash_type;

        zone->d.zt = NVME_ZONE_TYPE_SEQ_WRITE;
        // by HH ---------------------------------------------------
        if(i < n->num_zones-1)
        {
            //zone->d.zt = NVME_ZONE_TYPE_SEQ_WRITE;
            
            //zone->d.zone_flash_type = SLC;
            //TLC_START_LBA = zone->d.zslba + zone->d.zcap;
            //NUM_SLC_BLK += zone->d.zcap;

            // n->namespaces->id_ns.nsze = cpu_to_le64(n->namespaces->id_ns.nsze - n->zone_size);
            // n->namespaces->id_ns.ncap = ns->id_ns.nsze;
            // n->namespaces->id_ns.nuse = ns->id_ns.ncap;            
        }
        else
        {
           //zone->d.zt = NVME_ZONE_TYPE_CONVENTIONAL; 
        }
        // ----------------------------------------------------------

        zone->w_ptr = start;
        start += zone_size;
    }

    n->zone_size_log2 = 0;
    if (is_power_of_2(n->zone_size)) {
        n->zone_size_log2 = 63 - clz64(n->zone_size);
    }
}

static int zns_init_zone_cap(FemuCtrl *n)
{
    n->zoned = true;
    n->zasl_bs = NVME_DEFAULT_MAX_AZ_SIZE;
    n->zone_size_bs = NVME_DEFAULT_ZONE_SIZE;
    n->zone_cap_bs = 0;
    n->cross_zone_read = false;
    n->max_active_zones = 0;
    n->max_open_zones = 0;
    n->zd_extension_size = 0;

    return 0;
}

static void zns_init_zone_identify(FemuCtrl *n, NvmeNamespace *ns, int lba_index)
{
    NvmeIdNsZoned *id_ns_z;
    h_log("zns init tbl\n");
    rslc.mapslc = g_new0(slctbl, n->num_zones);
    wpzone.wpnand = g_new0(write_pointer, n->num_zones);

    slctbl *tbl = rslc.mapslc;
    
    for(int temp=0; temp < n->num_zones; temp++)
    {
        tbl->slcmap = g_new0(slc_mapping, n->zone_capacity);
        tbl++;
    }
    slc_wp = 0;

    h_log("zns init tbl end\n");
    zns_init_zoned_state(ns);
    id_ns_z = g_malloc0(sizeof(NvmeIdNsZoned));

    /* MAR/MOR are zeroes-based, 0xffffffff means no limit */
    id_ns_z->mar = cpu_to_le32(n->max_active_zones - 1);
    id_ns_z->mor = cpu_to_le32(n->max_open_zones - 1);
    id_ns_z->zoc = 0;
    id_ns_z->ozcs = n->cross_zone_read ? 0x01 : 0x00;

    id_ns_z->lbafe[lba_index].zsze = cpu_to_le64(n->zone_size);
    id_ns_z->lbafe[lba_index].zdes = n->zd_extension_size >> 6; /* Units of 64B */

    n->csi = NVME_CSI_ZONED;
    ns->id_ns.nsze = cpu_to_le64(n->num_zones * n->zone_size);
    ns->id_ns.ncap = ns->id_ns.nsze;
    ns->id_ns.nuse = ns->id_ns.ncap;

    /* NvmeIdNs */
    /*
     * The device uses the BDRV_BLOCK_ZERO flag to determine the "deallocated"
     * status of logical blocks. Since the spec defines that logical blocks
     * SHALL be deallocated when then zone is in the Empty or Offline states,
     * we can only support DULBE if the zone size is a multiple of the
     * calculated NPDG.
     */

    if (n->zone_size % (ns->id_ns.npdg + 1)) {
        femu_err("the zone size (%"PRIu64" blocks) is not a multiple of the"
                 "calculated deallocation granularity (%"PRIu16" blocks); DULBE"
                 "support disabled", n->zone_size, ns->id_ns.npdg + 1);
        ns->id_ns.nsfeat &= ~0x4;
    }

    n->id_ns_zoned = id_ns_z;
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

static void zns_aio_zone_reset_cb(NvmeRequest *req, NvmeZone *zone)
{
    NvmeNamespace *ns = req->ns;

    /* FIXME, We always assume reset SUCCESS */
    switch (zns_get_zone_state(zone)) {
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        zns_aor_dec_open(ns);
    case NVME_ZONE_STATE_CLOSED:
        zns_aor_dec_active(ns);
    case NVME_ZONE_STATE_FULL:
        zone->w_ptr = zone->d.zslba;
        zone->d.wp = zone->w_ptr;
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_EMPTY);
    default:
        break;
    }
}

typedef uint16_t (*op_handler_t)(NvmeNamespace *, NvmeZone *, NvmeZoneState,
                                 NvmeRequest *);

enum NvmeZoneProcessingMask {
    NVME_PROC_CURRENT_ZONE    = 0,
    NVME_PROC_OPENED_ZONES    = 1 << 0,
    NVME_PROC_CLOSED_ZONES    = 1 << 1,
    NVME_PROC_READ_ONLY_ZONES = 1 << 2,
    NVME_PROC_FULL_ZONES      = 1 << 3,
};

static uint16_t zns_get_mgmt_zone_slba_idx(FemuCtrl *n, NvmeCmd *c,
                                           uint64_t *slba, uint32_t *zone_idx)
{
    NvmeNamespace *ns = &n->namespaces[0];
    uint32_t dw10 = le32_to_cpu(c->cdw10);
    uint32_t dw11 = le32_to_cpu(c->cdw11);

    if (!n->zoned) {
        return NVME_INVALID_OPCODE | NVME_DNR;
    }

    *slba = ((uint64_t)dw11) << 32 | dw10;
    if (unlikely(*slba >= ns->id_ns.nsze)) {
        *slba = 0;
        return NVME_LBA_RANGE | NVME_DNR;
    }

    *zone_idx = zns_zone_idx(ns, *slba);

    printf("slba 0x%lx, zone idx: %d, n->num_zones %d\n", *slba, *zone_idx, n->num_zones);
    assert(*zone_idx < n->num_zones);

    return NVME_SUCCESS;
}

static uint16_t zns_bulk_proc_zone(NvmeNamespace *ns, NvmeZone *zone,
                                   enum NvmeZoneProcessingMask proc_mask,
                                   op_handler_t op_hndlr, NvmeRequest *req)
{
    uint16_t status = NVME_SUCCESS;
    NvmeZoneState zs = zns_get_zone_state(zone);
    bool proc_zone;

    switch (zs) {
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        proc_zone = proc_mask & NVME_PROC_OPENED_ZONES;
        break;
    case NVME_ZONE_STATE_CLOSED:
        proc_zone = proc_mask & NVME_PROC_CLOSED_ZONES;
        break;
    case NVME_ZONE_STATE_READ_ONLY:
        proc_zone = proc_mask & NVME_PROC_READ_ONLY_ZONES;
        break;
    case NVME_ZONE_STATE_FULL:
        proc_zone = proc_mask & NVME_PROC_FULL_ZONES;
        break;
    default:
        proc_zone = false;
    }

    if (proc_zone) {
        status = op_hndlr(ns, zone, zs, req);
    }

    return status;
}

static uint16_t zns_do_zone_op(NvmeNamespace *ns, NvmeZone *zone,
                               enum NvmeZoneProcessingMask proc_mask,
                               op_handler_t op_hndlr, NvmeRequest *req)
{
    FemuCtrl *n = ns->ctrl;
    NvmeZone *next;
    uint16_t status = NVME_SUCCESS;
    int i;

    if (!proc_mask) {
        status = op_hndlr(ns, zone, zns_get_zone_state(zone), req);
    } else {
        if (proc_mask & NVME_PROC_CLOSED_ZONES) {
            QTAILQ_FOREACH_SAFE(zone, &n->closed_zones, entry, next) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }
        }
        if (proc_mask & NVME_PROC_OPENED_ZONES) {
            QTAILQ_FOREACH_SAFE(zone, &n->imp_open_zones, entry, next) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }

            QTAILQ_FOREACH_SAFE(zone, &n->exp_open_zones, entry, next) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }
        }
        if (proc_mask & NVME_PROC_FULL_ZONES) {
            QTAILQ_FOREACH_SAFE(zone, &n->full_zones, entry, next) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }
        }

        if (proc_mask & NVME_PROC_READ_ONLY_ZONES) {
            for (i = 0; i < n->num_zones; i++, zone++) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }
        }
    }

out:
    return status;
}

static int zns_aor_check(NvmeNamespace *ns, uint32_t act, uint32_t opn)
{
    FemuCtrl *n = ns->ctrl;
    if (n->max_active_zones != 0 &&
        n->nr_active_zones + act > n->max_active_zones) {
        return NVME_ZONE_TOO_MANY_ACTIVE | NVME_DNR;
    }
    if (n->max_open_zones != 0 &&
        n->nr_open_zones + opn > n->max_open_zones) {
        return NVME_ZONE_TOO_MANY_OPEN | NVME_DNR;
    }

    return NVME_SUCCESS;
}


static uint16_t zns_open_zone(NvmeNamespace *ns, NvmeZone *zone,
                              NvmeZoneState state, NvmeRequest *req)
{
    uint16_t status;

    switch (state) {
    case NVME_ZONE_STATE_EMPTY:
        status = zns_aor_check(ns, 1, 0);
        if (status != NVME_SUCCESS) {
            return status;
        }
        zns_aor_inc_active(ns);
    case NVME_ZONE_STATE_CLOSED:
        status = zns_aor_check(ns, 0, 1);
        if (status != NVME_SUCCESS) {
            if (state == NVME_ZONE_STATE_EMPTY) {
                zns_aor_dec_active(ns);
            }
            return status;
        }
        zns_aor_inc_open(ns);
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_EXPLICITLY_OPEN);
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        return NVME_SUCCESS;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }
}

static uint16_t zns_close_zone(NvmeNamespace *ns, NvmeZone *zone,
                               NvmeZoneState state, NvmeRequest *req)
{
    switch (state) {
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        zns_aor_dec_open(ns);
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_CLOSED);
    case NVME_ZONE_STATE_CLOSED:
        return NVME_SUCCESS;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }
}

static uint16_t zns_finish_zone(NvmeNamespace *ns, NvmeZone *zone,
                                NvmeZoneState state, NvmeRequest *req)
{
    switch (state) {
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        zns_aor_dec_open(ns);
    case NVME_ZONE_STATE_CLOSED:
        zns_aor_dec_active(ns);
    case NVME_ZONE_STATE_EMPTY:
        zone->w_ptr = zns_zone_wr_boundary(zone);
        zone->d.wp = zone->w_ptr;
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_FULL);
    case NVME_ZONE_STATE_FULL:
        return NVME_SUCCESS;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }
}

static uint16_t zns_reset_zone(NvmeNamespace *ns, NvmeZone *zone,
                               NvmeZoneState state, NvmeRequest *req)
{
    switch (state) {
    case NVME_ZONE_STATE_EMPTY:
        return NVME_SUCCESS;
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
    case NVME_ZONE_STATE_CLOSED:
    case NVME_ZONE_STATE_FULL:
        break;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }

    zns_aio_zone_reset_cb(req, zone);

    return NVME_SUCCESS;
}

static uint16_t zns_offline_zone(NvmeNamespace *ns, NvmeZone *zone,
                                 NvmeZoneState state, NvmeRequest *req)
{
    switch (state) {
    case NVME_ZONE_STATE_READ_ONLY:
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_OFFLINE);
    case NVME_ZONE_STATE_OFFLINE:
        return NVME_SUCCESS;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }
}

static uint16_t zns_set_zd_ext(NvmeNamespace *ns, NvmeZone *zone)
{
    uint16_t status;
    uint8_t state = zns_get_zone_state(zone);

    if (state == NVME_ZONE_STATE_EMPTY) {
        status = zns_aor_check(ns, 1, 0);
        if (status != NVME_SUCCESS) {
            return status;
        }
        zns_aor_inc_active(ns);
        zone->d.za |= NVME_ZA_ZD_EXT_VALID;

        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_CLOSED);
        return NVME_SUCCESS;
    }

    return NVME_ZONE_INVAL_TRANSITION;
}

static uint16_t zns_zone_mgmt_send(FemuCtrl *n, NvmeRequest *req)
{
    NvmeCmd *cmd = (NvmeCmd *)&req->cmd;
    NvmeNamespace *ns = req->ns;
    uint64_t prp1 = le64_to_cpu(cmd->dptr.prp1);
    uint64_t prp2 = le64_to_cpu(cmd->dptr.prp2);
    NvmeZone *zone;
    uintptr_t *resets;
    uint8_t *zd_ext;
    uint32_t dw13 = le32_to_cpu(cmd->cdw13);
    uint64_t slba = 0;
    uint32_t zone_idx = 0;
    uint16_t status;
    uint8_t action;
    bool all;
    enum NvmeZoneProcessingMask proc_mask = NVME_PROC_CURRENT_ZONE;

    action = dw13 & 0xff;
    all = dw13 & 0x100;

    req->status = NVME_SUCCESS;

    if (!all) {
        status = zns_get_mgmt_zone_slba_idx(n, cmd, &slba, &zone_idx);
        if (status) {
            return status;
        }
    }

    zone = &n->zone_array[zone_idx];
    if (slba != zone->d.zslba) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    switch (action) {
    case NVME_ZONE_ACTION_OPEN:
        if (all) {
            proc_mask = NVME_PROC_CLOSED_ZONES;
        }
        status = zns_do_zone_op(ns, zone, proc_mask, zns_open_zone, req);
        break;
    case NVME_ZONE_ACTION_CLOSE:
        if (all) {
            proc_mask = NVME_PROC_OPENED_ZONES;
        }
        status = zns_do_zone_op(ns, zone, proc_mask, zns_close_zone, req);
        break;
    case NVME_ZONE_ACTION_FINISH:
        if (all) {
            proc_mask = NVME_PROC_OPENED_ZONES | NVME_PROC_CLOSED_ZONES;
        }
        status = zns_do_zone_op(ns, zone, proc_mask, zns_finish_zone, req);
        break;
    case NVME_ZONE_ACTION_RESET:
        resets = (uintptr_t *)&req->opaque;

        if (all) {
            proc_mask = NVME_PROC_OPENED_ZONES | NVME_PROC_CLOSED_ZONES |
                NVME_PROC_FULL_ZONES;
        }
        *resets = 1;
        status = zns_do_zone_op(ns, zone, proc_mask, zns_reset_zone, req);
        (*resets)--;
        return NVME_SUCCESS;
    case NVME_ZONE_ACTION_OFFLINE:
        if (all) {
            proc_mask = NVME_PROC_READ_ONLY_ZONES;
        }
        status = zns_do_zone_op(ns, zone, proc_mask, zns_offline_zone, req);
        break;
    case NVME_ZONE_ACTION_SET_ZD_EXT:
        if (all || !n->zd_extension_size) {
            return NVME_INVALID_FIELD | NVME_DNR;
        }
        zd_ext = zns_get_zd_extension(ns, zone_idx);
        status = dma_write_prp(n, (uint8_t *)zd_ext, n->zd_extension_size, prp1,
                               prp2);
        if (status) {
            return status;
        }
        status = zns_set_zd_ext(ns, zone);
        if (status == NVME_SUCCESS) {
            return status;
        }
        break;
    default:
        status = NVME_INVALID_FIELD;
    }

    if (status) {
        status |= NVME_DNR;
    }

    return status;
}

static bool zns_zone_matches_filter(uint32_t zafs, NvmeZone *zl)
{
    NvmeZoneState zs = zns_get_zone_state(zl);

    switch (zafs) {
    case NVME_ZONE_REPORT_ALL:
        return true;
    case NVME_ZONE_REPORT_EMPTY:
        return zs == NVME_ZONE_STATE_EMPTY;
    case NVME_ZONE_REPORT_IMPLICITLY_OPEN:
        return zs == NVME_ZONE_STATE_IMPLICITLY_OPEN;
    case NVME_ZONE_REPORT_EXPLICITLY_OPEN:
        return zs == NVME_ZONE_STATE_EXPLICITLY_OPEN;
    case NVME_ZONE_REPORT_CLOSED:
        return zs == NVME_ZONE_STATE_CLOSED;
    case NVME_ZONE_REPORT_FULL:
        return zs == NVME_ZONE_STATE_FULL;
    case NVME_ZONE_REPORT_READ_ONLY:
        return zs == NVME_ZONE_STATE_READ_ONLY;
    case NVME_ZONE_REPORT_OFFLINE:
        return zs == NVME_ZONE_STATE_OFFLINE;
    default:
        return false;
    }
}

static uint16_t zns_zone_mgmt_recv(FemuCtrl *n, NvmeRequest *req)
{
    NvmeCmd *cmd = (NvmeCmd *)&req->cmd;
    NvmeNamespace *ns = req->ns;
    uint64_t prp1 = le64_to_cpu(cmd->dptr.prp1);
    uint64_t prp2 = le64_to_cpu(cmd->dptr.prp2);
    /* cdw12 is zero-based number of dwords to return. Convert to bytes */
    uint32_t data_size = (le32_to_cpu(cmd->cdw12) + 1) << 2;
    uint32_t dw13 = le32_to_cpu(cmd->cdw13);
    uint32_t zone_idx, zra, zrasf, partial;
    uint64_t max_zones, nr_zones = 0;
    uint16_t status;
    uint64_t slba, capacity = zns_ns_nlbas(ns);
    NvmeZoneDescr *z;
    NvmeZone *zone;
    NvmeZoneReportHeader *header;
    void *buf, *buf_p;
    size_t zone_entry_sz;

    req->status = NVME_SUCCESS;

    status = zns_get_mgmt_zone_slba_idx(n, cmd, &slba, &zone_idx);
    if (status) {
        return status;
    }

    zra = dw13 & 0xff;
    if (zra != NVME_ZONE_REPORT && zra != NVME_ZONE_REPORT_EXTENDED) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if (zra == NVME_ZONE_REPORT_EXTENDED && !n->zd_extension_size) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    zrasf = (dw13 >> 8) & 0xff;
    if (zrasf > NVME_ZONE_REPORT_OFFLINE) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    if (data_size < sizeof(NvmeZoneReportHeader)) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    status = nvme_check_mdts(n, data_size);
    if (status) {
        return status;
    }

    partial = (dw13 >> 16) & 0x01;

    zone_entry_sz = sizeof(NvmeZoneDescr);
    if (zra == NVME_ZONE_REPORT_EXTENDED) {
        zone_entry_sz += n->zd_extension_size;
    }

    max_zones = (data_size - sizeof(NvmeZoneReportHeader)) / zone_entry_sz;
    buf = g_malloc0(data_size);

    zone = &n->zone_array[zone_idx];
    for (; slba < capacity; slba += n->zone_size) {
        if (partial && nr_zones >= max_zones) {
            break;
        }
        if (zns_zone_matches_filter(zrasf, zone++)) {
            nr_zones++;
        }
    }
    header = (NvmeZoneReportHeader *)buf;
    header->nr_zones = cpu_to_le64(nr_zones);

    buf_p = buf + sizeof(NvmeZoneReportHeader);
    for (; zone_idx < n->num_zones && max_zones > 0; zone_idx++) {
        zone = &n->zone_array[zone_idx];
        if (zns_zone_matches_filter(zrasf, zone)) {
            z = (NvmeZoneDescr *)buf_p;
            buf_p += sizeof(NvmeZoneDescr);

            z->zt = zone->d.zt;
            z->zs = zone->d.zs;
            z->zcap = cpu_to_le64(zone->d.zcap);
            z->zslba = cpu_to_le64(zone->d.zslba);
            z->za = zone->d.za;

            if (zns_wp_is_valid(zone)) {
                z->wp = cpu_to_le64(zone->d.wp);
            }else {
                // if(zns_get_zone_state(zone) == NVME_ZONE_STATE_FULL)
                // {
                //     z->wp = cpu_to_le64(zone->d.wp);
                // }
                // else
                {
                    z->wp = cpu_to_le64(~0ULL);
                }
            }

            if (zra == NVME_ZONE_REPORT_EXTENDED) {
                if (zone->d.za & NVME_ZA_ZD_EXT_VALID) {
                    memcpy(buf_p, zns_get_zd_extension(ns, zone_idx),
                           n->zd_extension_size);
                }
                buf_p += n->zd_extension_size;
            }

            max_zones--;
        }
    }

    status = dma_read_prp(n, (uint8_t *)buf, data_size, prp1, prp2);

    g_free(buf);

    return status;
}
/////////////////////////////////////////////////////////////////////////////



static void bb_init_ctrl_str(FemuCtrl *n)
{
    // static int fsid_vbb = 0;
    // const char *vbbssd_mn = "FEMU BlackBox-SSD Controller";
    // const char *vbbssd_sn = "vSSD";

    // nvme_set_ctrl_name(n, vbbssd_mn, vbbssd_sn, &fsid_vbb);

    uint8_t *pci_conf = n->parent_obj.config;

    static int fsid_zns = 0;
    const char *zns_mn = "FEMU ZNS-SSD Controller";
    const char *zns_sn = "vZNSSD";

    nvme_set_ctrl_name(n, zns_mn, zns_sn, &fsid_zns);

    pci_config_set_vendor_id(pci_conf, PCI_VENDOR_ID_INTEL);
    pci_config_set_device_id(pci_conf, 0x5845);
}

/* bb <=> black-box */
static void bb_init(FemuCtrl *n, Error **errp)
{
    struct ssd *ssd = n->ssd = g_malloc0(sizeof(struct ssd));

    bb_init_ctrl_str(n);

    ssd->dataplane_started_ptr = &n->dataplane_started;
    ssd->ssdname = (char *)n->devname;
    femu_debug("Starting FEMU in Blackbox-SSD mode ...\n");

    // by HH //////////////////////////////////////////////////////////
     NvmeNamespace *ns = &n->namespaces[0];

     zns_init_zone_cap(n);
     if (zns_init_zone_geometry(ns, errp) != 0) {
         return;
     }
     zns_init_zone_identify(n, ns, 0);
    /////////////////////////////////////////////////////////////////////    
    ssd_init(n);
}

static void bb_flip(FemuCtrl *n, NvmeCmd *cmd)
{
    struct ssd *ssd = n->ssd;
    int64_t cdw10 = le64_to_cpu(cmd->cdw10);

    switch (cdw10) {
    case FEMU_ENABLE_GC_DELAY:
        ssd->sp.enable_gc_delay = true;
        femu_log("%s,FEMU GC Delay Emulation [Enabled]!\n", n->devname);
        break;
    case FEMU_DISABLE_GC_DELAY:
        ssd->sp.enable_gc_delay = false;
        femu_log("%s,FEMU GC Delay Emulation [Disabled]!\n", n->devname);
        break;
    case FEMU_ENABLE_DELAY_EMU:
        // ssd->sp.pg_rd_lat = NAND_TLC_READ_LATENCY;
        // ssd->sp.pg_wr_lat = NAND_TLC_PROG_LATENCY;
        // ssd->sp.blk_er_lat = NAND_TLC_ERASE_LATENCY;
        // ssd->sp.ch_xfer_lat = 0;
        //* by HH *********************************************
        ssd->sp.pg_slc_rd_lat = NAND_TLC_READ_LATENCY * SLC_LATENCY_COEFFIENTY;
        ssd->sp.pg_slc_wr_lat = NAND_TLC_PROG_LATENCY * SLC_LATENCY_COEFFIENTY;
        ssd->sp.blk_slc_er_lat = NAND_TLC_ERASE_LATENCY * SLC_LATENCY_COEFFIENTY;
        ssd->sp.ch_slc_xfer_lat = 0;

        ssd->sp.pg_mlc_rd_lat = NAND_TLC_READ_LATENCY * MLC_LATENCY_COEFFIENTY;
        ssd->sp.pg_mlc_wr_lat = NAND_TLC_PROG_LATENCY * MLC_LATENCY_COEFFIENTY;
        ssd->sp.blk_mlc_er_lat = NAND_TLC_ERASE_LATENCY * MLC_LATENCY_COEFFIENTY;
        ssd->sp.ch_mlc_xfer_lat = 0;

        ssd->sp.pg_tlc_rd_lat = NAND_TLC_READ_LATENCY;
        ssd->sp.pg_tlc_wr_lat = NAND_TLC_PROG_LATENCY;
        ssd->sp.blk_tlc_er_lat = NAND_TLC_ERASE_LATENCY;
        ssd->sp.ch_tlc_xfer_lat = 0;

        ssd->sp.pg_qlc_rd_lat = NAND_TLC_READ_LATENCY * QLC_LATENCY_COEFFIENTY;
        ssd->sp.pg_qlc_wr_lat = NAND_TLC_PROG_LATENCY * QLC_LATENCY_COEFFIENTY;
        ssd->sp.blk_qlc_er_lat = NAND_TLC_ERASE_LATENCY * QLC_LATENCY_COEFFIENTY;
        ssd->sp.ch_qlc_xfer_lat = 0;
        // *****************************************************

        femu_log("%s,FEMU Delay Emulation [Enabled]!\n", n->devname);
        break;
    case FEMU_DISABLE_DELAY_EMU:
        // ssd->sp.pg_rd_lat = 0;
        // ssd->sp.pg_wr_lat = 0;
        // ssd->sp.blk_er_lat = 0;
        // ssd->sp.ch_xfer_lat = 0;
        //* by HH *********************************************
        ssd->sp.pg_slc_rd_lat = 0;
        ssd->sp.pg_slc_wr_lat = 0;
        ssd->sp.blk_slc_er_lat = 0;
        ssd->sp.ch_slc_xfer_lat = 0;

        ssd->sp.pg_mlc_rd_lat = 0;
        ssd->sp.pg_mlc_wr_lat = 0;
        ssd->sp.blk_mlc_er_lat = 0;
        ssd->sp.ch_mlc_xfer_lat = 0;

        ssd->sp.pg_tlc_rd_lat = 0;
        ssd->sp.pg_tlc_wr_lat = 0;
        ssd->sp.blk_tlc_er_lat = 0;
        ssd->sp.ch_tlc_xfer_lat = 0;

        ssd->sp.pg_qlc_rd_lat = 0;
        ssd->sp.pg_qlc_wr_lat = 0;
        ssd->sp.blk_qlc_er_lat = 0;
        ssd->sp.ch_qlc_xfer_lat = 0;
        // *****************************************************

        femu_log("%s,FEMU Delay Emulation [Disabled]!\n", n->devname);
        break;
    case FEMU_RESET_ACCT:
        n->nr_tt_ios = 0;
        n->nr_tt_late_ios = 0;
        femu_log("%s,Reset tt_late_ios/tt_ios,%lu/%lu\n", n->devname,
                n->nr_tt_late_ios, n->nr_tt_ios);
        break;
    case FEMU_ENABLE_LOG:
        n->print_log = true;
        femu_log("%s,Log print [Enabled]!\n", n->devname);
        break;
    case FEMU_DISABLE_LOG:
        n->print_log = false;
        femu_log("%s,Log print [Disabled]!\n", n->devname);
        break;
    default:
        printf("FEMU:%s,Not implemented flip cmd (%lu)\n", n->devname, cdw10);
    }
}

static uint16_t bb_nvme_rw(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                           NvmeRequest *req)
{
    return nvme_rw(n, ns, cmd, req, false);
}

static uint16_t bb_nvme_rw_append(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                           NvmeRequest *req)
{
    return nvme_rw(n, ns, cmd, req, true);
}

static uint16_t bb_io_cmd(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                          NvmeRequest *req)
{
    switch (cmd->opcode) {
    case NVME_CMD_READ:
    case NVME_CMD_WRITE:
        return bb_nvme_rw(n, ns, cmd, req);
    case NVME_CMD_ZONE_APPEND:
        return bb_nvme_rw_append(n, ns, cmd, req);        
    
    //by HH: added zone IO cmd
    case NVME_CMD_ZONE_MGMT_SEND:
        return zns_zone_mgmt_send(n, req);
    case NVME_CMD_ZONE_MGMT_RECV:
        return zns_zone_mgmt_recv(n, req);       
        
    default:
        printf("invalid opcode %d\n", cmd->opcode);
        return NVME_INVALID_OPCODE | NVME_DNR;
    }
}

static uint16_t bb_admin_cmd(FemuCtrl *n, NvmeCmd *cmd)
{
    switch (cmd->opcode) {
    case NVME_ADM_CMD_FEMU_FLIP:
        bb_flip(n, cmd);
        return NVME_SUCCESS;
    default:
        return NVME_INVALID_OPCODE | NVME_DNR;
    }
}

int nvme_register_bbssd(FemuCtrl *n)
{
    n->ext_ops = (FemuExtCtrlOps) {
        .state            = NULL,
        .init             = bb_init,
        .exit             = NULL,
        .rw_check_req     = NULL,
        .admin_cmd        = bb_admin_cmd,
        .io_cmd           = bb_io_cmd,
        .get_log          = NULL,
    };

    return 0;
}

