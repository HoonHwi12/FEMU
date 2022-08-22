#ifndef __FEMU_ZNS_H
#define __FEMU_ZNS_H

#include "../nvme.h"

extern double d_rd_lat_s;
extern double d_wr_lat_s;
extern double d_er_lat_s;

typedef struct QEMU_PACKED NvmeZonedResult {
    uint64_t slba;
} NvmeZonedResult;

typedef struct NvmeIdCtrlZoned {
    uint8_t     zasl;
    uint8_t     rsvd1[4095];
} NvmeIdCtrlZoned;

enum NvmeZoneAttr {
    NVME_ZA_FINISHED_BY_CTLR         = 1 << 0,
    NVME_ZA_FINISH_RECOMMENDED       = 1 << 1,
    NVME_ZA_RESET_RECOMMENDED        = 1 << 2,
    NVME_ZA_ZD_EXT_VALID             = 1 << 7,
};

typedef struct QEMU_PACKED NvmeZoneReportHeader {
    uint64_t    nr_zones;
    uint8_t     rsvd[56];
} NvmeZoneReportHeader;

enum NvmeZoneReceiveAction {
    NVME_ZONE_REPORT                 = 0,
    NVME_ZONE_REPORT_EXTENDED        = 1,
};

enum NvmeZoneReportType {
    NVME_ZONE_REPORT_ALL             = 0,
    NVME_ZONE_REPORT_EMPTY           = 1,
    NVME_ZONE_REPORT_IMPLICITLY_OPEN = 2,
    NVME_ZONE_REPORT_EXPLICITLY_OPEN = 3,
    NVME_ZONE_REPORT_CLOSED          = 4,
    NVME_ZONE_REPORT_FULL            = 5,
    NVME_ZONE_REPORT_READ_ONLY       = 6,
    NVME_ZONE_REPORT_OFFLINE         = 7,
};

enum NvmeZoneType {
    NVME_ZONE_TYPE_RESERVED          = 0x00,
    NVME_ZONE_TYPE_CONVENTIONAL      = 0X01,
    NVME_ZONE_TYPE_SEQ_WRITE         = 0x02,
};

enum NvmeZoneSendAction {
    NVME_ZONE_ACTION_RSD             = 0x00,
    NVME_ZONE_ACTION_CLOSE           = 0x01,
    NVME_ZONE_ACTION_FINISH          = 0x02,
    NVME_ZONE_ACTION_OPEN            = 0x03,
    NVME_ZONE_ACTION_RESET           = 0x04,
    NVME_ZONE_ACTION_OFFLINE         = 0x05,
    NVME_ZONE_ACTION_SET_ZD_EXT      = 0x10,
};

//* by HH
extern struct slc_region rslc;
extern struct w_pointer wpzone;
extern struct line_mgmt slm;

extern pthread_mutex_t lock_nr_open;
extern pthread_mutex_t lock_nr_active;
extern pthread_mutex_t lock_slc_wp;
extern pthread_mutex_t lock_nand_wp;

extern uint64_t        slc_wp;

typedef struct slc_mapping {
    uint64_t zdslba;
    uint64_t zdnlb;

    // uint32_t zdline;
    // uint32_t zdch;
    // uint32_t zdlun;
    // uint32_t zdpg;

    uint64_t target_addr;
    bool isvalid;
} slc_mapping;

typedef struct slctbl {
        // struct {    
        //     //uint16_t zdnum;
        //     uint64_t zdslba;
        //     uint16_t zdnlb;
        // } g;
        slc_mapping *slcmap;
        u_int64_t num_slc_data;
        //uint64_t slctbl;   
} slctbl;

typedef struct slc_region {
    slctbl *mapslc; /* slc mapping table */
}slc_region;

// inline struct slctbl get_mapslc_ent(u_int64_t slc_index)
// {
//     return rslc.mapslc[slc_index];
// }

#include "../bbssd/ftl.h"
inline void set_mapslc_ent(struct ssd *ssd, uint16_t zone_index, uint64_t zdslba, uint32_t zdnlb, uint64_t target_addr)
{
    slctbl *tbl = rslc.mapslc;
    //* HH: nand mapping
    //struct ppa ppa = get_new_page(ssd);

    tbl += zone_index;
    slc_mapping *map_tbl = tbl->slcmap;
    
    if(tbl->num_slc_data == 0)
    {
        map_tbl += tbl->num_slc_data;

        map_tbl->zdslba = zdslba;
        map_tbl->zdnlb = zdnlb;  
        map_tbl->target_addr = target_addr;
        map_tbl->isvalid = true;
        // map_tbl->zdline = ppa.g.blk;
        // map_tbl->zdch = ppa.g.ch;
        // map_tbl->zdlun = ppa.g.lun;
        // map_tbl->zdpg = ppa.g.pg;
        
        tbl->num_slc_data++;
        h_log_tbl("First data! zone:%d, num_tbl: %ld\n", zone_index, tbl->num_slc_data);
        //h_log_writecmd("map_tbl[%ld] slba: 0x%lx, nlb: 0x%x, target: 0x%lx, valid: %d\n",
        //    tbl->num_slc_data-1, map_tbl->zdslba, map_tbl->zdnlb, map_tbl->target_addr, map_tbl->isvalid);
    }
    else
    {
        map_tbl += tbl->num_slc_data - 1;
        if( (map_tbl->zdslba + map_tbl->zdnlb + 1) == zdslba)
        {
            h_log_tbl("attached tbl! zone:%d, num_tbl: %ld, tbl_slba: 0x%lx, tbl_nlb: 0x%lx, cmd_slba: 0x%lx\n",
                zone_index, tbl->num_slc_data, map_tbl->zdslba, map_tbl->zdnlb, zdslba);            
            map_tbl->zdnlb += zdnlb + 1;
            //h_log_writecmd("tbl[%d] num data: %ld\n", zone_index, tbl->num_slc_data);
            //h_log_writecmd("map_tbl[%ld] slba: 0x%lx, nlb: 0x%x, map.target: 0x%lx, this.target_addr: 0x%lx\n",
            //    tbl->num_slc_data-1, map_tbl->zdslba, map_tbl->zdnlb, map_tbl->target_addr, target_addr);            
        }
        else
        {
            map_tbl = tbl->slcmap;
            map_tbl += tbl->num_slc_data;

            map_tbl->zdslba = zdslba;
            map_tbl->zdnlb = zdnlb;  
            map_tbl->target_addr = target_addr;
            map_tbl->isvalid = true;

            h_log_tbl("new tbl! slc_wp:0x%lx zone:%d, num_tbl: %ld, tbl_slba: 0x%lx, tbl_nlb: 0x%lx, cmd_slba: 0x%lx\n",
                slc_wp, zone_index, tbl->num_slc_data, map_tbl->zdslba, map_tbl->zdnlb, zdslba);
            
            // map_tbl->zdline = ppa.g.blk;
            // map_tbl->zdch = ppa.g.ch;
            // map_tbl->zdlun = ppa.g.lun;
            // map_tbl->zdpg = ppa.g.pg;       

            tbl->num_slc_data++;
            //h_log_writecmd("tbl[%d] num data: %ld\n", zone_index, tbl->num_slc_data);
            //h_log_writecmd("map_tbl[%ld] slba: 0x%lx, nlb: 0x%x, target: 0x%lx, valid: %d\n",
            //    tbl->num_slc_data-1, map_tbl->zdslba, map_tbl->zdnlb, map_tbl->target_addr, map_tbl->isvalid);            
        }
    }
}


typedef struct QEMU_PACKED NvmeZoneDescr {
    uint8_t     zt;
    uint8_t     zs;
    uint8_t     za;
    uint8_t     rsvd3[4];

    //by HH
    uint8_t     zone_flash_type;
    ///////////////////////

    uint64_t    zcap;
    uint64_t    zslba;
    uint64_t    wp;
    //uint8_t     rsvd32[32];

    //* by HH: dummy
    uint64_t rd_lat_ns;
    uint64_t wr_lat_ns;
    uint64_t er_lat_ns;
    uint64_t chnl_pg_xfer_lat_ns;
    ///////////////////////
} NvmeZoneDescr;

typedef enum NvmeZoneState {
    NVME_ZONE_STATE_RESERVED         = 0x00,
    NVME_ZONE_STATE_EMPTY            = 0x01,
    NVME_ZONE_STATE_IMPLICITLY_OPEN  = 0x02,
    NVME_ZONE_STATE_EXPLICITLY_OPEN  = 0x03,
    NVME_ZONE_STATE_CLOSED           = 0x04,
    NVME_ZONE_STATE_READ_ONLY        = 0x0D,
    NVME_ZONE_STATE_FULL             = 0x0E,
    NVME_ZONE_STATE_OFFLINE          = 0x0F,
} NvmeZoneState;

#define NVME_SET_CSI(vec, csi) (vec |= (uint8_t)(1 << (csi)))

typedef struct QEMU_PACKED NvmeLBAFE {
    uint64_t    zsze;
    uint8_t     zdes;
    uint8_t     rsvd9[7];
} NvmeLBAFE;

typedef struct QEMU_PACKED NvmeIdNsZoned {
    uint16_t    zoc;
    uint16_t    ozcs;
    uint32_t    mar;
    uint32_t    mor;
    uint32_t    rrl;
    uint32_t    frl;
    uint8_t     rsvd20[2796];
    NvmeLBAFE   lbafe[16];
    uint8_t     rsvd3072[768];
    uint8_t     vs[256];
} NvmeIdNsZoned;

typedef struct NvmeZone {
    NvmeZoneDescr   d;
    uint64_t        w_ptr;
    QTAILQ_ENTRY(NvmeZone) entry;
} NvmeZone;

typedef struct NvmeNamespaceParams {
    uint32_t nsid;
    QemuUUID uuid;

    bool     zoned;
    bool     cross_zone_read;
    uint64_t zone_size_bs;
    uint64_t zone_cap_bs;
    uint32_t max_active_zones;
    uint32_t max_open_zones;
    uint32_t zd_extension_size;
} NvmeNamespaceParams;

static inline uint32_t zns_nsid(NvmeNamespace *ns)
{
    if (ns) {
        return ns->id;
    }

    return -1;
}

static inline NvmeLBAF *zns_ns_lbaf(NvmeNamespace *ns)
{
    NvmeIdNs *id_ns = &ns->id_ns;
    return &id_ns->lbaf[NVME_ID_NS_FLBAS_INDEX(id_ns->flbas)];
}

static inline uint8_t zns_ns_lbads(NvmeNamespace *ns)
{
    /* NvmeLBAF */
    return zns_ns_lbaf(ns)->lbads;
}

/* calculate the number of LBAs that the namespace can accomodate */
static inline uint64_t zns_ns_nlbas(NvmeNamespace *ns)
{
    return ns->size >> zns_ns_lbads(ns);
}

/* convert an LBA to the equivalent in bytes */
static inline size_t zns_l2b(NvmeNamespace *ns, uint64_t lba)
{
    return lba << zns_ns_lbads(ns);
}

static inline NvmeZoneState zns_get_zone_state(NvmeZone *zone)
{
    return zone->d.zs >> 4;
}

static inline void zns_set_zone_state(NvmeZone *zone, NvmeZoneState state)
{
    zone->d.zs = state << 4;
}

static inline uint64_t zns_zone_rd_boundary(NvmeNamespace *ns, NvmeZone *zone)
{
    return zone->d.zslba + ns->ctrl->zone_size;
}

static inline uint64_t zns_zone_wr_boundary(NvmeZone *zone)
{
    return zone->d.zslba + zone->d.zcap;
}

static inline bool zns_wp_is_valid(NvmeZone *zone)
{
    uint8_t st = zns_get_zone_state(zone);

    return st != NVME_ZONE_STATE_FULL &&
           st != NVME_ZONE_STATE_READ_ONLY &&
           st != NVME_ZONE_STATE_OFFLINE &&
           st != NVME_ZONE_STATE_RESERVED;
}

static inline uint8_t *zns_get_zd_extension(NvmeNamespace *ns, uint32_t zone_idx)
{
    return &ns->ctrl->zd_extensions[zone_idx * ns->ctrl->zd_extension_size];
}

static inline void zns_aor_inc_open(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;

    //* by HH: disable asser for lock debugging
    //assert(n->nr_open_zones >= 0);

    if (n->max_open_zones) {
        // while (n->nr_open_zones > n->max_open_zones)
        // {
        //     printf("Error! nr_open_zones: %d, max_open_zones: %d, usleep(1000)\n", n->nr_open_zones, n->max_open_zones);
        //     usleep(1000);
        // }
        
        pthread_mutex_lock(&lock_nr_open);
        n->nr_open_zones++;
        h_log_zone("nr_open++(%d)\n", n->nr_open_zones);
        pthread_mutex_unlock(&lock_nr_open);
        //printf("nr_open++(%d)\n", ns->ctrl->nr_open_zones);

        //* by HH: decline
        // if(n->nr_open_zones > n->max_open_zones)
        // {
        //     printf("warning! nr_open_zones: %d, max_open_zones: %d, decline open zones\n", n->nr_open_zones, n->max_open_zones);
        //     n->nr_open_zones--;
        //     //sleep(10000);
        // }
        assert(n->nr_open_zones <= n->max_open_zones);
    }
}

static inline void zns_aor_dec_open_debug(NvmeNamespace *ns, int debug_root)
{
    FemuCtrl *n = ns->ctrl;
    if (n->max_open_zones) {
        //*by HH: wait
        // while (n->nr_open_zones <= 0)
        // {
        //     printf("Error! n->nr_open_zones=%d...root function: %d, usleep(1000)\n", n->nr_open_zones, debug_root);
        //     usleep(1000);
        // }
        if(n->nr_open_zones <= 0)
        {
            int temp_nr_open=0;
            NvmeZone *zone = n->zone_array;
            for(int i = 0; i < n->num_zones; i++, zone++)
            {
                switch (zns_get_zone_state(zone)) {
                case NVME_ZONE_STATE_EXPLICITLY_OPEN:
                    temp_nr_open++;
                case NVME_ZONE_STATE_IMPLICITLY_OPEN:
                    temp_nr_open++;
                default:
                    ;
                }
            }
            printf("DEBUG! n->nr_open_zones=%d...root function: %d, temp_nr_open:%d\n",
                n->nr_open_zones, debug_root, temp_nr_open);

            //sleep(10000);
        }

        //assert(n->nr_open_zones > 0);

        n->nr_open_zones--;
        h_log_zone("nr_open--(%d)\n", n->nr_open_zones);
    }

    //* by HH disable for lock test
    //assert(n->nr_open_zones >= 0);
}

static inline void zns_aor_dec_open(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    if (n->max_open_zones) {
        if(n->nr_open_zones <= 0)
        {
            printf("DEBUG! n->nr_open_zones=%d\n", n->nr_open_zones);
            sleep(10000);
        }

        assert(n->nr_open_zones > 0);

        pthread_mutex_lock(&lock_nr_open);
        n->nr_open_zones--;
        h_log_zone("nr_open--(%d)\n", n->nr_open_zones);
        pthread_mutex_unlock(&lock_nr_open);
    }
    assert(n->nr_open_zones >= 0);
}

static inline void zns_aor_inc_active(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    assert(n->nr_active_zones >= 0);
    if (n->max_active_zones) {
        //pthread_mutex_lock(&lock_nr_active);
        n->nr_active_zones++;
        h_log_zone("nr_active++(%d)\n", n->nr_active_zones);
        pthread_mutex_unlock(&lock_nr_active);

        if(n->nr_active_zones > n->max_active_zones)
        {
            printf("ERROR! n->nr_zctive:%d, n->max_active:%d\n",
                n->nr_active_zones, n->max_active_zones);
        }
        assert(n->nr_active_zones <= n->max_active_zones);
    }
}

static inline void zns_aor_dec_active(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    if (n->max_active_zones) {
        assert(n->nr_active_zones > 0);
        //pthread_mutex_lock(&lock_nr_active);
        n->nr_active_zones--;
        pthread_mutex_unlock(&lock_nr_active);
        h_log_zone("nr_active--(%d)\n", n->nr_active_zones);

        //* HH: wait
        // while (n->nr_active_zones < n->nr_open_zones)
        // {
        //     printf("Error! nr_active_zones: %d, nr_open_zones: %d, usleep(1000)\n", n->nr_active_zones, n->nr_open_zones);
        //     usleep(1000);
        // }
        if(n->nr_active_zones < n->nr_open_zones)
        {
            printf("DEBUG! nr_active_zones: %d, nr_open_zones: %d\n", n->nr_active_zones, n->nr_open_zones);
            sleep(10000);
        } 
        //*

        assert(n->nr_active_zones >= n->nr_open_zones);
    }
    assert(n->nr_active_zones >= 0);
}

void zns_ns_shutdown(NvmeNamespace *ns);
void zns_ns_cleanup(NvmeNamespace *ns);

#endif
