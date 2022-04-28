#include "../nvme.h"
#include "../zns/zns.h"

void set_latency(FemuCtrl *n)
{
    // by HH: Set latency ---------------------------------------------------------------------
    if (n->flash_type == TLC) {
        n->upg_rd_lat_ns = TLC_UPPER_PAGE_READ_LATENCY_NS;
        n->cpg_rd_lat_ns = TLC_CENTER_PAGE_READ_LATENCY_NS;
        n->lpg_rd_lat_ns = TLC_LOWER_PAGE_READ_LATENCY_NS;
        n->upg_wr_lat_ns = TLC_UPPER_PAGE_WRITE_LATENCY_NS;
        n->cpg_wr_lat_ns = TLC_CENTER_PAGE_WRITE_LATENCY_NS;
        n->lpg_wr_lat_ns = TLC_LOWER_PAGE_WRITE_LATENCY_NS;
        n->blk_er_lat_ns = TLC_BLOCK_ERASE_LATENCY_NS;
        n->chnl_pg_xfer_lat_ns = TLC_CHNL_PAGE_TRANSFER_LATENCY_NS;
    } else if (n->flash_type == QLC) {
        n->upg_rd_lat_ns  = QLC_UPPER_PAGE_READ_LATENCY_NS;
        n->cupg_rd_lat_ns = QLC_CENTER_UPPER_PAGE_READ_LATENCY_NS;
        n->clpg_rd_lat_ns = QLC_CENTER_LOWER_PAGE_READ_LATENCY_NS;
        n->lpg_rd_lat_ns  = QLC_LOWER_PAGE_READ_LATENCY_NS;
        n->upg_wr_lat_ns  = QLC_UPPER_PAGE_WRITE_LATENCY_NS;
        n->cupg_wr_lat_ns = QLC_CENTER_UPPER_PAGE_WRITE_LATENCY_NS;
        n->clpg_wr_lat_ns = QLC_CENTER_LOWER_PAGE_WRITE_LATENCY_NS;
        n->lpg_wr_lat_ns  = QLC_LOWER_PAGE_WRITE_LATENCY_NS;
        n->blk_er_lat_ns  = QLC_BLOCK_ERASE_LATENCY_NS;
        n->chnl_pg_xfer_lat_ns = QLC_CHNL_PAGE_TRANSFER_LATENCY_NS;
    } else if (n->flash_type == MLC) {
        n->upg_rd_lat_ns = MLC_UPPER_PAGE_READ_LATENCY_NS;
        n->lpg_rd_lat_ns = MLC_LOWER_PAGE_READ_LATENCY_NS;
        n->upg_wr_lat_ns = MLC_UPPER_PAGE_WRITE_LATENCY_NS;
        n->lpg_wr_lat_ns = MLC_LOWER_PAGE_WRITE_LATENCY_NS;
        n->blk_er_lat_ns = MLC_BLOCK_ERASE_LATENCY_NS;
        n->chnl_pg_xfer_lat_ns = MLC_CHNL_PAGE_TRANSFER_LATENCY_NS;
    }

    // by HH: set zone unit latency
    NvmeZone *zone = n->zone_array;
    for(int i=0; i < n->num_zones; i++, zone++)
    {
        if(zone->d.zone_flash_type == SLC) {
            zone->d.rd_lat_ns = SLC_RD_LATENCY_NS;
            zone->d.wr_lat_ns = SLC_WR_LATENCY_NS;
            zone->d.er_lat_ns = SLC_ER_LATENCY_NS;
            zone->d.chnl_pg_xfer_lat_ns = SLC_XFER_LATENCY_NS;
        }
        else if (zone->d.zone_flash_type == MLC) {
            zone->d.rd_lat_ns = MLC_RD_LATENCY_NS;
            zone->d.wr_lat_ns = MLC_WR_LATENCY_NS;
            zone->d.er_lat_ns = MLC_ER_LATENCY_NS;
            zone->d.chnl_pg_xfer_lat_ns = MLC_XFER_LATENCY_NS;
        } else if (zone->d.zone_flash_type == TLC) {
            zone->d.rd_lat_ns = TLC_RD_LATENCY_NS;
            zone->d.wr_lat_ns = TLC_WR_LATENCY_NS;
            zone->d.er_lat_ns = TLC_ER_LATENCY_NS;
            zone->d.chnl_pg_xfer_lat_ns = TLC_XFER_LATENCY_NS;
        } else if (zone->d.zone_flash_type == QLC) {
            zone->d.rd_lat_ns = QLC_RD_LATENCY_NS;
            zone->d.wr_lat_ns = QLC_WR_LATENCY_NS;
            zone->d.er_lat_ns = QLC_ER_LATENCY_NS;
            zone->d.chnl_pg_xfer_lat_ns = QLC_XFER_LATENCY_NS;
        }
    }
    // if (n->flash_type == TLC) {
    //     n->upg_rd_lat_ns = TLC_UPPER_PAGE_READ_LATENCY_NS;
    //     n->cpg_rd_lat_ns = TLC_CENTER_PAGE_READ_LATENCY_NS;
    //     n->lpg_rd_lat_ns = TLC_LOWER_PAGE_READ_LATENCY_NS;
    //     n->upg_wr_lat_ns = TLC_UPPER_PAGE_WRITE_LATENCY_NS;
    //     n->cpg_wr_lat_ns = TLC_CENTER_PAGE_WRITE_LATENCY_NS;
    //     n->lpg_wr_lat_ns = TLC_LOWER_PAGE_WRITE_LATENCY_NS;
    //     n->blk_er_lat_ns = TLC_BLOCK_ERASE_LATENCY_NS;
    //     n->chnl_pg_xfer_lat_ns = TLC_CHNL_PAGE_TRANSFER_LATENCY_NS;
    // } else if (n->flash_type == QLC) {
    //     n->upg_rd_lat_ns  = QLC_UPPER_PAGE_READ_LATENCY_NS;
    //     n->cupg_rd_lat_ns = QLC_CENTER_UPPER_PAGE_READ_LATENCY_NS;
    //     n->clpg_rd_lat_ns = QLC_CENTER_LOWER_PAGE_READ_LATENCY_NS;
    //     n->lpg_rd_lat_ns  = QLC_LOWER_PAGE_READ_LATENCY_NS;
    //     n->upg_wr_lat_ns  = QLC_UPPER_PAGE_WRITE_LATENCY_NS;
    //     n->cupg_wr_lat_ns = QLC_CENTER_UPPER_PAGE_WRITE_LATENCY_NS;
    //     n->clpg_wr_lat_ns = QLC_CENTER_LOWER_PAGE_WRITE_LATENCY_NS;
    //     n->lpg_wr_lat_ns  = QLC_LOWER_PAGE_WRITE_LATENCY_NS;
    //     n->blk_er_lat_ns  = QLC_BLOCK_ERASE_LATENCY_NS;
    //     n->chnl_pg_xfer_lat_ns = QLC_CHNL_PAGE_TRANSFER_LATENCY_NS;
    // } else if (n->flash_type == MLC) {
    //     n->upg_rd_lat_ns = MLC_UPPER_PAGE_READ_LATENCY_NS;
    //     n->lpg_rd_lat_ns = MLC_LOWER_PAGE_READ_LATENCY_NS;
    //     n->upg_wr_lat_ns = MLC_UPPER_PAGE_WRITE_LATENCY_NS;
    //     n->lpg_wr_lat_ns = MLC_LOWER_PAGE_WRITE_LATENCY_NS;
    //     n->blk_er_lat_ns = MLC_BLOCK_ERASE_LATENCY_NS;
    //     n->chnl_pg_xfer_lat_ns = MLC_CHNL_PAGE_TRANSFER_LATENCY_NS;
    // }
    // -----------------------------------------------------------------------------------
}

/*
 * TODO: should be independent from different FEMU modes
 */
int64_t advance_channel_timestamp(FemuCtrl *n, int ch, uint64_t now, int opcode)
{
    uint64_t start_data_xfer_ts;
    uint64_t data_ready_ts;

    /* TODO: Considering channel-level timing */
    return now;

    pthread_spin_lock(&n->chnl_locks[ch]);
    if (now < n->chnl_next_avail_time[ch]) {
        start_data_xfer_ts = n->chnl_next_avail_time[ch];
    } else {
        start_data_xfer_ts = now;
    }

    switch (opcode) {
    case NVME_CMD_OC_READ:
    case NVME_CMD_OC_WRITE:
        // data_ready_ts = start_data_xfer_ts + n->chnl_pg_xfer_lat_ns * 2;
        // by HH----------------------------------------------------------------------------
        //data_ready_ts = start_data_xfer_ts + n->zone_array->d.chnl_pg_xfer_lat_ns * 2;
        data_ready_ts = start_data_xfer_ts + n->chnl_pg_xfer_lat_ns * 2;
        // --------------------------------------------------------------------------------
        break;
    case NVME_CMD_OC_ERASE:
        data_ready_ts = start_data_xfer_ts;
        break;
    default:
        femu_err("opcode=%d\n", opcode);
        assert(0);
    }

    n->chnl_next_avail_time[ch] = data_ready_ts;
    pthread_spin_unlock(&n->chnl_locks[ch]);

    return data_ready_ts;
}

int64_t advance_chip_timestamp(FemuCtrl *n, int lunid, uint64_t now, int opcode,
                               uint8_t page_type)
{
    int64_t lat;
    int64_t io_done_ts;

    switch (opcode) {
    case NVME_CMD_OC_READ:
    case NVME_CMD_READ:
        //by HH ---------------------------------------------------------------------------------------------------
        lat = get_page_read_latency(n->flash_type, page_type);
        //lat = get_page_read_latency(n->zone_array->d.zone_flash_type, page_type);
        break;
    case NVME_CMD_OC_WRITE:
    case NVME_CMD_WRITE:
        lat = get_page_write_latency(n->flash_type, page_type);
        //lat = get_page_write_latency(n->zone_array->d.zone_flash_type, page_type);
        break;
    case NVME_CMD_OC_ERASE:
        lat = get_blk_erase_latency(n->flash_type);
        //lat = get_blk_erase_latency(n->flash_type);
        // -----------------------------------------------------------------------------------------------------------
        break;
    default:
        assert(0);
    }

    pthread_spin_lock(&n->chip_locks[lunid]);
    if (now < n->chip_next_avail_time[lunid]) {
        n->chip_next_avail_time[lunid] += lat;
    } else {
        n->chip_next_avail_time[lunid] = now + lat;
    }
    io_done_ts = n->chip_next_avail_time[lunid];
    pthread_spin_unlock(&n->chip_locks[lunid]);

    return io_done_ts;
}

