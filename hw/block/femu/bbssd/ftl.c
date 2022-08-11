#include "ftl.h"
#include "../zns/zns.h"

//#define FEMU_DEBUG_FTL

void ssd_init_lines(FemuCtrl *n, struct ssd *ssd);
void ssd_init_write_pointer(FemuCtrl *n, struct ssd *ssd);
void ssd_init_params(FemuCtrl *n, struct ssdparams *spp);
void ssd_init_ch(struct ssd_channel *ch, struct ssdparams *spp);
void ssd_init_maptbl(struct ssd *ssd);
void ssd_init_rmap(struct ssd *ssd);
void ssd_init_blktbl(struct ssd *ssd);
void ssd_init_rblkmap(struct ssd *ssd);

static void *ftl_thread(void *arg);

static inline bool should_gc(struct ssd *ssd)
{
    //* by HH
    return false;
    //return (ssd->lm.free_line_cnt <= ssd->sp.gc_thres_lines);
}

static inline bool should_gc_high(struct ssd *ssd)
{
    //* by HH
    return false;    
    //return (ssd->lm.free_line_cnt <= ssd->sp.gc_thres_lines_high);
}

static inline struct ppa get_maptbl_ent(struct ssd *ssd, uint64_t lpn)
{
    return ssd->maptbl[lpn];
}

static inline void set_maptbl_ent(struct ssd *ssd, uint64_t lpn, struct ppa *ppa)
{
    ftl_assert(lpn < ssd->sp.tt_pgs);
    ssd->maptbl[lpn] = *ppa;
}

static inline struct pba get_maptbl_blk(struct ssd *ssd, uint64_t lbn)
{
    return ssd->blktbl[lbn];
}

static inline void set_maptbl_blk(struct ssd *ssd, uint64_t lbn, struct pba *pba)
{
    ftl_assert(lbn < ssd->sp.tt_blks);
    //printf("set blk maptbl lbn: %ld pblk: %d#############################################\n", lbn, pba->g.blk);
    ssd->blktbl[lbn] = *pba;
}

static uint64_t ppa2pgidx(struct ssd *ssd, struct ppa *ppa)
{
    struct ssdparams *spp = &ssd->sp;
    uint64_t pgidx;

    pgidx = ppa->g.ch  * spp->pgs_per_ch  + \
            ppa->g.lun * spp->pgs_per_lun + \
            ppa->g.pl  * spp->pgs_per_pl  + \
            ppa->g.blk * spp->pgs_per_blk + \
            ppa->g.pg;

    ftl_assert(pgidx < spp->tt_pgs);

    return pgidx;
}

static uint64_t pba2blkidx(struct ssd *ssd, struct pba *pba)
{
    struct ssdparams *spp = &ssd->sp;
    uint64_t blkidx;

    blkidx = pba->g.ch  * spp->blks_per_ch  + \
            pba->g.lun * spp->blks_per_lun + \
            pba->g.pl  * spp->blks_per_pl  + \
            pba->g.blk;

    ftl_assert(blkidx < spp->tt_blks);

    return blkidx;
}

static inline uint64_t get_rmap_ent(struct ssd *ssd, struct ppa *ppa)
{
    uint64_t pgidx = ppa2pgidx(ssd, ppa);

    return ssd->rmap[pgidx];
}

/* set rmap[page_no(ppa)] -> lpn */
static inline void set_rmap_ent(struct ssd *ssd, uint64_t lpn, struct ppa *ppa)
{
    uint64_t pgidx = ppa2pgidx(ssd, ppa);

    ssd->rmap[pgidx] = lpn;
}

static inline uint64_t get_rmap_blk(struct ssd *ssd, struct pba *pba)
{
    uint64_t blkidx = pba2blkidx(ssd, pba);

    return ssd->rblkmap[blkidx];
}

static inline void set_rmap_blk(struct ssd *ssd, uint64_t lbn, struct pba *pba)
{
    uint64_t blkidx = pba2blkidx(ssd, pba);

    ssd->rblkmap[blkidx] = lbn;
}

static inline int victim_line_cmp_pri(pqueue_pri_t next, pqueue_pri_t curr)
{
    return (next > curr);
}

static inline pqueue_pri_t victim_line_get_pri(void *a)
{
    return ((struct line *)a)->vpc;
}

static inline void victim_line_set_pri(void *a, pqueue_pri_t pri)
{
    ((struct line *)a)->vpc = pri;
}

static inline size_t victim_line_get_pos(void *a)
{
    return ((struct line *)a)->pos;
}

static inline void victim_line_set_pos(void *a, size_t pos)
{
    ((struct line *)a)->pos = pos;
}

//static void ssd_init_lines(struct ssd *ssd)
void ssd_init_lines(FemuCtrl *n, struct ssd *ssd)
{
    struct ssdparams *spp = &ssd->sp;
    struct line_mgmt *lm = &ssd->lm;
    struct line *line;

    slm.tt_lines = 16; // Line size: nch*nlun*npg -> 4096 pgs per line
    slm.lines = g_malloc0(sizeof(struct line) * spp->blks_per_pl);

    h_log("SLC line: %d, spp->blks_per_line: %d\n", slm.tt_lines, spp->blks_per_line);

    QTAILQ_INIT(&slm.free_line_list);
    QTAILQ_INIT(&slm.full_line_list);
    slm.free_line_cnt = 0;
    for (int i = 0; i < slm.tt_lines; i++) {
        line = &slm.lines[i];
        line->id = i;
        line->ipc = 0;
        line->vpc = 0;
        line->pos = 0;
        /* initialize all the lines as free lines */
        QTAILQ_INSERT_TAIL(&slm.free_line_list, line, entry);
        slm.free_line_cnt++;
    }
    ftl_assert(slm.free_line_cnt == slm.tt_lines);
    slm.victim_line_cnt = 0;
    slm.full_line_cnt = 0;   


    lm->tt_lines = spp->blks_per_pl - slm.tt_lines;
    ftl_assert(lm->tt_lines+slm.tt_lines == spp->tt_lines);
    lm->lines = g_malloc0(sizeof(struct line) * spp->blks_per_pl);

    QTAILQ_INIT(&lm->free_line_list);
    lm->victim_line_pq = pqueue_init(spp->tt_lines, victim_line_cmp_pri,
            victim_line_get_pri, victim_line_set_pri,
            victim_line_get_pos, victim_line_set_pos);
    QTAILQ_INIT(&lm->full_line_list);

    lm->free_line_cnt = 0;
    for (int i = slm.tt_lines; i < lm->tt_lines + slm.tt_lines; i++) {
        line = &lm->lines[i];
        line->id = i;
        line->ipc = 0;
        line->vpc = 0;
        line->pos = 0;
        /* initialize all the lines as free lines */
        QTAILQ_INSERT_TAIL(&lm->free_line_list, line, entry);
        lm->free_line_cnt++;
    }

    ftl_assert(lm->free_line_cnt == lm->tt_lines);
    lm->victim_line_cnt = 0;
    lm->full_line_cnt = 0;
}

//static void ssd_init_write_pointer(struct ssd *ssd)
void ssd_init_write_pointer(FemuCtrl *n, struct ssd *ssd)
{
    struct write_pointer *wpp = &ssd->wp;
    struct line_mgmt *lm = &ssd->lm;
    struct line *curline;
    write_pointer *nwp = wpzone.wpnand;

    if(slm.tt_lines > 0)
    {
        wpp->curline = NULL;
        wpp->curline = QTAILQ_FIRST(&slm.free_line_list);
        QTAILQ_REMOVE(&slm.free_line_list, wpp->curline, entry);
        slm.free_line_cnt--;

        wpp->ch = 0;
        wpp->lun = 0;
        wpp->pg = 0;
        wpp->blk = 0;
        wpp->pl = 0;
    }

    for(int i=0; i < n->num_zones; i++)
    {
        curline = NULL;
        curline = QTAILQ_FIRST(&lm->free_line_list);
        QTAILQ_REMOVE(&lm->free_line_list, curline, entry);
        lm->free_line_cnt--;
        /* wpp->curline is always our next-to-write super-block */
        nwp->curline = curline;
        nwp->ch = 0;
        nwp->lun = 0;
        nwp->pg = 0;
        nwp->blk = 0;
        nwp->pl = 0;

        nwp++;
    }
}

static inline void check_addr(int a, int max)
{
    ftl_assert(a >= 0 && a < max);
}

static struct line *get_next_free_line(struct ssd *ssd)
{
    struct line_mgmt *lm = &ssd->lm;
    struct line *curline = NULL;

    curline = QTAILQ_FIRST(&lm->free_line_list);
   // printf("TLC new line id: %d\n", curline->id);

    if (!curline) {
        ftl_err("No free lines left in [%s] !!!!\n", ssd->ssdname);
        h_log("total lines: %d, full line: %d, free line: %d\n",
            lm->tt_lines, lm->full_line_cnt, lm->free_line_cnt);
        return NULL;
    }

    QTAILQ_REMOVE(&lm->free_line_list, curline, entry);
    lm->free_line_cnt--;
    return curline;
}

static void ssd_advance_write_pointer(struct ssd *ssd, uint32_t zone_index, bool debug)
{
    struct write_pointer *wpp = wpzone.wpnand;
    wpp += zone_index;

    struct ssdparams *spp = &ssd->sp;
    struct line_mgmt *lm = &ssd->lm;

    check_addr(wpp->ch, spp->nchs);

    wpp->ch++;
    if (wpp->ch == spp->nchs) {
        wpp->ch = 0;
        check_addr(wpp->lun, spp->luns_per_ch);
        wpp->lun++;
        /* in this case, we should go to next lun */
        if (wpp->lun == spp->luns_per_ch) {
            wpp->lun = 0;
            /* go to next page in the block */
            check_addr(wpp->pg, spp->pgs_per_blk);
            wpp->pg++;
            if (wpp->pg == spp->pgs_per_blk) {
                wpp->pg = 0;
                /* move current line to {victim,full} line list */
                if (wpp->curline->vpc == spp->pgs_per_line) {
                    printf("curline to full, vpc%d pgsperline%d\n", wpp->curline->vpc, spp->pgs_per_line);
                    /* all pgs are still valid, move to full line list */
                    ftl_assert(wpp->curline->ipc == 0);
                    QTAILQ_INSERT_TAIL(&lm->full_line_list, wpp->curline, entry);
                    lm->full_line_cnt++;
                } else {
                    printf("curline to victim, vpc%d pgsperline%d\n", wpp->curline->vpc, spp->pgs_per_line);
                    ftl_assert(wpp->curline->vpc >= 0 && wpp->curline->vpc < spp->pgs_per_line);
                    /* there must be some invalid pages in this line */
                    ftl_assert(wpp->curline->ipc > 0);
                    pqueue_insert(lm->victim_line_pq, wpp->curline);
                    lm->victim_line_cnt++;
                }
                /* current line is used up, pick another empty line */
                check_addr(wpp->blk, spp->blks_per_pl);
                wpp->curline = NULL;
                wpp->curline = get_next_free_line(ssd);
                if (!wpp->curline) {
                    /* TODO */
                    abort();
                }
                h_log_nand("zone[%d] new line id: %d\n", zone_index, wpp->curline->id);
                wpp->blk = wpp->curline->id;
                check_addr(wpp->blk, spp->blks_per_pl);
                /* make sure we are starting from page 0 in the super block */
                ftl_assert(wpp->pg == 0);
                ftl_assert(wpp->lun == 0);
                ftl_assert(wpp->ch == 0);
                /* TODO: assume # of pl_per_lun is 1, fix later */
                ftl_assert(wpp->pl == 0);
            }
        }
    }
    if(debug)
    {
            printf("advance%d: ssd ch%d lun%d pl%d blk%d pg%d\n",
            zone_index, wpp->ch, wpp->lun, wpp->pl, wpp->blk, wpp->pg);
    }
}


static void slc_advance_write_pointer(struct ssd *ssd)
{
    struct write_pointer *wpp = &ssd->wp;
    struct ssdparams *spp = &ssd->sp;
    uint64_t line_size = spp->secs_per_pg * spp->pgs_per_blk * spp-> pls_per_lun * spp->luns_per_ch * spp->nchs;

    check_addr(wpp->ch, spp->nchs);

    wpp->ch++;
    if (wpp->ch == spp->nchs) {
        wpp->ch = 0;
        check_addr(wpp->lun, spp->luns_per_ch);
        wpp->lun++;
        /* in this case, we should go to next lun */
        if (wpp->lun == spp->luns_per_ch) {
            wpp->lun = 0;
            /* go to next page in the block */
            check_addr(wpp->pg, spp->pgs_per_blk);
            wpp->pg++;
            if (wpp->pg == spp->pgs_per_blk) {
                wpp->pg = 0;
                /* move current line to {victim,full} line list */
                if (wpp->curline->vpc == spp->pgs_per_line) {
                    /* all pgs are still valid, move to full line list */
                    ftl_assert(wpp->curline->ipc == 0);
                    QTAILQ_INSERT_TAIL(&slm.full_line_list, wpp->curline, entry);
                    slm.full_line_cnt++;
                    h_log_nand("line[%d] to full line list! full:%d, slc_wp: 0x%lx\n", wpp->curline->id, slm.full_line_cnt, slc_wp);
                } else {
                    ftl_assert(wpp->curline->vpc >= 0 && wpp->curline->vpc < spp->pgs_per_line);
                    /* there must be some invalid pages in this line */
                    ftl_assert(wpp->curline->ipc > 0);
                    pqueue_insert(slm.victim_line_pq, wpp->curline);
                    slm.victim_line_cnt++;
                    h_log_nand("line[%d] to victim line list!, victim:%d, slc_wp: 0x%lx\n", wpp->curline->id, slm.victim_line_cnt, slc_wp);
                }

                //* HH: slc_wp align
                if(slc_wp % line_size != 0)
                {
                    slc_wp = (slc_wp / line_size) * line_size + line_size;
                }

                /* current line is used up, pick another empty line */
                check_addr(wpp->blk, spp->blks_per_pl);
                wpp->curline = NULL;
                wpp->curline = QTAILQ_FIRST(&slm.free_line_list);
                h_log_nand("slc_wp: 0x%lx, slm_ttline: %d, nch:%d luns per ch: %d pgs per blk: %d\n",
                        slc_wp, slm.tt_lines, spp->nchs, spp->luns_per_ch, spp->pgs_per_blk);

                if (!wpp->curline) {
                    printf("No free lines left in SLC, slc_wp(0x%lx)\n", slc_wp);
                    h_log_nand("total lines: %d, full line: %d, free line: %d\n",
                        slm.tt_lines, slm.full_line_cnt, slm.free_line_cnt);

                    sleep(10000);
                }
                h_log_nand("slc new line id: %d, slc free line cnt: %d, pgs_per_blk: %d\n",
                    wpp->curline->id, slm.free_line_cnt, spp->pgs_per_blk);

                QTAILQ_REMOVE(&slm.free_line_list, wpp->curline, entry);
                h_log_nand("slc line remove from free line list\n");
                slm.free_line_cnt--;

                if (!wpp->curline) {
                    /* TODO */
                    abort();
                }
                wpp->blk = wpp->curline->id;
                printf("Advance slc write pointer: new ch(%d) lun(%d) pl(%d) blk(%d) pl(%d)\n",
                    wpp->ch, wpp->lun, wpp->pl, wpp->blk, wpp->pg);
                check_addr(wpp->blk, spp->blks_per_pl);
                /* make sure we are starting from page 0 in the super block */
                ftl_assert(wpp->pg == 0);
                ftl_assert(wpp->lun == 0);
                ftl_assert(wpp->ch == 0);
                /* TODO: assume # of pl_per_lun is 1, fix later */
                ftl_assert(wpp->pl == 0);
            }
        }
    }
    if(H_TEST_LOG) h_log_nand_verbose("slc_wp: 0x%lx, wpp ch[%d] lun[%d] pg[%d] blk[%d] curline[%d]\n",
        slc_wp,wpp->ch, wpp->lun, wpp->pg, wpp->blk, wpp->curline->id);
}



static struct ppa get_new_page(struct ssd *ssd)
{
    struct write_pointer *wpp = &ssd->wp;

    struct ppa ppa;
    ppa.ppa = 0;
    ppa.g.ch = wpp->ch;
    ppa.g.lun = wpp->lun;
    ppa.g.pg = wpp->pg;
    ppa.g.blk = wpp->blk;
    ppa.g.pl = wpp->pl;
    ftl_assert(ppa.g.pl == 0);

    //h_log_nand("slc new line id: %d\n", wpp->curline->id);

    return ppa;
}

static void check_params(struct ssdparams *spp)
{
    /*
     * we are using a general write pointer increment method now, no need to
     * force luns_per_ch and nchs to be power of 2
     */

    //ftl_assert(is_power_of_2(spp->luns_per_ch));
    //ftl_assert(is_power_of_2(spp->nchs));
}

//static void ssd_init_params(FemuCtrl *n, struct ssdparams *spp)
void ssd_init_params(FemuCtrl *n, struct ssdparams *spp)
{
    spp->secsz = 512;
    spp->secs_per_pg = 32; // 16k per pg
    spp->pgs_per_blk = 512; // 2mb per blk
    spp->blks_per_pl = 4096; // number of total lines
    spp->blks_per_pl += (2*n->num_zones)+1;
    spp->pls_per_lun = 1;
    spp->luns_per_ch = 2; // 2gb per ch
    spp->nchs = 8; // 16gb total

    // spp->pg_rd_lat = NAND_TLC_READ_LATENCY;
    // spp->pg_wr_lat = NAND_TLC_PROG_LATENCY;
    // spp->blk_er_lat = NAND_TLC_ERASE_LATENCY;
    // spp->ch_xfer_lat = 0;
    //* by HH *********************************************  
    spp->pg_slc_rd_lat = NAND_TLC_READ_LATENCY * SLC_LATENCY_COEFFIENTY;
    spp->pg_slc_wr_lat = NAND_TLC_PROG_LATENCY * SLC_LATENCY_COEFFIENTY;
    spp->blk_slc_er_lat = NAND_TLC_ERASE_LATENCY * SLC_LATENCY_COEFFIENTY;
    spp->ch_slc_xfer_lat = 0;

    spp->pg_mlc_rd_lat = NAND_TLC_READ_LATENCY * MLC_LATENCY_COEFFIENTY;
    spp->pg_mlc_wr_lat = NAND_TLC_PROG_LATENCY * MLC_LATENCY_COEFFIENTY;
    spp->blk_mlc_er_lat = NAND_TLC_ERASE_LATENCY * MLC_LATENCY_COEFFIENTY;
    spp->ch_mlc_xfer_lat = 0;

    spp->pg_tlc_rd_lat = NAND_TLC_READ_LATENCY;
    spp->pg_tlc_wr_lat = NAND_TLC_PROG_LATENCY;
    spp->blk_tlc_er_lat = NAND_TLC_ERASE_LATENCY;
    spp->ch_tlc_xfer_lat = 0;

    spp->pg_qlc_rd_lat = NAND_TLC_READ_LATENCY * QLC_LATENCY_COEFFIENTY;
    spp->pg_qlc_wr_lat = NAND_TLC_PROG_LATENCY * QLC_LATENCY_COEFFIENTY;
    spp->blk_qlc_er_lat = NAND_TLC_ERASE_LATENCY * QLC_LATENCY_COEFFIENTY;
    spp->ch_qlc_xfer_lat = 0;
    // *****************************************************

    /* calculated values */
    spp->secs_per_blk = spp->secs_per_pg * spp->pgs_per_blk;
    spp->secs_per_pl = spp->secs_per_blk * spp->blks_per_pl;
    spp->secs_per_lun = spp->secs_per_pl * spp->pls_per_lun;
    spp->secs_per_ch = spp->secs_per_lun * spp->luns_per_ch;
    spp->tt_secs = spp->secs_per_ch * spp->nchs;

    spp->pgs_per_pl = spp->pgs_per_blk * spp->blks_per_pl;
    spp->pgs_per_lun = spp->pgs_per_pl * spp->pls_per_lun;
    spp->pgs_per_ch = spp->pgs_per_lun * spp->luns_per_ch;
    spp->tt_pgs = spp->pgs_per_ch * spp->nchs;

    spp->blks_per_lun = spp->blks_per_pl * spp->pls_per_lun;
    spp->blks_per_ch = spp->blks_per_lun * spp->luns_per_ch;
    spp->tt_blks = spp->blks_per_ch * spp->nchs;

    spp->pls_per_ch =  spp->pls_per_lun * spp->luns_per_ch;
    spp->tt_pls = spp->pls_per_ch * spp->nchs;

    spp->tt_luns = spp->luns_per_ch * spp->nchs;

    /* line is special, put it at the end */
    spp->blks_per_line = spp->tt_luns; /* TODO: to fix under multiplanes */
    spp->pgs_per_line = spp->blks_per_line * spp->pgs_per_blk;
    spp->secs_per_line = spp->pgs_per_line * spp->secs_per_pg;
    spp->tt_lines = spp->blks_per_lun; /* TODO: to fix under multiplanes */

    spp->gc_thres_pcent = 0.75;
    spp->gc_thres_lines = (int)((1 - spp->gc_thres_pcent) * spp->tt_lines);
    spp->gc_thres_pcent_high = 0.95;
    spp->gc_thres_lines_high = (int)((1 - spp->gc_thres_pcent_high) * spp->tt_lines);
    spp->enable_gc_delay = true;


    check_params(spp);
}

static void ssd_init_nand_page(struct nand_page *pg, struct ssdparams *spp)
{
    pg->nsecs = spp->secs_per_pg;
    pg->sec = g_malloc0(sizeof(nand_sec_status_t) * pg->nsecs);
    for (int i = 0; i < pg->nsecs; i++) {
        pg->sec[i] = SEC_FREE;
    }
    pg->status = PG_FREE;
}

static void ssd_init_nand_blk(struct nand_block *blk, struct ssdparams *spp)
{
    blk->npgs = spp->pgs_per_blk;
    blk->pg = g_malloc0(sizeof(struct nand_page) * blk->npgs);
    for (int i = 0; i < blk->npgs; i++) {
        ssd_init_nand_page(&blk->pg[i], spp);
    }
    blk->ipc = 0;
    blk->vpc = 0;
    blk->erase_cnt = 0;
    blk->wp = 0;
}

static void ssd_init_nand_plane(struct nand_plane *pl, struct ssdparams *spp)
{
    pl->nblks = spp->blks_per_pl;
    pl->blk = g_malloc0(sizeof(struct nand_block) * pl->nblks);
    for (int i = 0; i < pl->nblks; i++) {
        ssd_init_nand_blk(&pl->blk[i], spp);
    }
}

static void ssd_init_nand_lun(struct nand_lun *lun, struct ssdparams *spp)
{
    lun->npls = spp->pls_per_lun;
    lun->pl = g_malloc0(sizeof(struct nand_plane) * lun->npls);
    for (int i = 0; i < lun->npls; i++) {
        ssd_init_nand_plane(&lun->pl[i], spp);
    }
    lun->next_lun_avail_time = 0;
    lun->busy = false;
}

//static void ssd_init_ch(struct ssd_channel *ch, struct ssdparams *spp)
void ssd_init_ch(struct ssd_channel *ch, struct ssdparams *spp)
{
    ch->nluns = spp->luns_per_ch;
    ch->lun = g_malloc0(sizeof(struct nand_lun) * ch->nluns);
    for (int i = 0; i < ch->nluns; i++) {
        ssd_init_nand_lun(&ch->lun[i], spp);
    }
    ch->next_ch_avail_time = 0;
    ch->busy = 0;
}

//static void ssd_init_maptbl(struct ssd *ssd)
void ssd_init_maptbl(struct ssd *ssd)
{
    struct ssdparams *spp = &ssd->sp;

    ssd->maptbl = g_malloc0(sizeof(struct ppa) * spp->tt_pgs);
    for (int i = 0; i < spp->tt_pgs; i++) {
        ssd->maptbl[i].ppa = UNMAPPED_PPA;
    }
}

//static void ssd_init_rmap(struct ssd *ssd)
void ssd_init_rmap(struct ssd *ssd)
{
    struct ssdparams *spp = &ssd->sp;

    ssd->rmap = g_malloc0(sizeof(uint64_t) * spp->tt_pgs);
    for (int i = 0; i < spp->tt_pgs; i++) {
        ssd->rmap[i] = INVALID_LPN;
    }
}

void ssd_init_blktbl(struct ssd *ssd)
{
    struct ssdparams *spp = &ssd->sp;

    ssd->blktbl = g_malloc0(sizeof(struct pba) * spp->tt_blks);
    for (int i = 0; i < spp->tt_blks; i++) {
        ssd->blktbl[i].pba = UNMAPPED_PBA;
    }
}

//static void ssd_init_rmap(struct ssd *ssd)
void ssd_init_rblkmap(struct ssd *ssd)
{
    struct ssdparams *spp = &ssd->sp;

    ssd->rblkmap = g_malloc0(sizeof(uint64_t) * spp->tt_blks);
    for (int i = 0; i < spp->tt_blks; i++) {
        ssd->rblkmap[i] = INVALID_LBN;
    }
}

void ssd_init(FemuCtrl *n)
{
    struct ssd *ssd = n->ssd;
    struct ssdparams *spp = &ssd->sp;

    ftl_assert(ssd);

    ssd_init_params(n, spp); 

    /* initialize ssd internal layout architecture */
    ssd->ch = g_malloc0(sizeof(struct ssd_channel) * spp->nchs);
    for (int i = 0; i < spp->nchs; i++) {
        ssd_init_ch(&ssd->ch[i], spp);
    }
h_log("init maptbl\n)");
    /* initialize maptbl */
    ssd_init_maptbl(ssd);
    ssd_init_blktbl(ssd);

h_log("init rmap\n)");
    /* initialize rmap */
    ssd_init_rmap(ssd);
    ssd_init_rblkmap(ssd);

h_log("init lines\n)");
    /* initialize all the lines */
    ssd_init_lines(n, ssd);

h_log("init w pointer\n)");
    /* initialize write pointer, this is how we allocate new pages for writes */
    ssd_init_write_pointer(n, ssd);

h_log("init thread create\n)");
    qemu_thread_create(&ssd->ftl_thread, "FEMU-FTL-Thread", ftl_thread, n,
                       QEMU_THREAD_JOINABLE);
}

static inline bool valid_ppa(struct ssd *ssd, struct ppa *ppa)
{
    struct ssdparams *spp = &ssd->sp;
    int ch = ppa->g.ch;
    int lun = ppa->g.lun;
    int pl = ppa->g.pl;
    int blk = ppa->g.blk;
    int pg = ppa->g.pg;
    int sec = ppa->g.sec;

    if (ch >= 0 && ch < spp->nchs && lun >= 0 && lun < spp->luns_per_ch && pl >=
        0 && pl < spp->pls_per_lun && blk >= 0 && blk < spp->blks_per_pl && pg
        >= 0 && pg < spp->pgs_per_blk && sec >= 0 && sec < spp->secs_per_pg)
        return true;

    return false;
}

static inline bool valid_lpn(struct ssd *ssd, uint64_t lpn)
{
    return (lpn < ssd->sp.tt_pgs);
}

static inline bool mapped_ppa(struct ppa *ppa)
{
    return !(ppa->ppa == UNMAPPED_PPA);
}

static inline struct ssd_channel *get_ch(struct ssd *ssd, struct ppa *ppa)
{
    if(ppa->g.sec == 0x89) printf("get ch%d\n", ppa->g.ch);
    return &(ssd->ch[ppa->g.ch]);
}

static inline struct nand_lun *get_lun(struct ssd *ssd, struct ppa *ppa)
{
    struct ssd_channel *ch = get_ch(ssd, ppa);
    if(ppa->g.sec == 0x89) printf("lun%d\n", ppa->g.lun);
    return &(ch->lun[ppa->g.lun]);
}

static inline struct nand_plane *get_pl(struct ssd *ssd, struct ppa *ppa)
{
    struct nand_lun *lun = get_lun(ssd, ppa);
    if(ppa->g.sec == 0x89) printf("pl%d\n", ppa->g.pl);
    return &(lun->pl[ppa->g.pl]);
}

static inline struct nand_block *get_blk(struct ssd *ssd, struct ppa *ppa)
{
    struct nand_plane *pl = get_pl(ssd, ppa);
    if(ppa->g.sec == 0x89) printf("blk%d\n", ppa->g.blk);
    return &(pl->blk[ppa->g.blk]);
}

static inline struct line *get_line(struct ssd *ssd, struct ppa *ppa)
{
    return &(ssd->lm.lines[ppa->g.blk]);
}

static inline struct nand_page *get_pg(struct ssd *ssd, struct ppa *ppa)
{
    struct nand_block *blk = get_blk(ssd, ppa);
    if(ppa->g.sec == 0x89) printf("pg%d\n", ppa->g.pg);
    return &(blk->pg[ppa->g.pg]);
}

static uint64_t ssd_advance_status(struct ssd *ssd, struct ppa *ppa, struct nand_cmd *ncmd, uint16_t flash_type)
{
    int c = ncmd->cmd;
    uint64_t cmd_stime = (ncmd->stime == 0) ? \
        qemu_clock_get_ns(QEMU_CLOCK_REALTIME) : ncmd->stime;
    uint64_t nand_stime;
    struct ssdparams *spp = &ssd->sp;
    struct nand_lun *lun = get_lun(ssd, ppa);
    uint64_t lat = 0;

    switch (c) {
    case NAND_READ:
        /* read: perform NAND cmd first */
        nand_stime = (lun->next_lun_avail_time < cmd_stime) ? cmd_stime : \
                     lun->next_lun_avail_time;

        // lun->next_lun_avail_time = nand_stime + spp->pg_rd_lat;
        //* by HH
        if(flash_type == SLC) lun->next_lun_avail_time = nand_stime + spp->pg_slc_rd_lat;
        else if(flash_type == MLC) lun->next_lun_avail_time = nand_stime + spp->pg_mlc_rd_lat;
        else if(flash_type == TLC) lun->next_lun_avail_time = nand_stime + spp->pg_tlc_rd_lat;
        else if(flash_type == QLC) lun->next_lun_avail_time = nand_stime + spp->pg_qlc_rd_lat;
        //*******************
        lat = lun->next_lun_avail_time - cmd_stime;
#if 0
        lun->next_lun_avail_time = nand_stime + spp->pg_rd_lat;

        /* read: then data transfer through channel */
        chnl_stime = (ch->next_ch_avail_time < lun->next_lun_avail_time) ? \
            lun->next_lun_avail_time : ch->next_ch_avail_time;
        ch->next_ch_avail_time = chnl_stime + spp->ch_xfer_lat;

        lat = ch->next_ch_avail_time - cmd_stime;
#endif
        break;

    case NAND_WRITE:
        /* write: transfer data through channel first */
        nand_stime = (lun->next_lun_avail_time < cmd_stime) ? cmd_stime : \
                     lun->next_lun_avail_time;
        if (ncmd->type == USER_IO) {
            // lun->next_lun_avail_time = nand_stime + spp->pg_wr_lat;
            //* by HH *************
            if(flash_type == SLC) lun->next_lun_avail_time = nand_stime + spp->pg_slc_wr_lat;
            else if(flash_type == MLC) lun->next_lun_avail_time = nand_stime + spp->pg_mlc_wr_lat;
            else if(flash_type == TLC) lun->next_lun_avail_time = nand_stime + spp->pg_tlc_wr_lat;
            else if(flash_type == QLC) lun->next_lun_avail_time = nand_stime + spp->pg_qlc_wr_lat;
            //***********************
        } else {
            // lun->next_lun_avail_time = nand_stime + spp->pg_wr_lat;
            //* by HH *************
            if(flash_type == SLC) lun->next_lun_avail_time = nand_stime + spp->pg_slc_wr_lat;
            else if(flash_type == MLC) lun->next_lun_avail_time = nand_stime + spp->pg_mlc_wr_lat;
            else if(flash_type == TLC) lun->next_lun_avail_time = nand_stime + spp->pg_tlc_wr_lat;
            else if(flash_type == QLC) lun->next_lun_avail_time = nand_stime + spp->pg_qlc_wr_lat;
            //***********************
        }
        lat = lun->next_lun_avail_time - cmd_stime;

#if 0
        chnl_stime = (ch->next_ch_avail_time < cmd_stime) ? cmd_stime : \
                     ch->next_ch_avail_time;
        ch->next_ch_avail_time = chnl_stime + spp->ch_xfer_lat;

        /* write: then do NAND program */
        nand_stime = (lun->next_lun_avail_time < ch->next_ch_avail_time) ? \
            ch->next_ch_avail_time : lun->next_lun_avail_time;
        lun->next_lun_avail_time = nand_stime + spp->pg_wr_lat;

        lat = lun->next_lun_avail_time - cmd_stime;
#endif
        break;

    case NAND_ERASE:
        /* erase: only need to advance NAND status */
        nand_stime = (lun->next_lun_avail_time < cmd_stime) ? cmd_stime : \
                     lun->next_lun_avail_time;

        // lun->next_lun_avail_time = nand_stime + spp->blk_er_lat;
        //* by HH *************
        if(flash_type == SLC) lun->next_lun_avail_time = nand_stime + spp->blk_slc_er_lat;
        else if(flash_type == MLC) lun->next_lun_avail_time = nand_stime + spp->blk_mlc_er_lat;
        else if(flash_type == TLC) lun->next_lun_avail_time = nand_stime + spp->blk_tlc_er_lat;
        else if(flash_type == QLC) lun->next_lun_avail_time = nand_stime + spp->blk_qlc_er_lat;
        //***********************

        lat = lun->next_lun_avail_time - cmd_stime;
        break;

    default:
        ftl_err("Unsupported NAND command: 0x%x\n", c);
    }

    return lat;
}

/* update SSD status about one page from PG_VALID -> PG_VALID */
// static void mark_page_invalid(struct ssd *ssd, struct ppa *ppa)
// {
//     struct line_mgmt *lm = &ssd->lm;
//     struct ssdparams *spp = &ssd->sp;
//     struct nand_block *blk = NULL;
//     struct nand_page *pg = NULL;
//     bool was_full_line = false;
//     struct line *line;

//     /* update corresponding page status */
//     pg = get_pg(ssd, ppa);
//     ftl_assert(pg->status == PG_VALID);
//     pg->status = PG_INVALID;
    
//     /* update corresponding block status */
//     blk = get_blk(ssd, ppa);
//     ftl_assert(blk->ipc >= 0 && blk->ipc < spp->pgs_per_blk);
//     blk->ipc++;
//     ftl_assert(blk->vpc > 0 && blk->vpc <= spp->pgs_per_blk);
//     blk->vpc--;

//     /* update corresponding line status */
//     line = get_line(ssd, ppa);
//     ftl_assert(line->ipc >= 0 && line->ipc < spp->pgs_per_line);

//     if (line->vpc == spp->pgs_per_line) {
//         ftl_assert(line->ipc == 0);
//         was_full_line = true;
//     }
//     line->ipc++;
//     ftl_assert(line->vpc > 0 && line->vpc <= spp->pgs_per_line);
//     /* Adjust the position of the victime line in the pq under over-writes */
//     if (line->pos) {
//         /* Note that line->vpc will be updated by this call */
//         pqueue_change_priority(lm->victim_line_pq, line->vpc - 1, line);
//     } else {
//         line->vpc--;
//     }

//     if (was_full_line) {
//         printf("full line\n");
//         printf("line id %d\n", line->id);

//         if(line->entry.tqe_next == NULL)
//         {
//             printf("next line is null");
//         }
//         else
//         {
//             printf("next line is not null");
//             printf("next line %d\n", line->entry.tqe_next->id);
//         }

//         /* move line: "full" -> "victim" */
//         QTAILQ_REMOVE(&lm->full_line_list, line, entry);

//         lm->full_line_cnt--;
//         pqueue_insert(lm->victim_line_pq, line);
//         lm->victim_line_cnt++;
//     }
// }

// static void mark_slc_page_invalid(struct ssd *ssd, struct ppa *ppa)
// {
//     struct ssdparams *spp = &ssd->sp;
//     struct nand_block *blk = NULL;
//     struct nand_page *pg = NULL;
//     bool was_full_line = false;
//     struct line *line;

//     /* update corresponding page status */
//     pg = get_pg(ssd, ppa);
//     ftl_assert(pg->status == PG_VALID);
//     pg->status = PG_INVALID;

//     /* update corresponding block status */
//     blk = get_blk(ssd, ppa);
//     ftl_assert(blk->ipc >= 0 && blk->ipc < spp->pgs_per_blk);
//     blk->ipc++;
//     ftl_assert(blk->vpc > 0 && blk->vpc <= spp->pgs_per_blk);
//     blk->vpc--;

//     /* update corresponding line status */
//     line = get_line(ssd, ppa);
//     ftl_assert(line->ipc >= 0 && line->ipc < spp->pgs_per_line);
//     if (line->vpc == spp->pgs_per_line) {
//         ftl_assert(line->ipc == 0);
//         was_full_line = true;
//     }
//     line->ipc++;
//     ftl_assert(line->vpc > 0 && line->vpc <= spp->pgs_per_line);
//     /* Adjust the position of the victime line in the pq under over-writes */
//     if (line->pos) {
//         /* Note that line->vpc will be updated by this call */
//         pqueue_change_priority(slm.victim_line_pq, line->vpc - 1, line);
//     } else {
//         line->vpc--;
//     }

//     if (was_full_line) {
//         printf("slc already full line! id:%d\n", line->id);
//         /* move line: "full" -> "victim" */
//         QTAILQ_REMOVE(&slm.full_line_list, line, entry);
//         printf("slc full line delete\n");
//         slm.full_line_cnt--;
//         pqueue_insert(slm.victim_line_pq, line);
//         slm.victim_line_cnt++;
//     }
// }

static void mark_page_valid(struct ssd *ssd, struct ppa *ppa, bool debug, uint32_t zone_index)
{
    struct nand_block *blk = NULL;
    struct nand_page *pg = NULL;
    struct line *line;
//printf("Get pg\n");
    /* update page status */
    pg = get_pg(ssd, ppa);
    ftl_assert(pg->status == PG_FREE);
    pg->status = PG_VALID;

//printf("Get blk!\n");
    /* update corresponding block status */
    blk = get_blk(ssd, ppa);
    ftl_assert(blk->vpc >= 0 && blk->vpc < ssd->sp.pgs_per_blk);
    blk->vpc++;

//printf("Get line: %d\n", ppa->g.blk);
    /* update corresponding line status */
    line = get_line(ssd, ppa);
//printf("Get line finish!\n");
    ftl_assert(line->vpc >= 0 && line->vpc < ssd->sp.pgs_per_line);
    line->vpc++;
//printf("mark page valid finish!\n");

    if(debug)
    {
        printf("zone %d linevpc: %d, ch%d lun%d pl%d blk%d pg%d\n",
            zone_index, line->vpc, ppa->g.ch, ppa->g.lun, ppa->g.pl, ppa->g.blk, ppa->g.pg);
    }    
}

static void mark_slc_page_valid(struct ssd *ssd, struct ppa *ppa)
{
    struct nand_block *blk = NULL;
    struct nand_page *pg = NULL;
    struct line *line;
    //struct write_pointer *wpp = &ssd->wp;

    /* update page status */
    pg = get_pg(ssd, ppa);
    ftl_assert(pg->status == PG_FREE);

    pg->status = PG_VALID;

//    printf("mark slc pg valid: ch%d lun%d pl%d blk%d pg%d\n",
        //ppa->g.ch, ppa->g.lun, ppa->g.pl, ppa->g.blk, ppa->g.pg);
    /* update corresponding block status */
    blk = get_blk(ssd, ppa);
//printf("get blk ch%d pl%d lun%d pblk%d pg:%d vpc:%d ipc:%d\n",
//    ppa->g.ch, ppa->g.pl, ppa->g.lun, ppa->g.blk, ppa->g.pg,blk->vpc, blk->ipc);
    ftl_assert(blk->vpc >= 0 && blk->vpc < ssd->sp.pgs_per_blk);
    blk->vpc++;
//printf("blk vpc++ ch%d pl%d lun%d pblk%d vpc:%d ipc:%d\n",
//    ppa->g.ch, ppa->g.pl, ppa->g.lun, ppa->g.blk, blk->vpc, blk->ipc);

    /* update corresponding line status */
    line = &(slm.lines[ppa->g.blk]);
//printf("get line pblk%d vpc:%d ipc:%d\n", ppa->g.blk, line->vpc, line->ipc);
    ftl_assert(line>vpc >= 0 && line->vpc < ssd->sp.pgs_per_line);

    line->vpc++;
//printf("line vpc++ pblk%d vpc:%d ipc:%d\n", ppa->g.blk, line->vpc, line->ipc);
}

static void mark_block_free(struct ssd *ssd, struct ppa *ppa)
{
    struct ssdparams *spp = &ssd->sp;
    struct nand_block *blk = get_blk(ssd, ppa);
    struct nand_page *pg = NULL;

    for (int i = 0; i < spp->pgs_per_blk; i++) {
        /* reset page status */
        pg = &blk->pg[i];
        ftl_assert(pg->nsecs == spp->secs_per_pg);
        pg->status = PG_FREE;
    }

    /* reset block status */
    ftl_assert(blk->npgs == spp->pgs_per_blk);
    blk->ipc = 0;
    blk->vpc = 0;
    blk->erase_cnt++;
}

static void gc_read_page(struct ssd *ssd, struct ppa *ppa)
{
    /* advance ssd status, we don't care about how long it takes */
    if (ssd->sp.enable_gc_delay) {
        struct nand_cmd gcr;
        gcr.type = GC_IO;
        gcr.cmd = NAND_READ;
        gcr.stime = 0;
        ssd_advance_status(ssd, ppa, &gcr, TLC);
    }
}

/* move valid page data (already in DRAM) from victim line to a new page */
static uint64_t gc_write_page(struct ssd *ssd, struct ppa *old_ppa)
{
//     struct ppa new_ppa;
//     struct nand_lun *new_lun;
//     uint64_t lpn = get_rmap_ent(ssd, old_ppa);

//     ftl_assert(valid_lpn(ssd, lpn));
//     new_ppa = get_new_page(ssd);
//     /* update maptbl */
//     set_maptbl_ent(ssd, lpn, &new_ppa);
//     /* update rmap */
//     set_rmap_ent(ssd, lpn, &new_ppa);

//     mark_page_valid(ssd, &new_ppa);

//     /* need to advance the write pointer here */
//     ssd_advance_write_pointer(ssd);

//     if (ssd->sp.enable_gc_delay) {
//         struct nand_cmd gcw;
//         gcw.type = GC_IO;
//         gcw.cmd = NAND_WRITE;
//         gcw.stime = 0;
//         ssd_advance_status(ssd, &new_ppa, &gcw, TLC);
//     }

//     /* advance per-ch gc_endtime as well */
// #if 0
//     new_ch = get_ch(ssd, &new_ppa);
//     new_ch->gc_endtime = new_ch->next_ch_avail_time;
// #endif

//     new_lun = get_lun(ssd, &new_ppa);
//     new_lun->gc_endtime = new_lun->next_lun_avail_time;

     return 0;
}

static struct line *select_victim_line(struct ssd *ssd, bool force)
{
    struct line_mgmt *lm = &ssd->lm;
    struct line *victim_line = NULL;

    victim_line = pqueue_peek(lm->victim_line_pq);
    if (!victim_line) {
        return NULL;
    }

    if (!force && victim_line->ipc < ssd->sp.pgs_per_line / 8) {
        return NULL;
    }

    pqueue_pop(lm->victim_line_pq);
    victim_line->pos = 0;
    lm->victim_line_cnt--;

    /* victim_line is a danggling node now */
    return victim_line;
}

/* here ppa identifies the block we want to clean */
static void clean_one_block(struct ssd *ssd, struct ppa *ppa)
{
    struct ssdparams *spp = &ssd->sp;
    struct nand_page *pg_iter = NULL;
    int cnt = 0;

    for (int pg = 0; pg < spp->pgs_per_blk; pg++) {
        ppa->g.pg = pg;
        pg_iter = get_pg(ssd, ppa);
        /* there shouldn't be any free page in victim blocks */
        ftl_assert(pg_iter->status != PG_FREE);
        if (pg_iter->status == PG_VALID) {
            gc_read_page(ssd, ppa);
            /* delay the maptbl update until "write" happens */
            gc_write_page(ssd, ppa);
            cnt++;
        }
    }

    ftl_assert(get_blk(ssd, ppa)->vpc == cnt);
}

static void mark_line_free(struct ssd *ssd, struct ppa *ppa)
{
    struct line_mgmt *lm = &ssd->lm;
    struct line *line = get_line(ssd, ppa);
    line->ipc = 0;
    line->vpc = 0;
    /* move this line to free line list */
    QTAILQ_INSERT_TAIL(&lm->free_line_list, line, entry);
    lm->free_line_cnt++;
}

static int do_gc(struct ssd *ssd, bool force)
{
    struct line *victim_line = NULL;
    struct ssdparams *spp = &ssd->sp;
    struct nand_lun *lunp;
    struct ppa ppa;
    int ch, lun;

    victim_line = select_victim_line(ssd, force);
    if (!victim_line) {
        return -1;
    }

    ppa.g.blk = victim_line->id;
    ftl_debug("GC-ing line:%d,ipc=%d,victim=%d,full=%d,free=%d\n", ppa.g.blk,
              victim_line->ipc, ssd->lm.victim_line_cnt, ssd->lm.full_line_cnt,
              ssd->lm.free_line_cnt);

    /* copy back valid data */
    for (ch = 0; ch < spp->nchs; ch++) {
        for (lun = 0; lun < spp->luns_per_ch; lun++) {
            ppa.g.ch = ch;
            ppa.g.lun = lun;
            ppa.g.pl = 0;
            lunp = get_lun(ssd, &ppa);
            clean_one_block(ssd, &ppa);
            mark_block_free(ssd, &ppa);

            if (spp->enable_gc_delay) {
                struct nand_cmd gce;
                gce.type = GC_IO;
                gce.cmd = NAND_ERASE;
                gce.stime = 0;
                ssd_advance_status(ssd, &ppa, &gce, TLC);
            }

            lunp->gc_endtime = lunp->next_lun_avail_time;
        }
    }

    /* update line status */
    mark_line_free(ssd, &ppa);

    return 0;
}


static int do_slc_gc(FemuCtrl *n, struct ssd *ssd)
{
    struct line *gc_line = NULL;
    struct nand_lun *lunp;
    struct nand_lun *new_lun;
    struct ppa ppa, new_ppa;
    struct pba pba, new_pba;
    struct nand_cmd gcr, gcw, gce;
    struct write_pointer *wpp;
    struct ssdparams *spp = &ssd->sp;
    struct nand_page *pg_iter = NULL;
    struct nand_block *blk;
    struct write_pointer *swpp = &ssd->wp; 

    uint64_t lba;
    int len;
    uint64_t start_lpn;
    uint64_t end_lpn;

    slctbl *tbl;
    slc_mapping *map_tbl;

    uint64_t lpn;
    uint64_t lbn;
    //uint64_t current_line;
    //int num_gc_line = slm.full_line_cnt + slm.victim_line_cnt;
    int cnt = 0;
    int ch, lun;
    //int pg;

    //int tempcnt=0;
    //int temp_erase=0;

    IN_SLC_GC = true;

    for(int i=0; i < n->num_zones; i++)
    {
        tbl = rslc.mapslc;
        tbl += i;

        wpp = wpzone.wpnand;
        wpp += i;

        while(tbl->num_slc_data != 0)
        {
            map_tbl = tbl->slcmap;
            map_tbl += tbl->num_slc_data - 1;
            tbl->num_slc_data--;

            lba = map_tbl->zdslba;
            len = map_tbl->zdnlb;
            start_lpn = lba / spp->secs_per_pg;
            end_lpn = (lba + len - 1) / spp->secs_per_pg;

            h_log_gc("zone[#%d] #data: %ld, length: %ld, start lpn: %ld, end lpn: %ld\n",
                i, tbl->num_slc_data, map_tbl->zdnlb, start_lpn, end_lpn);

            for (lpn = start_lpn; lpn <= end_lpn; lpn++)
            {
                //lbn = lpn / spp->pgs_per_blk / spp->luns_per_ch / spp->nchs;
                lbn = (lpn % (spp->nchs * spp->luns_per_ch)) + ((lpn / (spp->nchs* spp->luns_per_ch * spp->pgs_per_blk)) * spp->nchs * spp->luns_per_ch);
                //ppa = get_maptbl_ent(ssd, lpn);
                pba = get_maptbl_blk(ssd, lbn);
                if(pba.g.pl > 0)
                {
                    //printf("get maptbl blk pl: %d\n", pba.g.pl);
                    continue;
                }

                //ch = lpn % spp->nchs;
                //lun = (lpn % (spp->nchs * spp->luns_per_ch)) / spp->nchs;
                //pg = lpn / (spp->nchs * spp->luns_per_ch);
                for(int i = 0 ; i<spp->pgs_per_blk; i++)
                {
                    ppa.g.ch = pba.g.ch;
                    ppa.g.lun = pba.g.lun;
                    ppa.g.pl = pba.g.pl;
                    ppa.g.blk = pba.g.blk;
                    ppa.g.pg = i;
                    //ppa.g.sec = 0x89;
                    h_log_gc("SLC GC: lpn: %ld, lbn: %ld, ch%d lun%d pl%d blk%d pg%d\n",
                        lpn, lbn, ppa.g.ch, ppa.g.lun, ppa.g.pl, ppa.g.blk, ppa.g.pg);

                    pg_iter = get_pg(ssd, &ppa);

                    if (pg_iter->status == PG_VALID)
                    {
                        //tempcnt++;
                        //printf("gc read ");
                        //* GC read
                        gcr.type = GC_IO;
                        gcr.cmd = NAND_READ;
                        gcr.stime = 0;

    h_log_gc("advance status ");
                        ssd_advance_status(ssd, &ppa, &gcr, SLC);

                        //* GC write
                        ftl_assert(valid_lpn(ssd, lpn));

                        new_ppa.g.blk = wpp->blk;
                        new_ppa.g.ch = wpp->ch;
                        new_ppa.g.lun = wpp->lun;
                        new_ppa.g.pl = wpp->pl;
                        new_ppa.g.pg = wpp->pg;
                        h_log_gc("TLC GC: line%d, ch%d lun%d pl%d blk%d pg%d\n", wpp->curline->id, new_ppa.g.ch, new_ppa.g.lun, new_ppa.g.pl, new_ppa.g.blk, new_ppa.g.pg);

                        new_pba.g.blk = new_ppa.g.blk;
                        new_pba.g.ch = new_ppa.g.ch;
                        new_pba.g.lun = new_ppa.g.lun;
                        new_pba.g.pl = new_ppa.g.pl;

                        blk = get_blk(ssd, &new_ppa);
                        if(blk == NULL)
                        {
                            printf("Error: get GC blk is NULL! ch:%d lun:%d pl:%d blk:%d pg:%d\n",
                                new_ppa.g.ch, new_ppa.g.lun, new_ppa.g.pl, new_ppa.g.blk, new_ppa.g.pg);
                        }
                        if(blk->ipc == 0 && blk->vpc ==0)
                        {
                            h_log_gc("unmapped blk! set TLC maptbl blk:%d\n", new_pba.g.blk);
                            set_maptbl_blk(ssd, (map_tbl->target_addr / spp->secs_per_blk), &new_pba);
                            set_rmap_blk(ssd, (map_tbl->target_addr / spp->secs_per_blk), &new_pba);
                        }

                        /* update maptbl */
                        //set_maptbl_ent(ssd, (map_tbl->target_addr * spp->pgs_per_blk) + (lpn - start_line * spp->nchs * spp->luns_per_ch * spp->pgs_per_blk * spp->pgs_per_blk), &new_ppa);

                        /* update rmap */
                        //set_rmap_ent(ssd, (map_tbl->target_addr * spp->pgs_per_blk) + (lpn - start_line * spp->nchs * spp->luns_per_ch * spp->pgs_per_blk * spp->pgs_per_blk), &new_ppa);
                        h_log_gc("mark TLC page valid\n");
                        mark_page_valid(ssd, &new_ppa, false, i);

                        h_log_gc("advance write pointer\n");
                        ssd_advance_write_pointer(ssd, i, false);

    //printf("gc write %d\n", tempcnt);
                        gcw.type = GC_IO;
                        gcw.cmd = NAND_WRITE;
                        gcw.stime = 0;

                        h_log_gc("advance status\n");
                        ssd_advance_status(ssd, &new_ppa, &gcw, TLC);

                        /* advance per-ch gc_endtime as well */
                        new_lun = get_lun(ssd, &new_ppa);
                        new_lun->gc_endtime = new_lun->next_lun_avail_time;
                        
                        pg_iter->status  = PG_FREE;
                        cnt++;
                    } // for all pg in a block
                    else
                    {
                        h_log_gc("slc pg not valid! : ");
                        h_log_gc("ch%d lun%d pl%d blk%d pg%d\n",
                            ppa.g.ch, ppa.g.lun, ppa.g.pl, ppa.g.blk, ppa.g.pg);
                    }
                } // for all pg in a blk
            } // for all nlb in a mapping
        } // for all mapping in zones
    } // for all zones


    //* GC erase
    while(1)
    {
        if(slm.victim_line_cnt > 0)
        {
            gc_line = pqueue_peek(slm.victim_line_pq);
            if(!gc_line) printf("no victim line selected!! err\n");
            pqueue_pop(slm.victim_line_pq);
            gc_line->pos = 0;
            slm.victim_line_cnt--;
            slm.free_line_cnt++;
        }
        else if(slm.full_line_cnt > 0)
        {
            gc_line = QTAILQ_FIRST(&slm.full_line_list);
            if(!gc_line) printf("no full line selected!! err\n");
            QTAILQ_REMOVE(&slm.full_line_list, gc_line,entry);
            slm.full_line_cnt--;
            slm.free_line_cnt++;
        }
        else if(swpp->curline->ipc > 0 || swpp->curline->vpc > 0)
        {
            gc_line = swpp->curline;
            if(!gc_line) printf("no full line selected!! err\n");

            swpp->ch = 0;
            swpp->lun = 0;
            swpp->pl = 0;
            
            swpp->curline = QTAILQ_FIRST(&slm.free_line_list);
            QTAILQ_REMOVE(&slm.free_line_list, swpp->curline, entry);

            swpp->blk = swpp->curline->id;
            swpp->pg = 0;
        }
        else
        {
            swpp->ch = 0;
            swpp->lun = 0;
            swpp->pl = 0;
            swpp->pg = 0;
            //printf("No gc line swpp id%d ipc%d vpc%d\n", swpp->curline->id, swpp->curline->ipc, swpp->curline->vpc);
            break;
        }

        if (!gc_line) {
            printf("no GC line selected!! err\n");

            pthread_mutex_lock(&lock_slc_wp);
            slc_wp = 0;  
            pthread_mutex_unlock(&lock_slc_wp);

            return -1;
        }

        ppa.g.blk = gc_line->id;
        h_log_gc("GC erase for blk%d\n", ppa.g.blk);

        /* copy back valid data */
        for (ch = 0; ch < spp->nchs; ch++) {
            for (lun = 0; lun < spp->luns_per_ch; lun++) {
                ppa.g.ch = ch;
                ppa.g.lun = lun;
                ppa.g.pl = 0;
                lunp = get_lun(ssd, &ppa);
                mark_block_free(ssd, &ppa);

//temp_erase++;
//printf("gc erase%d\n", temp_erase);
                gce.type = GC_IO;
                gce.cmd = NAND_ERASE;
                gce.stime = 0;
                ssd_advance_status(ssd, &ppa, &gce, SLC);

                lunp->gc_endtime = lunp->next_lun_avail_time;
            }
        }
        /* update line status */
        gc_line->ipc = 0;
        gc_line->vpc = 0;
        QTAILQ_INSERT_TAIL(&slm.free_line_list, gc_line, entry);
    }

    pthread_mutex_lock(&lock_slc_wp);
    slc_wp = 0;  
    pthread_mutex_unlock(&lock_slc_wp);

    IN_SLC_GC = false;

    // QTAILQ_INIT(&slm.free_line_list);
    // QTAILQ_INIT(&slm.full_line_list);
    // slm.free_line_cnt = 0;
    // for (int i = 0; i < slm.tt_lines; i++)
    // {
    //     line = &slm.lines[i];
    //     line->id = i;
    //     line->ipc = 0;
    //     line->vpc = 0;
    //     line->pos = 0;
    //     /* initialize all the lines as free lines */
    //     QTAILQ_INSERT_TAIL(&slm.free_line_list, line, entry);
    //     slm.free_line_cnt++;
    //     printf("slm id:%d inserted to tail\n", line->id);
    // }
    // ftl_assert(slm.free_line_cnt == slm.tt_lines);
    // slm.victim_line_cnt = 0;
    // slm.full_line_cnt = 0;   

    return 0;
}

static uint64_t ssd_read(struct ssd *ssd, NvmeRequest *req)
{
    uint64_t lba = req->slba;
    struct ssdparams *spp = &ssd->sp;
    int nsecs = req->nlb;
    struct ppa ppa;
    struct pba pba;
    uint64_t start_lpn = lba / spp->secs_per_pg;
    uint64_t end_lpn = (lba + nsecs -1) / spp->secs_per_pg;
    uint64_t ch, lun, pg;
    uint64_t lpn;
    uint64_t lbn;
    uint64_t sublat, maxlat = 0;
//printf("debug] ftl read start\n");
    if (end_lpn >= spp->tt_pgs) {
        //ftl_err("start_lpn=%"PRIu64",tt_pgs=%d\n", start_lpn, ssd->sp.tt_pgs);
        ftl_err("end_lpn=%"PRIu64",tt_pgs=%d\n", end_lpn, spp->tt_pgs);
    }

    /* normal IO read path */
    for (lpn = start_lpn; lpn <= end_lpn; lpn++) {
    //for (lbn = lba/spp->secs_per_blk; lbn <= (lba + nsecs - 1)/spp->secs_per_blk; lbn++) {
        //lbn = lpn / spp->pgs_per_blk / spp->luns_per_ch / spp->nchs;
        lbn = (lpn % (spp->nchs * spp->luns_per_ch)) + ((lpn / (spp->nchs* spp->luns_per_ch * spp->pgs_per_blk)) * spp->nchs * spp->luns_per_ch);
        //ppa = get_maptbl_ent(ssd, lba / spp->secs_per_pg);
//printf("debug] get_maptbl_blk lba:0x%lx, lbn:%ld, nsecs: %d\n", lba, lbn, nsecs);
        pba = get_maptbl_blk(ssd, lbn);

        ch = lpn % spp->nchs;
        lun = (lpn % (spp->nchs * spp->luns_per_ch)) / spp->nchs;
        pg = lpn / (spp->nchs * spp->luns_per_ch);

        ppa.g.ch = ch;
        ppa.g.lun = lun;
        ppa.g.pl = pba.g.pl;
        ppa.g.blk = pba.g.blk;
        ppa.g.pg = pg;
        ppa.g.sec = pba.g.sec;

        if (!mapped_ppa(&ppa) || !valid_ppa(ssd, &ppa)) {
            //printf("%s,lpn(%" PRId64 ") not mapped to valid ppa\n", ssd->ssdname, lpn);
            //printf("Invalid ppa,ch:%d,lun:%d,blk:%d,pl:%d,pg:%d,sec:%d\n",
            //ppa.g.ch, ppa.g.lun, ppa.g.blk, ppa.g.pl, ppa.g.pg, ppa.g.sec);
            continue;
        }
//printf("debug] advance status\n");
        struct nand_cmd srd;
        srd.type = USER_IO;
        srd.cmd = NAND_READ;
        srd.stime = req->stime;
        sublat = ssd_advance_status(ssd, &ppa, &srd, req->zone_flash_type);
        maxlat = (sublat > maxlat) ? sublat : maxlat;
        //printf("debug] advance status finish\n");
    }

    return maxlat;
}

static uint64_t ssd_write(FemuCtrl *n, struct ssd *ssd, NvmeRequest *req)
{
    uint64_t lba = req->slba;
    struct ssdparams *spp = &ssd->sp;
    int len = req->nlb;
    uint64_t start_lpn = lba / spp->secs_per_pg;
    uint64_t end_lpn = (lba + len -1) / spp->secs_per_pg;
    struct ppa ppa;
    struct pba pba;
    uint64_t lpn;
    uint64_t lbn;
    uint64_t ch, lun, pg;
    uint64_t curlat = 0, maxlat = 0;
    int r;
    NvmeZone *zone = n->zone_array;
    uint32_t zone_index = lba / zone->d.zcap;
    struct nand_block *blk;
    //NvmeZone *next_zone = n->zone_array;
    //next_zone++;
    struct write_pointer *wpp;
    
    if(lba>0x183555ff)
    {
        printf("selected zoneindex %d\n", zone_index);
    }    

    if (end_lpn >= spp->tt_pgs) {
        //ftl_err("start_lpn=%"PRIu64",tt_pgs=%d\n", start_lpn, ssd->sp.tt_pgs);
        ftl_err("end_lpn=%"PRIu64",tt_pgs=%d\n", end_lpn, spp->tt_pgs);
    }

    while (should_gc_high(ssd)) {
        /* perform GC here until !should_gc(ssd) */
        r = do_gc(ssd, true);
        if (r == -1)
            break;
    }

    for (lpn = start_lpn; lpn <= end_lpn; lpn++) {
    //for (lbn = lba/spp->secs_per_blk; lbn <= (lba+len-1)/spp->secs_per_blk; lbn++) {
        //ppa = get_maptbl_ent(ssd, lpn);
        //lbn = lpn / spp->pgs_per_blk / spp->luns_per_ch / spp->nchs;
        lbn = (lpn % (spp->nchs * spp->luns_per_ch)) + ((lpn / (spp->nchs* spp->luns_per_ch * spp->pgs_per_blk)) * spp->nchs * spp->luns_per_ch);
        pba = get_maptbl_blk(ssd, lbn);
        
        ch = lpn % spp->nchs;
        lun = (lpn % (spp->nchs * spp->luns_per_ch)) / spp->nchs;
        pg = lpn / (spp->nchs * spp->luns_per_ch);

        ppa.g.ch = ch;
        ppa.g.lun = lun;
        ppa.g.pl = pba.g.pl;
        ppa.g.blk = pba.g.blk;
        ppa.g.pg = pg;

        //* HH: need to add mapped ppa
        // if (mapped_ppa(&ppa)) {
        //     /* update old page information first */
        //     mark_page_invalid(ssd, &ppa);
        //     //set_rmap_ent(ssd, INVALID_LPN, &ppa);
        // }

        /* new write */
        //ppa = get_new_page(ssd);
        wpp = wpzone.wpnand;
        wpp += zone_index;

        ppa.ppa = wpp->pg;
        ppa.g.ch = wpp->ch;
        ppa.g.lun = wpp->lun;
        ppa.g.pg = wpp->pg;
        ppa.g.blk = wpp->blk;
        ppa.g.pl = wpp->pl;
        ftl_assert(ppa.g.pl == 0);
        
        pba.g.blk = ppa.g.blk;
        pba.g.ch = ppa.g.ch;
        pba.g.lun = ppa.g.lun;
        pba.g.pl = ppa.g.pl;

        blk = get_blk(ssd, &ppa);

        if(blk->ipc == 0 && blk->vpc ==0)
        {
            set_maptbl_blk(ssd, lbn, &pba);
            set_rmap_blk(ssd, lbn, &pba);
        }

        /* update maptbl */
        //set_maptbl_ent(ssd, lpn, &ppa);

        /* update rmap */
        //set_rmap_ent(ssd, lpn, &ppa);

                if(lba>0x183555ff)
{
    printf("ftl 6\n");
    mark_page_valid(ssd, &ppa, true, zone_index);
}
        else mark_page_valid(ssd, &ppa, false, zone_index);

        if(lba>0x183555ff)
{
    printf("ftl 4\n");
    ssd_advance_write_pointer(ssd, zone_index, true);
}
        /* need to advance the write pointer here */
        else ssd_advance_write_pointer(ssd, zone_index, false);

        struct nand_cmd swr;
        swr.type = USER_IO;
        swr.cmd = NAND_WRITE;
        swr.stime = req->stime;
        /* get latency statistics */
        curlat = ssd_advance_status(ssd, &ppa, &swr, req->zone_flash_type);
        maxlat = (curlat > maxlat) ? curlat : maxlat;
    }

    // hh: debug
    return maxlat;
}

static uint64_t slc_write(struct ssd *ssd, NvmeRequest *req)
{
    uint64_t lba = req->slba;
    struct ssdparams *spp = &ssd->sp;
    int len = req->nlb;
    uint64_t start_lpn = lba / spp->secs_per_pg;
    uint64_t end_lpn = (lba + len -1) / spp->secs_per_pg;
    struct ppa ppa;
    struct pba pba;
    uint64_t lpn;
    uint64_t lbn;
    //uint64_t ch, lun, pg;
    uint64_t curlat = 0, maxlat = 0;
    struct nand_block *blk;

    if (end_lpn >= spp->tt_pgs) {
        //ftl_err("start_lpn=%"PRIu64",tt_pgs=%d\n", start_lpn, ssd->sp.tt_pgs);
        ftl_err("end_lpn=%"PRIu64",tt_pgs=%d\n", end_lpn, spp->tt_pgs);
    }
//printf("debug] ftl slc write\n");

    for (lpn = start_lpn; lpn <= end_lpn; lpn++) {
        //ppa = get_maptbl_ent(ssd, lpn);
        //lbn = lpn / spp->pgs_per_blk / spp->luns_per_ch / spp->nchs;
        //lbn = (lpn % (spp->nchs * spp->luns_per_ch)) / spp->pgs_per_blk;
        lbn = (lpn % (spp->nchs * spp->luns_per_ch)) + ((lpn / (spp->nchs* spp->luns_per_ch * spp->pgs_per_blk)) * spp->nchs * spp->luns_per_ch);

        pba = get_maptbl_blk(ssd, lbn);
        
        //ch = lpn % spp->nchs;
        //lun = (lpn % (spp->nchs * spp->luns_per_ch)) / spp->nchs;
        //pg = lpn / (spp->nchs * spp->luns_per_ch);

        // ppa.g.ch = ch;
        // ppa.g.lun = lun;
        // ppa.g.pl = pba.g.pl;
        // ppa.g.blk = pba.g.blk;
        // ppa.g.pg = pg;

        struct write_pointer *wpp = &ssd->wp;
        ppa.g.ch = wpp->ch;
        ppa.g.lun = wpp->lun;
        ppa.g.pl = wpp->pl;
        ppa.g.blk = wpp->blk;
        ppa.g.pg = wpp->pg;
        ppa.ppa = 0;


        // if (mapped_ppa(&ppa)) {
        //     printf("debug] yes mapped ppa\n");
        //     /* update old page information first */
        //     mark_slc_page_invalid(ssd, &ppa);
        //     //set_rmap_ent(ssd, INVALID_LPN, &ppa);
        //     printf("debug] mark invalid\n");
        // }

        //struct write_pointer *wpp = &ssd->wp;

        /* new write */
        ppa = get_new_page(ssd);
        //printf("slc write ch%d lun%d pl%d blk%d pg:%d\n", ppa.g.ch, ppa.g.lun, ppa.g.pl, ppa.g.blk, ppa.g.pg);

        pba.g.blk = ppa.g.blk;
        pba.g.ch = ppa.g.ch;
        pba.g.lun = ppa.g.lun;
        pba.g.pl = ppa.g.pl;

        if(pba.g.ch >= spp->nchs || pba.g.lun >= spp->luns_per_ch || pba.g.pl >= spp->pls_per_lun
            || pba.g.blk >= slm.tt_lines || ppa.g.pg > spp->pgs_per_blk)
        {
            printf("warning: Attempting to write in invalid physical area! ch(%d) lun(%d) pl(%d) blk(%d) pg(%d)\n",
                pba.g.ch, pba.g.lun, pba.g.pl, pba.g.blk, ppa.g.pg);
        }

        blk = get_blk(ssd, &ppa);
        
        if(blk->ipc == 0 && blk->vpc ==0)
        {
            h_log_gc("unmapped blk! set SLC maptbl lpn: %ld, lbn:%ld blk:%d, ch%d, lun%d\n",
                lpn, lbn, pba.g.blk, pba.g.ch, pba.g.lun);
            set_maptbl_blk(ssd, lbn, &pba);
            set_rmap_blk(ssd, lbn, &pba);
        }
        else{
//            printf("already mapped blk! lbn:%ld pblk: %d ipc: %d vpc: %d\n",
 //               lbn, ppa.g.blk, blk->ipc, blk->vpc);
        }

        /* update maptbl */
        //set_maptbl_ent(ssd, lpn, &ppa);        

        /* update rmap */
        //set_rmap_ent(ssd, lpn, &ppa);
        mark_slc_page_valid(ssd, &ppa);

        //printf("slc write: curline id%d ipc%d vpc%d\n", wpp->curline->id, wpp->curline->ipc, wpp->curline->vpc);

        /* need to advance the write pointer here */
        slc_advance_write_pointer(ssd);

        struct nand_cmd swr;
        swr.type = USER_IO;
        swr.cmd = NAND_WRITE;
        swr.stime = req->stime;
        /* get latency statistics */
        curlat = ssd_advance_status(ssd, &ppa, &swr, SLC);
        maxlat = (curlat > maxlat) ? curlat : maxlat;
    }

    //printf("debug] ftl write finish\n");
    //printf("return maxlat: 0x%lx\n", maxlat);
    return maxlat;
}

//extern bool H_TEST_LOG;
static void *ftl_thread(void *arg)
{
    FemuCtrl *n = (FemuCtrl *)arg;
    struct ssd *ssd = n->ssd;
    struct ssdparams *spp = &ssd->sp;
    NvmeRequest *req = NULL;
    uint64_t lat = 0;
    int rc;
    int i;
    clock_t idle_timer = clock();
    while (!*(ssd->dataplane_started_ptr)) {
        usleep(100000);
    }

    /* FIXME: not safe, to handle ->to_ftl and ->to_poller gracefully */
    ssd->to_ftl = n->to_ftl;
    ssd->to_poller = n->to_poller;

    while (1) {
        for (i = 1; i <= n->num_poller; i++) {
            if (!ssd->to_ftl[i] || !femu_ring_count(ssd->to_ftl[i]))
                continue;

            rc = femu_ring_dequeue(ssd->to_ftl[i], (void *)&req, 1);
            if (rc != 1) {
                printf("FEMU: FTL to_ftl dequeue failed\n");
            }

            ftl_assert(req);

            //* by HH Check zone type *****************************
            uint32_t zone_idx = (n->zone_size_log2 > 0 ? req->slba >> n->zone_size_log2 : req->slba /
                    n->zone_size);

            assert(zone_idx < n->num_zones);

            NvmeZone *zone;
            zone = n->zone_array;
            
            
            zone += zone_idx;
            req->zone_flash_type = zone->d.zone_flash_type;

            if(H_TEST_LOG) printf("to ftl slba: 0x%lx\n", req->slba);
            //*******************************************************

            switch (req->cmd.opcode) {
            case NVME_CMD_WRITE:
                if(req->cmd.cdw15 != 0x89)
                {
                    lat = ssd_write(n, ssd, req);
                }
                else
                {
                    lat = slc_write(ssd, req);
                }
                idle_timer = clock();
                break;
            case NVME_CMD_READ:
            //printf("debug] ftl read\n");
                if(H_TEST_LOG) h_log("ftl read: 0x%lx\n", req->slba);
                lat = ssd_read(ssd, req);
                idle_timer = clock();
                break;
            case NVME_CMD_DSM:
                lat = 0;
                idle_timer = clock();
                break;
            default:
                ;
            }

            req->reqlat = lat;
            req->expire_time += lat;

            rc = femu_ring_enqueue(ssd->to_poller[i], (void *)&req, 1);
            if (rc != 1) {
                ftl_err("FTL to_poller enqueue failed\n");
            }

            /* clean one line if needed (in the background) */
            if (should_gc(ssd)) {
                do_gc(ssd, false);
            }
        }

        if( false && slm.tt_lines > 0
            && (slc_wp*2 > slm.tt_lines*spp->pgs_per_blk*spp->nchs*spp->luns_per_ch)
            && ((float)(clock() - idle_timer)/CLOCKS_PER_SEC >= 1) ) 
            // if(slc_wp > 0
            //     && (float)((clock() - idle_timer)/CLOCKS_PER_SEC) > 1
            //     && spp->pgs_per_blk > 0)
        {
            printf("gc start\n");
            do_slc_gc(n, ssd);
        }

        if(H_TEST_LOG) printf("to ftl slba: 0x%lx finish\n", req->slba);
    }

    return NULL;
}

