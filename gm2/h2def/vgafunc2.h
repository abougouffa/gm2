    typedef struct {
	int width;
	int height;
	int bytesperpixel;
	int colors;
	int linewidth;		/* scanline width in bytes */
	int maxlogicalwidth;	/* maximum logical scanline width */
	int startaddressrange;	/* changeable bits set */
	int maxpixels;		/* video memory / bytesperpixel */
	int haveblit;		/* mask of blit functions available */
	int flags;		/* other flags */

	/* Extended fields: */

	int chiptype;		/* Chiptype detected */
	int memory;		/* videomemory in KB */
	int linewidth_unit;	/* Use only a multiple of this as parameter for set_logicalwidth and
				   set_displaystart */
	char *linear_aperture;	/* points to mmap secondary mem aperture of card (NULL if unavailable) */
	int aperture_size;	/* size of aperture in KB if size>=videomemory. 0 if unavail */
	void (*set_aperture_page) (int page);
	/* if aperture_size<videomemory select a memory page */
	void *extensions;	/* points to copy of eeprom for mach32 */
	/* depends from actual driver/chiptype.. etc. */
    } vga_modeinfo;

    extern vga_modeinfo *vga_getmodeinfo(int mode);
    extern int vga_getdefaultmode(void);
    extern int vga_getcurrentmode(void);
    extern int vga_getcurrentchipset(void);
    extern char *vga_getmodename(int mode);
    extern int vga_getmodenumber(char *name);
    extern int vga_lastmodenumber(void);
    extern int vga_getoptmode(int x, int y, int colors, int bytesperpixel, int c);

    extern unsigned char *graph_mem;
    extern unsigned char *vga_getgraphmem(void);

    extern void vga_setpage(int p);
    extern void vga_setreadpage(int p);
    extern void vga_setwritepage(int p);
    extern void vga_setlogicalwidth(int w);
    extern void vga_setdisplaystart(int a);
    extern void vga_waitretrace(void);
    extern int vga_claimvideomemory(int n);
    extern void vga_disabledriverreport(void);
    extern int vga_setmodeX(void);
    extern int vga_init(void);	/* Used to return void in svgalib <= 1.12. */
    extern int vga_initf(int);
    extern int vga_getmousetype(void);
    extern int vga_getmonitortype(void);
    extern void vga_setmousesupport(int s);
    extern void vga_lockvc(void);
    extern void vga_unlockvc(void);
    extern int vga_getkey(void);
    extern int vga_oktowrite(void);
    extern void vga_copytoplanar256(unsigned char *virtualp, int pitch,
				  int voffset, int vpitch, int w, int h);
    extern void vga_copytoplanar16(unsigned char *virtualp, int pitch,
				   int voffset, int vpitch, int w, int h);
    extern void vga_copytoplane(unsigned char *virtualp, int pitch,
		       int voffset, int vpitch, int w, int h, int plane);
    extern int vga_setlinearaddressing(void);
    extern void vga_safety_fork(void (*shutdown_routine) (void));

    extern int vga_simple_init(void);
    extern void vga_chipset_saveregs(unsigned char *);
    extern void vga_chipset_setregs(unsigned char *);
