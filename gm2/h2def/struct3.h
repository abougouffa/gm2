
    typedef struct {
	void (*set_aperture_page) (int page);
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
	/* if aperture_size<videomemory select a memory page */
	void *extensions;	/* points to copy of eeprom for mach32 */
	/* depends from actual driver/chiptype.. etc. */
    } vga_modeinfo;
