/* Copyright (C) 2005 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

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
