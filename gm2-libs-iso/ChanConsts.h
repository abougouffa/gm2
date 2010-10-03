/* Copyright (C) 2008, 2009, 2010
 *               Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA  02110-1301  USA */

/*
 *   taken from ChanConsts.def
 */

typedef enum openResults {
  opened,           /* the open succeeded as requested */
  wrongNameFormat,  /* given name is in the wrong format for the implementation */
  wrongFlags,       /* given flags include a value that does not apply to the device */
  tooManyOpen,      /* this device cannot support any more open channels */
  outOfChans,       /* no more channels can be allocated */
  wrongPermissions, /* file or directory permissions do not allow request */
  noRoomOnDevice,   /* storage limits on the device prevent the open */
  noSuchFile,       /* a needed file does not exist */
  fileExists,       /* a file of the given name already exists when a new one is required */
  wrongFileType,    /* the file is of the wrong type to support the required operations */
  noTextOperations, /* text operations have been requested, but are not supported */
  noRawOperations,  /* raw operations have been requested, but are not supported */
  noMixedOperations,/* text and raw operations have been requested, but they
		       are not supported in combination */
  alreadyOpen,      /* the source/destination is already open for operations not supported
		       in combination with the requested operations */
  otherProblem      /* open failed for some other reason */
} openResults;

