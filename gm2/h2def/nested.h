#define THIS

#ifdef THIS
#  ifdef THAT
#     define BAD 123
#  else
#     define GOOD 456
#  endif
#endif

#ifdef EGA			/* Kernel headers may define this. */
#  undef EGA
#endif
