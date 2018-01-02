
#if !defined(m2range_h)
#   define m2range_h
#   if defined(__GNUG__)
#      define EXTERN extern "C"
#   else
#      define EXTERN extern
#   endif

EXTERN tree M2Range_BuildIfCallWholeHandlerLoc (location_t location, tree condition, const char *message);
EXTERN tree M2Range_BuildIfCallRealHandlerLoc (location_t location, tree condition, const char *message);

#undef EXTERN
#endif
