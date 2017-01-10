
#ifndef mcReservedH
#define mcReservedH


/* additional tokens which extend PIM Modula-2 slightly */

typedef enum mcReserved_toktype {
  mcReserved_eoftok, mcReserved_plustok, mcReserved_minustok,
  mcReserved_timestok, mcReserved_dividetok, mcReserved_becomestok,
  mcReserved_ambersandtok, mcReserved_periodtok, mcReserved_commatok,
  mcReserved_semicolontok, mcReserved_lparatok, mcReserved_rparatok,
  mcReserved_lsbratok, mcReserved_rsbratok, mcReserved_lcbratok,
  mcReserved_rcbratok, mcReserved_uparrowtok, mcReserved_singlequotetok,
  mcReserved_equaltok, mcReserved_hashtok, mcReserved_lesstok,
  mcReserved_greatertok, mcReserved_lessgreatertok, mcReserved_lessequaltok,
  mcReserved_greaterequaltok, mcReserved_ldirectivetok,
  mcReserved_rdirectivetok, mcReserved_periodperiodtok, mcReserved_colontok,
  mcReserved_doublequotestok, mcReserved_bartok, mcReserved_andtok,
  mcReserved_arraytok, mcReserved_begintok, mcReserved_bytok,
  mcReserved_casetok, mcReserved_consttok, mcReserved_definitiontok,
  mcReserved_divtok, mcReserved_dotok, mcReserved_elsetok,
  mcReserved_elsiftok, mcReserved_endtok, mcReserved_excepttok,
  mcReserved_exittok, mcReserved_exporttok, mcReserved_finallytok,
  mcReserved_fortok, mcReserved_fromtok, mcReserved_iftok,
  mcReserved_implementationtok, mcReserved_importtok, mcReserved_intok,
  mcReserved_looptok, mcReserved_modtok, mcReserved_moduletok,
  mcReserved_nottok, mcReserved_oftok, mcReserved_ortok,
  mcReserved_packedsettok, mcReserved_pointertok, mcReserved_proceduretok,
  mcReserved_qualifiedtok, mcReserved_unqualifiedtok, mcReserved_recordtok,
  mcReserved_remtok, mcReserved_repeattok, mcReserved_retrytok,
  mcReserved_returntok, mcReserved_settok, mcReserved_thentok,
  mcReserved_totok, mcReserved_typetok, mcReserved_untiltok,
  mcReserved_vartok, mcReserved_whiletok, mcReserved_withtok,
  mcReserved_asmtok, mcReserved_volatiletok, mcReserved_periodperiodperiodtok,
  mcReserved_datetok, mcReserved_linetok, mcReserved_filetok,
  mcReserved_attributetok, mcReserved_builtintok, mcReserved_inlinetok,
  mcReserved_integertok, mcReserved_identtok, mcReserved_realtok,
  mcReserved_stringtok, mcReserved_commenttok,
} mcReserved_toktype ;

#endif
