/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/decl.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (TRUE)
#      define TRUE (1==1)
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#   include "GStorage.h"
typedef unsigned int nameKey_Name;

#   define nameKey_NulName 0
typedef struct mcPretty_writeProc_p mcPretty_writeProc;

typedef struct mcPretty_writeLnProc_p mcPretty_writeLnProc;

typedef struct _T4_r _T4;

typedef _T4 *mcPretty_pretty;

typedef unsigned int FIO_File;

FIO_File FIO_StdOut;
typedef struct symbolKey_performOperation_p symbolKey_performOperation;

#   define ASCII_tab ASCII_ht
typedef struct _T6_r _T6;

typedef struct _T9_a _T9;

typedef _T6 *alists_alist;

#   define ASCII_ht (char) 011
#   define ASCII_lf ASCII_nl
#   define ASCII_nl (char) 012
typedef struct Indexing_IndexProcedure_p Indexing_IndexProcedure;

#   define SYSTEM_BITSPERBYTE 8
#   define SYSTEM_BYTESPERWORD 4
typedef struct decl_isNodeF_p decl_isNodeF;

#   define symbolKey_NulKey NULL
typedef struct symbolKey_isSymbol_p symbolKey_isSymbol;

#   define ASCII_nul (char) 000
#   define ASCII_soh (char) 001
#   define ASCII_stx (char) 002
#   define ASCII_etx (char) 003
#   define ASCII_eot (char) 004
#   define ASCII_enq (char) 005
#   define ASCII_ack (char) 006
#   define ASCII_bel (char) 007
#   define ASCII_bs (char) 010
#   define ASCII_vt (char) 013
#   define ASCII_np (char) 014
#   define ASCII_cr (char) 015
#   define ASCII_so (char) 016
#   define ASCII_si (char) 017
#   define ASCII_dle (char) 020
#   define ASCII_dc1 (char) 021
#   define ASCII_dc2 (char) 022
#   define ASCII_dc3 (char) 023
#   define ASCII_dc4 (char) 024
#   define ASCII_nak (char) 025
#   define ASCII_syn (char) 026
#   define ASCII_etb (char) 027
#   define ASCII_can (char) 030
#   define ASCII_em (char) 031
#   define ASCII_sub (char) 032
#   define ASCII_esc (char) 033
#   define ASCII_fs (char) 034
#   define ASCII_gs (char) 035
#   define ASCII_rs (char) 036
#   define ASCII_us (char) 037
#   define ASCII_sp (char) 040
#   define ASCII_ff ASCII_np
#   define ASCII_eof ASCII_eot
#   define ASCII_del (char) 0177
#   define ASCII_EOL ASCII_nl
FIO_File FIO_StdErr;
FIO_File FIO_StdIn;
typedef long int libc_time_t;

typedef struct libc_tm_r libc_tm;

typedef libc_tm *libc_ptrToTM;

typedef struct libc_timeb_r libc_timeb;

typedef struct _T3_r _T3;

typedef _T3 *mcError_error;

int mcLexBuf_currentinteger;
unsigned int mcLexBuf_currentcolumn;
void * mcLexBuf_currentstring;
typedef struct alists_performOperation_p alists_performOperation;

typedef struct wlists_performOperation_p wlists_performOperation;

typedef struct StdIO_ProcWrite_p StdIO_ProcWrite;

typedef struct StdIO_ProcRead_p StdIO_ProcRead;

#   define indentation 3
#   define indentationC 2
#   define debugScopes FALSE
#   define debugDecl FALSE
#   define caseException TRUE
typedef struct fixupInfo_r fixupInfo;

typedef struct explistT_r explistT;

typedef struct setvalueT_r setvalueT;

typedef struct identlistT_r identlistT;

typedef struct funccallT_r funccallT;

typedef struct commentT_r commentT;

typedef struct stmtT_r stmtT;

typedef struct returnT_r returnT;

typedef struct exitT_r exitT;

typedef struct vardeclT_r vardeclT;

typedef struct typeT_r typeT;

typedef struct recordT_r recordT;

typedef struct varientT_r varientT;

typedef struct varT_r varT;

typedef struct enumerationT_r enumerationT;

typedef struct subrangeT_r subrangeT;

typedef struct subscriptT_r subscriptT;

typedef struct arrayT_r arrayT;

typedef struct stringT_r stringT;

typedef struct literalT_r literalT;

typedef struct constT_r constT;

typedef struct varparamT_r varparamT;

typedef struct paramT_r paramT;

typedef struct varargsT_r varargsT;

typedef struct optargT_r optargT;

typedef struct pointerT_r pointerT;

typedef struct recordfieldT_r recordfieldT;

typedef struct varientfieldT_r varientfieldT;

typedef struct enumerationfieldT_r enumerationfieldT;

typedef struct setT_r setT;

typedef struct componentrefT_r componentrefT;

typedef struct pointerrefT_r pointerrefT;

typedef struct arrayrefT_r arrayrefT;

typedef struct assignmentT_r assignmentT;

typedef struct ifT_r ifT;

typedef struct elsifT_r elsifT;

typedef struct loopT_r loopT;

typedef struct whileT_r whileT;

typedef struct repeatT_r repeatT;

typedef struct caseT_r caseT;

typedef struct caselabellistT_r caselabellistT;

typedef struct caselistT_r caselistT;

typedef struct rangeT_r rangeT;

typedef struct forT_r forT;

typedef struct statementT_r statementT;

typedef struct scopeT_r scopeT;

typedef struct procedureT_r procedureT;

typedef struct proctypeT_r proctypeT;

typedef struct binaryT_r binaryT;

typedef struct unaryT_r unaryT;

typedef struct moduleT_r moduleT;

typedef struct defT_r defT;

typedef struct impT_r impT;

typedef struct where_r where;

typedef struct nodeProcedure_p nodeProcedure;

typedef struct cnameT_r cnameT;

typedef struct _T1_r _T1;

#   define MaxBuf 127
#   define maxNoOfElements 5
typedef enum {explist, funccall, exit_, return_, stmtseq, comment, halt, new, dispose, inc, dec, incl, excl, nil, true, false, address, loc, byte, word, char_, cardinal, longcard, shortcard, integer, longint, shortint, real, longreal, shortreal, bitset, boolean, proc, ztype, rtype, type, record, varient, var, enumeration, subrange, array, subscript, string, const_, literal, varparam, param, varargs, optarg_, pointer, recordfield, varientfield, enumerationfield, set, proctype, procedure, def, imp, module, loop, while_, for_, repeat, case_, caselabellist, caselist, range, assignment, call, if_, elsif, constexp, neg, cast, val, plus, sub, div_, mod, mult, divide, in, adr, size, tsize, ord, float_, trunc, chr, abs_, high, throw, min, max, componentref, pointerref, arrayref, deref, equal, notequal, less, greater, greequal, lessequal, lsl, lsr, lor, land, lnot, lxor, and, or, not, identlist, vardecl, setvalue} nodeT;

#   define MaxnoOfelements 5
typedef enum {mcReserved_eoftok, mcReserved_plustok, mcReserved_minustok, mcReserved_timestok, mcReserved_dividetok, mcReserved_becomestok, mcReserved_ambersandtok, mcReserved_periodtok, mcReserved_commatok, mcReserved_semicolontok, mcReserved_lparatok, mcReserved_rparatok, mcReserved_lsbratok, mcReserved_rsbratok, mcReserved_lcbratok, mcReserved_rcbratok, mcReserved_uparrowtok, mcReserved_singlequotetok, mcReserved_equaltok, mcReserved_hashtok, mcReserved_lesstok, mcReserved_greatertok, mcReserved_lessgreatertok, mcReserved_lessequaltok, mcReserved_greaterequaltok, mcReserved_ldirectivetok, mcReserved_rdirectivetok, mcReserved_periodperiodtok, mcReserved_colontok, mcReserved_doublequotestok, mcReserved_bartok, mcReserved_andtok, mcReserved_arraytok, mcReserved_begintok, mcReserved_bytok, mcReserved_casetok, mcReserved_consttok, mcReserved_definitiontok, mcReserved_divtok, mcReserved_dotok, mcReserved_elsetok, mcReserved_elsiftok, mcReserved_endtok, mcReserved_excepttok, mcReserved_exittok, mcReserved_exporttok, mcReserved_finallytok, mcReserved_fortok, mcReserved_fromtok, mcReserved_iftok, mcReserved_implementationtok, mcReserved_importtok, mcReserved_intok, mcReserved_looptok, mcReserved_modtok, mcReserved_moduletok, mcReserved_nottok, mcReserved_oftok, mcReserved_ortok, mcReserved_packedsettok, mcReserved_pointertok, mcReserved_proceduretok, mcReserved_qualifiedtok, mcReserved_unqualifiedtok, mcReserved_recordtok, mcReserved_remtok, mcReserved_repeattok, mcReserved_retrytok, mcReserved_returntok, mcReserved_settok, mcReserved_thentok, mcReserved_totok, mcReserved_typetok, mcReserved_untiltok, mcReserved_vartok, mcReserved_whiletok, mcReserved_withtok, mcReserved_asmtok, mcReserved_volatiletok, mcReserved_periodperiodperiodtok, mcReserved_datetok, mcReserved_linetok, mcReserved_filetok, mcReserved_attributetok, mcReserved_builtintok, mcReserved_inlinetok, mcReserved_integertok, mcReserved_identtok, mcReserved_realtok, mcReserved_stringtok} mcReserved_toktype;

mcReserved_toktype mcLexBuf_currenttoken;
typedef enum {ansiC, ansiCP, pim4} language;

typedef enum {completed, blocked, partial, recursive} dependentState;

typedef enum {text, punct, space} outputStates;

typedef _T1 *decl_node;

typedef struct _T2_r _T2;

typedef _T2 *symbolKey_symbolTree;

typedef struct _T5_r _T5;

typedef struct stringRecord_r stringRecord;

typedef struct Contents_r Contents;

typedef struct _T8_a _T8;

typedef _T5 *Indexing_Index;

typedef stringRecord *DynamicStrings_String;

typedef struct _T7_r _T7;

typedef struct _T10_a _T10;

typedef _T7 *wlists_wlist;

typedef void (*mcPretty_writeProc_t) (char);
struct mcPretty_writeProc_p { mcPretty_writeProc_t proc; };

typedef void (*mcPretty_writeLnProc_t) (void);
struct mcPretty_writeLnProc_p { mcPretty_writeLnProc_t proc; };

typedef void (*symbolKey_performOperation_t) (void *);
struct symbolKey_performOperation_p { symbolKey_performOperation_t proc; };

struct _T9_a { void * array[MaxnoOfelements-1+1]; };
typedef void (*Indexing_IndexProcedure_t) (void *);
struct Indexing_IndexProcedure_p { Indexing_IndexProcedure_t proc; };

typedef unsigned int (*decl_isNodeF_t) (decl_node);
struct decl_isNodeF_p { decl_isNodeF_t proc; };

typedef unsigned int (*symbolKey_isSymbol_t) (void *);
struct symbolKey_isSymbol_p { symbolKey_isSymbol_t proc; };

struct libc_tm_r {
                   int tm_sec;
                   int tm_min;
                   int tm_hour;
                   int tm_mday;
                   int tm_mon;
                   int tm_year;
                   int tm_wday;
                   int tm_yday;
                   int tm_isdst;
                   long int tm_gmtoff;
                   void *tm_zone;
                 };

struct libc_timeb_r {
                      libc_time_t time;
                      short unsigned int millitm;
                      short unsigned int timezone;
                      short unsigned int dstflag;
                    };

struct _T3_r {
               mcError_error parent;
               mcError_error child;
               mcError_error next;
               unsigned int fatal;
               DynamicStrings_String s;
               unsigned int token;
             };

typedef void (*alists_performOperation_t) (void *);
struct alists_performOperation_p { alists_performOperation_t proc; };

typedef void (*wlists_performOperation_t) (unsigned int);
struct wlists_performOperation_p { wlists_performOperation_t proc; };

typedef void (*StdIO_ProcWrite_t) (char);
struct StdIO_ProcWrite_p { StdIO_ProcWrite_t proc; };

typedef void (*StdIO_ProcRead_t) (char *);
struct StdIO_ProcRead_p { StdIO_ProcRead_t proc; };

struct fixupInfo_r {
                     unsigned int count;
                     Indexing_Index info;
                   };

struct explistT_r {
                    Indexing_Index exp;
                  };

struct setvalueT_r {
                     decl_node type;
                     Indexing_Index values;
                   };

struct identlistT_r {
                      wlists_wlist names;
                    };

struct funccallT_r {
                     decl_node function;
                     decl_node args;
                     decl_node type;
                   };

struct commentT_r {
                    DynamicStrings_String content;
                  };

struct stmtT_r {
                 Indexing_Index statements;
               };

struct returnT_r {
                   decl_node exp;
                 };

struct exitT_r {
                 decl_node loop;
               };

struct vardeclT_r {
                    wlists_wlist names;
                    decl_node type;
                    decl_node scope;
                  };

struct typeT_r {
                 nameKey_Name name;
                 decl_node type;
                 decl_node scope;
                 unsigned int isHidden;
                 unsigned int isInternal;
               };

struct recordT_r {
                   symbolKey_symbolTree localSymbols;
                   Indexing_Index listOfSons;
                   decl_node scope;
                 };

struct varientT_r {
                    Indexing_Index listOfSons;
                    decl_node varient;
                    decl_node tag;
                    decl_node scope;
                  };

struct varT_r {
                nameKey_Name name;
                decl_node type;
                decl_node decl;
                decl_node scope;
                unsigned int isInitialised;
                unsigned int isParameter;
                unsigned int isVarParameter;
              };

struct enumerationT_r {
                        unsigned int noOfElements;
                        symbolKey_symbolTree localSymbols;
                        Indexing_Index listOfSons;
                        decl_node low;
                        decl_node high;
                        decl_node scope;
                      };

struct subrangeT_r {
                     decl_node low;
                     decl_node high;
                     decl_node type;
                     decl_node scope;
                   };

struct subscriptT_r {
                      decl_node type;
                      decl_node expr;
                    };

struct arrayT_r {
                  decl_node subr;
                  decl_node type;
                  decl_node scope;
                  unsigned int isUnbounded;
                };

struct stringT_r {
                   nameKey_Name name;
                   unsigned int length;
                   unsigned int isCharCompatible;
                   DynamicStrings_String cstring;
                   unsigned int clength;
                 };

struct literalT_r {
                    nameKey_Name name;
                    decl_node type;
                  };

struct constT_r {
                  nameKey_Name name;
                  decl_node type;
                  decl_node value;
                  decl_node scope;
                };

struct varparamT_r {
                     decl_node namelist;
                     decl_node type;
                     decl_node scope;
                     unsigned int isUnbounded;
                   };

struct paramT_r {
                  decl_node namelist;
                  decl_node type;
                  decl_node scope;
                  unsigned int isUnbounded;
                };

struct varargsT_r {
                    decl_node scope;
                  };

struct optargT_r {
                   decl_node namelist;
                   decl_node type;
                   decl_node scope;
                   decl_node init;
                 };

struct pointerT_r {
                    decl_node type;
                    decl_node scope;
                  };

struct varientfieldT_r {
                         nameKey_Name name;
                         decl_node parent;
                         decl_node varient;
                         unsigned int simple;
                         Indexing_Index listOfSons;
                         decl_node scope;
                       };

struct setT_r {
                decl_node type;
                decl_node scope;
              };

struct componentrefT_r {
                         decl_node rec;
                         decl_node field;
                         decl_node resultType;
                       };

struct pointerrefT_r {
                       decl_node ptr;
                       decl_node field;
                       decl_node resultType;
                     };

struct arrayrefT_r {
                     decl_node array;
                     decl_node index;
                     decl_node resultType;
                   };

struct assignmentT_r {
                       decl_node des;
                       decl_node expr;
                     };

struct ifT_r {
               decl_node expr;
               decl_node elsif;
               decl_node then;
               decl_node else_;
             };

struct elsifT_r {
                  decl_node expr;
                  decl_node elsif;
                  decl_node then;
                  decl_node else_;
                };

struct loopT_r {
                 decl_node statements;
                 unsigned int labelno;
               };

struct whileT_r {
                  decl_node expr;
                  decl_node statements;
                };

struct repeatT_r {
                   decl_node expr;
                   decl_node statements;
                 };

struct caseT_r {
                 decl_node expression;
                 Indexing_Index caseLabelList;
                 decl_node else_;
               };

struct caselabellistT_r {
                          decl_node caseList;
                          decl_node statements;
                        };

struct caselistT_r {
                     Indexing_Index rangePairs;
                   };

struct rangeT_r {
                  decl_node lo;
                  decl_node hi;
                };

struct forT_r {
                decl_node des;
                decl_node start;
                decl_node end;
                decl_node increment;
                decl_node statements;
              };

struct statementT_r {
                      Indexing_Index sequence;
                    };

struct scopeT_r {
                  symbolKey_symbolTree symbols;
                  Indexing_Index constants;
                  Indexing_Index types;
                  Indexing_Index procedures;
                  Indexing_Index variables;
                };

struct proctypeT_r {
                     Indexing_Index parameters;
                     unsigned int returnopt;
                     unsigned int vararg;
                     decl_node optarg_;
                     decl_node scope;
                     decl_node returnType;
                   };

struct binaryT_r {
                   decl_node left;
                   decl_node right;
                   decl_node resultType;
                 };

struct unaryT_r {
                  decl_node arg;
                  decl_node resultType;
                };

struct where_r {
                 unsigned int defDeclared;
                 unsigned int modDeclared;
                 unsigned int firstUsed;
               };

typedef void (*nodeProcedure_t) (decl_node);
struct nodeProcedure_p { nodeProcedure_t proc; };

struct cnameT_r {
                  nameKey_Name name;
                  unsigned int init;
                };

struct _T2_r {
               nameKey_Name name;
               void *key;
               symbolKey_symbolTree left;
               symbolKey_symbolTree right;
             };

struct _T5_r {
               void *ArrayStart;
               unsigned int ArraySize;
               unsigned int Used;
               unsigned int Low;
               unsigned int High;
               unsigned int Debug;
               unsigned int Map;
             };

struct _T8_a { char array[(MaxBuf-1)+1]; };
struct _T10_a { unsigned int array[maxNoOfElements-1+1]; };
struct _T4_r {
               mcPretty_writeProc write_;
               mcPretty_writeLnProc writeln;
               unsigned int needsSpace;
               unsigned int needsIndent;
               unsigned int seekPos;
               unsigned int curLine;
               unsigned int curPos;
               unsigned int indent;
               mcPretty_pretty stacked;
             };

struct _T6_r {
               unsigned int noOfelements;
               _T9 elements;
               alists_alist next;
             };

struct recordfieldT_r {
                        nameKey_Name name;
                        decl_node type;
                        unsigned int tag;
                        decl_node parent;
                        decl_node varient;
                        decl_node scope;
                        cnameT cname;
                      };

struct enumerationfieldT_r {
                             nameKey_Name name;
                             decl_node type;
                             decl_node scope;
                             unsigned int value;
                             cnameT cname;
                           };

struct procedureT_r {
                      nameKey_Name name;
                      scopeT decls;
                      decl_node scope;
                      Indexing_Index parameters;
                      unsigned int built;
                      unsigned int checking;
                      unsigned int returnopt;
                      unsigned int vararg;
                      unsigned int paramcount;
                      decl_node optarg_;
                      decl_node returnType;
                      decl_node beginStatements;
                    };

struct moduleT_r {
                   nameKey_Name name;
                   nameKey_Name source;
                   Indexing_Index importedModules;
                   fixupInfo constFixup;
                   fixupInfo enumFixup;
                   scopeT decls;
                   decl_node beginStatements;
                   decl_node finallyStatements;
                   unsigned int enumsComplete;
                   unsigned int constsComplete;
                   unsigned int visited;
                 };

struct defT_r {
                nameKey_Name name;
                nameKey_Name source;
                unsigned int hasHidden;
                unsigned int forC;
                Indexing_Index exported;
                Indexing_Index importedModules;
                fixupInfo constFixup;
                fixupInfo enumFixup;
                scopeT decls;
                unsigned int enumsComplete;
                unsigned int constsComplete;
                unsigned int visited;
              };

struct impT_r {
                nameKey_Name name;
                nameKey_Name source;
                Indexing_Index importedModules;
                fixupInfo constFixup;
                fixupInfo enumFixup;
                decl_node beginStatements;
                decl_node finallyStatements;
                decl_node definitionModule;
                scopeT decls;
                unsigned int enumsComplete;
                unsigned int constsComplete;
                unsigned int visited;
              };

struct Contents_r {
                    _T8 buf;
                    unsigned int len;
                    DynamicStrings_String next;
                  };

struct _T7_r {
               unsigned int noOfElements;
               _T10 elements;
               wlists_wlist next;
             };

typedef struct descriptor_r descriptor;

typedef descriptor *Descriptor;

typedef struct DebugInfo_r DebugInfo;

typedef enum {inuse, marked, onlist, poisoned} desState;

struct descriptor_r {
                      unsigned int charStarUsed;
                      void *charStar;
                      unsigned int charStarSize;
                      unsigned int charStarValid;
                      desState state;
                      DynamicStrings_String garbage;
                    };

struct DebugInfo_r {
                     DynamicStrings_String next;
                     void *file;
                     unsigned int line;
                     void *proc;
                   };

struct _T1_r {
               nodeT kind;  /* case tag */
               union {
                       explistT explistF;
                       exitT exitF;
                       returnT returnF;
                       stmtT stmtF;
                       commentT commentF;
                       typeT typeF;
                       recordT recordF;
                       varientT varientF;
                       varT varF;
                       enumerationT enumerationF;
                       subrangeT subrangeF;
                       subscriptT subscriptF;
                       arrayT arrayF;
                       stringT stringF;
                       constT constF;
                       literalT literalF;
                       varparamT varparamF;
                       paramT paramF;
                       varargsT varargsF;
                       optargT optargF;
                       pointerT pointerF;
                       recordfieldT recordfieldF;
                       varientfieldT varientfieldF;
                       enumerationfieldT enumerationfieldF;
                       setT setF;
                       proctypeT proctypeF;
                       procedureT procedureF;
                       defT defF;
                       impT impF;
                       moduleT moduleF;
                       loopT loopF;
                       whileT whileF;
                       forT forF;
                       repeatT repeatF;
                       caseT caseF;
                       caselabellistT caselabellistF;
                       caselistT caselistF;
                       rangeT rangeF;
                       ifT ifF;
                       elsifT elsifF;
                       assignmentT assignmentF;
                       arrayrefT arrayrefF;
                       pointerrefT pointerrefF;
                       componentrefT componentrefF;
                       binaryT binaryF;
                       unaryT unaryF;
                       identlistT identlistF;
                       vardeclT vardeclF;
                       funccallT funccallF;
                       setvalueT setvalueF;
                     };
               where at;
             };

struct stringRecord_r {
                        Contents contents;
                        Descriptor head;
                        DebugInfo debug;
                      };

static FIO_File outputFile;
static language lang;
static decl_node bitsperunitN;
static decl_node bitsperwordN;
static decl_node bitspercharN;
static decl_node unitsperwordN;
static decl_node mainModule;
static decl_node currentModule;
static decl_node defModule;
static decl_node systemN;
static decl_node addressN;
static decl_node locN;
static decl_node byteN;
static decl_node wordN;
static decl_node adrN;
static decl_node sizeN;
static decl_node tsizeN;
static decl_node newN;
static decl_node disposeN;
static decl_node incN;
static decl_node decN;
static decl_node inclN;
static decl_node exclN;
static decl_node highN;
static decl_node m2rtsN;
static decl_node haltN;
static decl_node throwN;
static decl_node chrN;
static decl_node absN;
static decl_node floatN;
static decl_node truncN;
static decl_node ordN;
static decl_node valN;
static decl_node minN;
static decl_node maxN;
static decl_node booleanN;
static decl_node procN;
static decl_node charN;
static decl_node integerN;
static decl_node cardinalN;
static decl_node longcardN;
static decl_node shortcardN;
static decl_node longintN;
static decl_node shortintN;
static decl_node bitsetN;
static decl_node bitnumN;
static decl_node ztypeN;
static decl_node rtypeN;
static decl_node realN;
static decl_node longrealN;
static decl_node shortrealN;
static decl_node nilN;
static decl_node trueN;
static decl_node falseN;
static Indexing_Index scopeStack;
static Indexing_Index defUniverseI;
static Indexing_Index modUniverseI;
static symbolKey_symbolTree modUniverse;
static symbolKey_symbolTree defUniverse;
static symbolKey_symbolTree baseSymbols;
static outputStates outputState;
static mcPretty_pretty doP;
static alists_alist todoQ;
static alists_alist partialQ;
static alists_alist doneQ;
static unsigned int mustVisitScope;
static unsigned int simplified;
static unsigned int tempCount;
static decl_node globalNode;
void SYSTEM_ShiftVal (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, int ShiftCount);
void SYSTEM_ShiftLeft (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int ShiftCount);
void SYSTEM_ShiftRight (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int ShiftCount);
void SYSTEM_RotateVal (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, int RotateCount);
void SYSTEM_RotateLeft (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int RotateCount);
void SYSTEM_RotateRight (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int RotateCount);
void M2RTS_ExecuteTerminationProcedures (void);
unsigned int M2RTS_InstallTerminationProcedure (PROC p);
void M2RTS_ExecuteInitialProcedures (void);
unsigned int M2RTS_InstallInitialProcedure (PROC p);
void M2RTS_Terminate (void);
void M2RTS_HALT (int exitcode);
void M2RTS_Halt (char *file_, unsigned int _file_high, unsigned int line, char *function_, unsigned int _function_high, char *description_, unsigned int _description_high);
void M2RTS_ExitOnHalt (int e);
void M2RTS_ErrorMessage (char *message_, unsigned int _message_high, char *file_, unsigned int _file_high, unsigned int line, char *function_, unsigned int _function_high);
unsigned int M2RTS_Length (char *a_, unsigned int _a_high);
void M2RTS_AssignmentException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_IncException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_DecException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_InclException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ExclException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ShiftException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_RotateException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_StaticArraySubscriptException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_DynamicArraySubscriptException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ForLoopBeginException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ForLoopToException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ForLoopEndException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_PointerNilException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_NoReturnException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_CaseException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_WholeNonPosDivException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_WholeNonPosModException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_WholeZeroDivException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_WholeZeroRemException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_NoException (void * filename, unsigned int line, unsigned int column, void * scope);
unsigned int decl_getDeclaredMod (decl_node n);
unsigned int decl_getDeclaredDef (decl_node n);
unsigned int decl_getFirstUsed (decl_node n);
unsigned int decl_isDef (decl_node n);
unsigned int decl_isImp (decl_node n);
unsigned int decl_isImpOrModule (decl_node n);
unsigned int decl_isVisited (decl_node n);
void decl_unsetVisited (decl_node n);
void decl_setVisited (decl_node n);
void decl_setEnumsComplete (decl_node n);
unsigned int decl_getEnumsComplete (decl_node n);
void decl_resetEnumPos (decl_node n);
decl_node decl_getNextEnum (void);
unsigned int decl_isModule (decl_node n);
unsigned int decl_isMainModule (decl_node n);
void decl_setMainModule (decl_node n);
void decl_setCurrentModule (decl_node n);
decl_node decl_lookupDef (nameKey_Name n);
decl_node decl_lookupImp (nameKey_Name n);
decl_node decl_lookupModule (nameKey_Name n);
void decl_putDefForC (decl_node n);
decl_node decl_lookupInScope (decl_node scope, nameKey_Name n);
unsigned int decl_isConst (decl_node n);
unsigned int decl_isType (decl_node n);
void decl_putType (decl_node des, decl_node exp);
decl_node decl_getType (decl_node n);
decl_node decl_skipType (decl_node n);
void decl_putTypeHidden (decl_node des);
unsigned int decl_isTypeHidden (decl_node n);
unsigned int decl_hasHidden (decl_node n);
unsigned int decl_isVar (decl_node n);
unsigned int decl_isTemporary (decl_node n);
unsigned int decl_isExported (decl_node n);
decl_node decl_getDeclScope (void);
decl_node decl_getScope (decl_node n);
unsigned int decl_isLiteral (decl_node n);
unsigned int decl_isConstSet (decl_node n);
unsigned int decl_isEnumerationField (decl_node n);
unsigned int decl_isEnumeration (decl_node n);
unsigned int decl_isUnbounded (decl_node n);
unsigned int decl_isParameter (decl_node n);
unsigned int decl_isVarParam (decl_node n);
unsigned int decl_isParam (decl_node n);
unsigned int decl_isNonVarParam (decl_node n);
decl_node decl_addOptParameter (decl_node proc, nameKey_Name id, decl_node type, decl_node init);
unsigned int decl_isOptarg (decl_node n);
unsigned int decl_isRecord (decl_node n);
unsigned int decl_isRecordField (decl_node n);
unsigned int decl_isVarientField (decl_node n);
unsigned int decl_isArray (decl_node n);
unsigned int decl_isProcType (decl_node n);
unsigned int decl_isPointer (decl_node n);
unsigned int decl_isProcedure (decl_node n);
unsigned int decl_isVarient (decl_node n);
unsigned int decl_isSet (decl_node n);
unsigned int decl_isSubrange (decl_node n);
unsigned int decl_isZtype (decl_node n);
unsigned int decl_isRtype (decl_node n);
decl_node decl_makeConst (nameKey_Name n);
void decl_putConst (decl_node n, decl_node v);
decl_node decl_makeType (nameKey_Name n);
decl_node decl_makeTypeImp (nameKey_Name n);
decl_node decl_makeVar (nameKey_Name n);
void decl_putVar (decl_node var, decl_node type, decl_node decl);
decl_node decl_makeVarDecl (decl_node i, decl_node type);
decl_node decl_makeEnum (void);
decl_node decl_makeEnumField (decl_node e, nameKey_Name n);
decl_node decl_makeSubrange (decl_node low, decl_node high);
void decl_putSubrangeType (decl_node sub, decl_node type);
decl_node decl_makePointer (decl_node type);
decl_node decl_makeSet (decl_node type);
decl_node decl_makeArray (decl_node subr, decl_node type);
void decl_putUnbounded (decl_node n);
decl_node decl_makeRecord (void);
decl_node decl_makeVarient (decl_node r);
decl_node decl_addFieldsToRecord (decl_node r, decl_node v, decl_node i, decl_node t);
void decl_buildVarientSelector (decl_node r, decl_node v, nameKey_Name tag, decl_node type);
decl_node decl_buildVarientFieldRecord (decl_node v, decl_node p);
nameKey_Name decl_getSymName (decl_node n);
decl_node decl_import (decl_node m, decl_node n);
decl_node decl_lookupExported (decl_node n, nameKey_Name i);
decl_node decl_lookupSym (nameKey_Name n);
void decl_addImportedModule (decl_node m, decl_node i, unsigned int scoped);
void decl_setSource (decl_node n, nameKey_Name s);
nameKey_Name decl_getSource (decl_node n);
decl_node decl_getMainModule (void);
decl_node decl_getCurrentModule (void);
void decl_foreachDefModuleDo (symbolKey_performOperation p);
void decl_foreachModModuleDo (symbolKey_performOperation p);
void decl_enterScope (decl_node n);
void decl_leaveScope (void);
decl_node decl_makeProcedure (nameKey_Name n);
decl_node decl_makeProcType (void);
void decl_putProcTypeOptReturn (decl_node proc);
void decl_putReturnType (decl_node p, decl_node type);
decl_node decl_makeVarParameter (decl_node l, decl_node type);
decl_node decl_makeNonVarParameter (decl_node l, decl_node type);
void decl_paramEnter (decl_node n);
void decl_paramLeave (decl_node n);
decl_node decl_makeIdentList (void);
unsigned int decl_putIdent (decl_node n, nameKey_Name i);
void decl_addVarParameters (decl_node n, decl_node i, decl_node type);
void decl_addNonVarParameters (decl_node n, decl_node i, decl_node type);
decl_node decl_makeVarargs (void);
unsigned int decl_isVarargs (decl_node n);
void decl_addParameter (decl_node proc, decl_node param);
decl_node decl_makeBinaryTok (mcReserved_toktype op, decl_node l, decl_node r);
decl_node decl_makeUnaryTok (mcReserved_toktype op, decl_node e);
decl_node decl_makeComponentRef (decl_node rec, decl_node field);
decl_node decl_makePointerRef (decl_node ptr, decl_node field);
unsigned int decl_isPointerRef (decl_node n);
decl_node decl_makeDeRef (decl_node n);
decl_node decl_makeArrayRef (decl_node array, decl_node index);
decl_node decl_getLastOp (decl_node n);
decl_node decl_getCardinal (void);
decl_node decl_makeLiteralInt (nameKey_Name n);
decl_node decl_makeLiteralReal (nameKey_Name n);
decl_node decl_makeString (nameKey_Name n);
decl_node decl_makeSetValue (void);
unsigned int decl_isSetValue (decl_node n);
decl_node decl_putSetValue (decl_node n, decl_node t);
decl_node decl_includeSetValue (decl_node n, decl_node l, decl_node h);
decl_node decl_getBuiltinConst (nameKey_Name n);
decl_node decl_makeExpList (void);
unsigned int decl_isExpList (decl_node n);
void decl_putExpList (decl_node n, decl_node e);
decl_node decl_makeConstExp (void);
decl_node decl_getNextConstExp (void);
void decl_setConstExpComplete (decl_node n);
decl_node decl_fixupConstExp (decl_node c, decl_node e);
void decl_resetConstExpPos (decl_node n);
decl_node decl_makeFuncCall (decl_node c, decl_node n);
decl_node decl_makeStatementSequence (void);
unsigned int decl_isStatementSequence (decl_node n);
void decl_addStatement (decl_node s, decl_node n);
decl_node decl_makeReturn (void);
unsigned int decl_isReturn (decl_node n);
void decl_putReturn (decl_node n, decl_node e);
decl_node decl_makeWhile (void);
void decl_putWhile (decl_node n, decl_node e, decl_node s);
unsigned int decl_isWhile (decl_node n);
decl_node decl_makeAssignment (decl_node d, decl_node e);
void decl_putBegin (decl_node b, decl_node s);
void decl_putFinally (decl_node b, decl_node s);
decl_node decl_makeExit (decl_node l, unsigned int n);
unsigned int decl_isExit (decl_node n);
decl_node decl_makeLoop (void);
unsigned int decl_isLoop (decl_node n);
void decl_putLoop (decl_node l, decl_node s);
decl_node decl_makeComment (char *a_, unsigned int _a_high);
decl_node decl_makeIf (decl_node e, decl_node s);
unsigned int decl_isIf (decl_node n);
decl_node decl_makeElsif (decl_node i, decl_node e, decl_node s);
unsigned int decl_isElsif (decl_node n);
void decl_putElse (decl_node i, decl_node s);
decl_node decl_makeFor (void);
unsigned int decl_isFor (decl_node n);
void decl_putFor (decl_node f, decl_node i, decl_node s, decl_node e, decl_node b, decl_node sq);
decl_node decl_makeRepeat (void);
unsigned int decl_isRepeat (decl_node n);
void decl_putRepeat (decl_node n, decl_node s, decl_node e);
decl_node decl_makeCase (void);
unsigned int decl_isCase (decl_node n);
decl_node decl_putCaseExpression (decl_node n, decl_node e);
decl_node decl_putCaseElse (decl_node n, decl_node e);
decl_node decl_putCaseStatement (decl_node n, decl_node l, decl_node s);
decl_node decl_makeCaseLabelList (decl_node l, decl_node s);
unsigned int decl_isCaseLabelList (decl_node n);
decl_node decl_makeCaseList (void);
unsigned int decl_isCaseList (decl_node n);
decl_node decl_putCaseRange (decl_node n, decl_node lo, decl_node hi);
decl_node decl_makeRange (decl_node lo, decl_node hi);
unsigned int decl_isRange (decl_node n);
decl_node decl_dupExpr (decl_node n);
void decl_setLangC (void);
void decl_setLangCP (void);
void decl_setLangM2 (void);
void decl_out (void);
nameKey_Name nameKey_makeKey (char *a_, unsigned int _a_high);
nameKey_Name nameKey_makekey (void * a);
void nameKey_getKey (nameKey_Name key, char *a, unsigned int _a_high);
unsigned int nameKey_lengthKey (nameKey_Name key);
unsigned int nameKey_isKey (char *a_, unsigned int _a_high);
void nameKey_writeKey (nameKey_Name key);
unsigned int nameKey_isSameExcludingCase (nameKey_Name key1, nameKey_Name key2);
void * nameKey_keyToCharStar (nameKey_Name key);
symbolKey_symbolTree symbolKey_initTree (void);
void symbolKey_killTree (symbolKey_symbolTree *t);
void * symbolKey_getSymKey (symbolKey_symbolTree t, nameKey_Name name);
void symbolKey_putSymKey (symbolKey_symbolTree t, nameKey_Name name, void * key);
void symbolKey_delSymKey (symbolKey_symbolTree t, nameKey_Name name);
unsigned int symbolKey_isEmptyTree (symbolKey_symbolTree t);
unsigned int symbolKey_doesTreeContainAny (symbolKey_symbolTree t, symbolKey_isSymbol p);
void symbolKey_foreachNodeDo (symbolKey_symbolTree t, symbolKey_performOperation p);
void mcDebug_assert (unsigned int q);
void mcDebug_writeDebug (char *a_, unsigned int _a_high);
void Storage_ALLOCATE (void * *a, unsigned int Size);
void Storage_DEALLOCATE (void * *a, unsigned int Size);
void Storage_REALLOCATE (void * *a, unsigned int Size);
unsigned int Storage_Available (unsigned int Size);
unsigned int SFIO_Exists (DynamicStrings_String fname);
FIO_File SFIO_OpenToRead (DynamicStrings_String fname);
FIO_File SFIO_OpenToWrite (DynamicStrings_String fname);
FIO_File SFIO_OpenForRandom (DynamicStrings_String fname, unsigned int towrite, unsigned int newfile);
DynamicStrings_String SFIO_WriteS (FIO_File file, DynamicStrings_String s);
DynamicStrings_String SFIO_ReadS (FIO_File file);
unsigned int FIO_IsNoError (FIO_File f);
unsigned int FIO_IsActive (FIO_File f);
unsigned int FIO_Exists (char *fname_, unsigned int _fname_high);
FIO_File FIO_OpenToRead (char *fname_, unsigned int _fname_high);
FIO_File FIO_OpenToWrite (char *fname_, unsigned int _fname_high);
FIO_File FIO_OpenForRandom (char *fname_, unsigned int _fname_high, unsigned int towrite, unsigned int newfile);
void FIO_Close (FIO_File f);
unsigned int FIO_exists (void * fname, unsigned int flength);
FIO_File FIO_openToRead (void * fname, unsigned int flength);
FIO_File FIO_openToWrite (void * fname, unsigned int flength);
FIO_File FIO_openForRandom (void * fname, unsigned int flength, unsigned int towrite, unsigned int newfile);
void FIO_FlushBuffer (FIO_File f);
unsigned int FIO_ReadNBytes (FIO_File f, unsigned int nBytes, void * a);
void FIO_ReadAny (FIO_File f, unsigned char *a, unsigned int _a_high);
unsigned int FIO_WriteNBytes (FIO_File f, unsigned int nBytes, void * a);
void FIO_WriteAny (FIO_File f, unsigned char *a, unsigned int _a_high);
void FIO_WriteChar (FIO_File f, char ch);
unsigned int FIO_EOF (FIO_File f);
unsigned int FIO_EOLN (FIO_File f);
unsigned int FIO_WasEOLN (FIO_File f);
char FIO_ReadChar (FIO_File f);
void FIO_UnReadChar (FIO_File f, char ch);
void FIO_WriteLine (FIO_File f);
void FIO_WriteString (FIO_File f, char *a_, unsigned int _a_high);
void FIO_ReadString (FIO_File f, char *a, unsigned int _a_high);
void FIO_WriteCardinal (FIO_File f, unsigned int c);
unsigned int FIO_ReadCardinal (FIO_File f);
int FIO_GetUnixFileDescriptor (FIO_File f);
void FIO_SetPositionFromBeginning (FIO_File f, long int pos);
void FIO_SetPositionFromEnd (FIO_File f, long int pos);
long int FIO_FindPosition (FIO_File f);
void FIO_GetFileName (FIO_File f, char *a, unsigned int _a_high);
void * FIO_getFileName (FIO_File f);
unsigned int FIO_getFileNameLength (FIO_File f);
void FIO_FlushOutErr (void);
DynamicStrings_String DynamicStrings_InitString (char *a_, unsigned int _a_high);
DynamicStrings_String DynamicStrings_KillString (DynamicStrings_String s);
void DynamicStrings_Fin (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_InitStringCharStar (void * a);
DynamicStrings_String DynamicStrings_InitStringChar (char ch);
DynamicStrings_String DynamicStrings_Mark (DynamicStrings_String s);
unsigned int DynamicStrings_Length (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_ConCat (DynamicStrings_String a, DynamicStrings_String b);
DynamicStrings_String DynamicStrings_ConCatChar (DynamicStrings_String a, char ch);
DynamicStrings_String DynamicStrings_Assign (DynamicStrings_String a, DynamicStrings_String b);
DynamicStrings_String DynamicStrings_Dup (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_Add (DynamicStrings_String a, DynamicStrings_String b);
unsigned int DynamicStrings_Equal (DynamicStrings_String a, DynamicStrings_String b);
unsigned int DynamicStrings_EqualCharStar (DynamicStrings_String s, void * a);
unsigned int DynamicStrings_EqualArray (DynamicStrings_String s, char *a_, unsigned int _a_high);
DynamicStrings_String DynamicStrings_Mult (DynamicStrings_String s, unsigned int n);
DynamicStrings_String DynamicStrings_Slice (DynamicStrings_String s, int low, int high);
int DynamicStrings_Index (DynamicStrings_String s, char ch, unsigned int o);
int DynamicStrings_RIndex (DynamicStrings_String s, char ch, unsigned int o);
DynamicStrings_String DynamicStrings_RemoveComment (DynamicStrings_String s, char comment);
DynamicStrings_String DynamicStrings_RemoveWhitePrefix (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_RemoveWhitePostfix (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_ToUpper (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_ToLower (DynamicStrings_String s);
void DynamicStrings_CopyOut (char *a, unsigned int _a_high, DynamicStrings_String s);
char DynamicStrings_char (DynamicStrings_String s, int i);
void * DynamicStrings_string (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_InitStringDB (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_InitStringCharStarDB (void * a, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_InitStringCharDB (char ch, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_MultDB (DynamicStrings_String s, unsigned int n, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_DupDB (DynamicStrings_String s, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_SliceDB (DynamicStrings_String s, int low, int high, char *file_, unsigned int _file_high, unsigned int line);
void DynamicStrings_PushAllocation (void);
void DynamicStrings_PopAllocation (unsigned int halt);
DynamicStrings_String DynamicStrings_PopAllocationExemption (unsigned int halt, DynamicStrings_String e);
DynamicStrings_String StringConvert_IntegerToString (int i, unsigned int width, char padding, unsigned int sign, unsigned int base, unsigned int lower);
DynamicStrings_String StringConvert_CardinalToString (unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
int StringConvert_StringToInteger (DynamicStrings_String s, unsigned int base, unsigned int *found);
unsigned int StringConvert_StringToCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
DynamicStrings_String StringConvert_LongIntegerToString (long int i, unsigned int width, char padding, unsigned int sign, unsigned int base, unsigned int lower);
long int StringConvert_StringToLongInteger (DynamicStrings_String s, unsigned int base, unsigned int *found);
DynamicStrings_String StringConvert_LongCardinalToString (long unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
long unsigned int StringConvert_StringToLongCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
DynamicStrings_String StringConvert_ShortCardinalToString (short unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
short unsigned int StringConvert_StringToShortCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
int StringConvert_stoi (DynamicStrings_String s);
DynamicStrings_String StringConvert_itos (int i, unsigned int width, char padding, unsigned int sign);
DynamicStrings_String StringConvert_ctos (unsigned int c, unsigned int width, char padding);
unsigned int StringConvert_stoc (DynamicStrings_String s);
int StringConvert_hstoi (DynamicStrings_String s);
int StringConvert_ostoi (DynamicStrings_String s);
int StringConvert_bstoi (DynamicStrings_String s);
unsigned int StringConvert_hstoc (DynamicStrings_String s);
unsigned int StringConvert_ostoc (DynamicStrings_String s);
unsigned int StringConvert_bstoc (DynamicStrings_String s);
long double StringConvert_StringToLongreal (DynamicStrings_String s, unsigned int *found);
DynamicStrings_String StringConvert_LongrealToString (long double x, unsigned int TotalWidth, unsigned int FractionWidth);
double StringConvert_stor (DynamicStrings_String s);
long double StringConvert_stolr (DynamicStrings_String s);
DynamicStrings_String StringConvert_ToSigFig (DynamicStrings_String s, unsigned int n);
DynamicStrings_String StringConvert_ToDecimalPlaces (DynamicStrings_String s, unsigned int n);
DynamicStrings_String mcOptions_handleOptions (void);
unsigned int mcOptions_getQuiet (void);
unsigned int mcOptions_getVerbose (void);
unsigned int mcOptions_getInternalDebugging (void);
DynamicStrings_String mcOptions_getCppCommandLine (void);
DynamicStrings_String mcOptions_getOutputFile (void);
unsigned int mcOptions_getExtendedOpaque (void);
void mcOptions_setDebugTopological (unsigned int value);
unsigned int mcOptions_getDebugTopological (void);
DynamicStrings_String mcOptions_getHPrefix (void);
unsigned int mcOptions_getIgnoreFQ (void);
DynamicStrings_String FormatStrings_Sprintf0 (DynamicStrings_String s);
DynamicStrings_String FormatStrings_Sprintf1 (DynamicStrings_String s, unsigned char *w_, unsigned int _w_high);
DynamicStrings_String FormatStrings_Sprintf2 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
DynamicStrings_String FormatStrings_Sprintf3 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
DynamicStrings_String FormatStrings_Sprintf4 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
int libc_write (int d, void * buf, int nbytes);
int libc_read (int d, void * buf, int nbytes);
int libc_system (void * a);
void libc_abort (void);
void * libc_malloc (unsigned int size);
void libc_free (void * ptr);
void * libc_realloc (void * ptr, unsigned int size);
int libc_isatty (int fd);
void libc_exit (int r);
void * libc_getenv (void * s);
int libc_getpid (void);
int libc_dup (int d);
int libc_close (int d);
int libc_open (void * filename, int oflag, ...);
int libc_creat (void * filename, unsigned int mode);
long int libc_lseek (int fd, long int offset, int whence);
void libc_perror (char *string_, unsigned int _string_high);
int libc_readv (int fd, void * v, int n);
int libc_writev (int fd, void * v, int n);
void * libc_getcwd (void * buf, int size);
int libc_chown (void * filename, int uid, int gid);
int libc_strlen (void * a);
void * libc_strcpy (void * dest, void * src);
void * libc_strncpy (void * dest, void * src, unsigned int n);
int libc_unlink (void * file);
void * libc_memcpy (void * dest, void * src, unsigned int size);
void * libc_memset (void * s, int c, unsigned int size);
void * libc_memmove (void * dest, void * src, unsigned int size);
int libc_printf (char *format_, unsigned int _format_high, ...);
int libc_setenv (void * name, void * value, int overwrite);
void libc_srand (int seed);
int libc_rand (void);
libc_time_t libc_time (void * a);
void * libc_localtime (libc_time_t *t);
int libc_ftime (libc_timeb *t);
int libc_shutdown (int s, int how);
int libc_rename (void * oldpath, void * newpath);
int libc_setjmp (void * env);
void libc_longjmp (void * env, int val);
void libc_atexit (PROC proc);
void * libc_ttyname (int filedes);
unsigned int libc_sleep (unsigned int seconds);
void mcMetaError_metaError1 (char *m_, unsigned int _m_high, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaError2 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaError3 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaError4 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrors1 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrors2 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrors3 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrors4 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrorT1 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrorT2 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrorT3 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrorT4 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrorsT1 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrorsT2 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrorsT3 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrorsT4 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrorString1 (DynamicStrings_String m, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrorString2 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrorString3 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrorString4 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrorStringT1 (unsigned int tok, DynamicStrings_String m, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrorStringT2 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrorStringT3 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrorStringT4 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcError_internalError (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line);
void mcError_writeFormat0 (char *a_, unsigned int _a_high);
void mcError_writeFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcError_writeFormat2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
void mcError_writeFormat3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
mcError_error mcError_newError (unsigned int atTokenNo);
mcError_error mcError_newWarning (unsigned int atTokenNo);
mcError_error mcError_chainError (unsigned int atTokenNo, mcError_error e);
void mcError_errorFormat0 (mcError_error e, char *a_, unsigned int _a_high);
void mcError_errorFormat1 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcError_errorFormat2 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
void mcError_errorFormat3 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
void mcError_errorString (mcError_error e, DynamicStrings_String str);
void mcError_errorStringAt (DynamicStrings_String s, unsigned int tok);
void mcError_errorStringAt2 (DynamicStrings_String s, unsigned int tok1, unsigned int tok2);
void mcError_errorStringsAt2 (DynamicStrings_String s1, DynamicStrings_String s2, unsigned int tok1, unsigned int tok2);
void mcError_warnStringAt (DynamicStrings_String s, unsigned int tok);
void mcError_warnStringAt2 (DynamicStrings_String s, unsigned int tok1, unsigned int tok2);
void mcError_warnStringsAt2 (DynamicStrings_String s1, DynamicStrings_String s2, unsigned int tok1, unsigned int tok2);
void mcError_warnFormat0 (char *a_, unsigned int _a_high);
void mcError_warnFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcError_flushErrors (void);
void mcError_flushWarnings (void);
void mcError_errorAbort0 (char *a_, unsigned int _a_high);
unsigned int mcLexBuf_openSource (DynamicStrings_String s);
void mcLexBuf_closeSource (void);
void mcLexBuf_reInitialize (void);
void mcLexBuf_resetForNewPass (void);
void mcLexBuf_getToken (void);
void mcLexBuf_insertToken (mcReserved_toktype token);
void mcLexBuf_insertTokenAndRewind (mcReserved_toktype token);
unsigned int mcLexBuf_getPreviousTokenLineNo (void);
unsigned int mcLexBuf_getLineNo (void);
unsigned int mcLexBuf_getTokenNo (void);
unsigned int mcLexBuf_tokenToLineNo (unsigned int tokenNo, unsigned int depth);
unsigned int mcLexBuf_getColumnNo (void);
unsigned int mcLexBuf_tokenToColumnNo (unsigned int tokenNo, unsigned int depth);
DynamicStrings_String mcLexBuf_findFileNameFromToken (unsigned int tokenNo, unsigned int depth);
DynamicStrings_String mcLexBuf_getFileName (void);
void mcLexBuf_addTok (mcReserved_toktype t);
void mcLexBuf_addTokCharStar (mcReserved_toktype t, void * s);
void mcLexBuf_addTokInteger (mcReserved_toktype t, int i);
void mcLexBuf_setFile (void * filename);
void mcLexBuf_pushFile (void * filename);
void mcLexBuf_popFile (void * filename);
void StrLib_StrConCat (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high, char *c, unsigned int _c_high);
unsigned int StrLib_StrLess (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
unsigned int StrLib_StrEqual (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
unsigned int StrLib_StrLen (char *a_, unsigned int _a_high);
void StrLib_StrCopy (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
unsigned int StrLib_IsSubString (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
void StrLib_StrRemoveWhitePrefix (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
mcPretty_pretty mcPretty_initPretty (mcPretty_writeProc w, mcPretty_writeLnProc l);
mcPretty_pretty mcPretty_dupPretty (mcPretty_pretty p);
void mcPretty_killPretty (mcPretty_pretty *p);
mcPretty_pretty mcPretty_pushPretty (mcPretty_pretty p);
mcPretty_pretty mcPretty_popPretty (mcPretty_pretty p);
unsigned int mcPretty_getindent (mcPretty_pretty p);
void mcPretty_setindent (mcPretty_pretty p, unsigned int n);
unsigned int mcPretty_getcurpos (mcPretty_pretty s);
unsigned int mcPretty_getseekpos (mcPretty_pretty s);
unsigned int mcPretty_getcurline (mcPretty_pretty s);
void mcPretty_setNeedSpace (mcPretty_pretty s);
void mcPretty_noSpace (mcPretty_pretty s);
void mcPretty_print (mcPretty_pretty p, char *a_, unsigned int _a_high);
void mcPretty_prints (mcPretty_pretty p, DynamicStrings_String s);
void mcPretty_raw (mcPretty_pretty p, DynamicStrings_String s);
Indexing_Index Indexing_InitIndex (unsigned int low);
Indexing_Index Indexing_KillIndex (Indexing_Index i);
Indexing_Index Indexing_DebugIndex (Indexing_Index i);
unsigned int Indexing_InBounds (Indexing_Index i, unsigned int n);
unsigned int Indexing_HighIndice (Indexing_Index i);
unsigned int Indexing_LowIndice (Indexing_Index i);
void Indexing_PutIndice (Indexing_Index i, unsigned int n, void * a);
void * Indexing_GetIndice (Indexing_Index i, unsigned int n);
unsigned int Indexing_IsIndiceInIndex (Indexing_Index i, void * a);
void Indexing_RemoveIndiceFromIndex (Indexing_Index i, void * a);
void Indexing_DeleteIndice (Indexing_Index i, unsigned int j);
void Indexing_IncludeIndiceIntoIndex (Indexing_Index i, void * a);
void Indexing_ForeachIndiceInIndexDo (Indexing_Index i, Indexing_IndexProcedure p);
alists_alist alists_initList (void);
void alists_killList (alists_alist *l);
void alists_putItemIntoList (alists_alist l, void * c);
void * alists_getItemFromList (alists_alist l, unsigned int n);
unsigned int alists_getIndexOfList (alists_alist l, void * c);
unsigned int alists_noOfItemsInList (alists_alist l);
void alists_includeItemIntoList (alists_alist l, void * c);
void alists_removeItemFromList (alists_alist l, void * c);
unsigned int alists_isItemInList (alists_alist l, void * c);
void alists_foreachItemInListDo (alists_alist l, alists_performOperation p);
alists_alist alists_duplicateList (alists_alist l);
wlists_wlist wlists_initList (void);
void wlists_killList (wlists_wlist *l);
void wlists_putItemIntoList (wlists_wlist l, unsigned int c);
unsigned int wlists_getItemFromList (wlists_wlist l, unsigned int n);
unsigned int wlists_getIndexOfList (wlists_wlist l, unsigned int c);
unsigned int wlists_noOfItemsInList (wlists_wlist l);
void wlists_includeItemIntoList (wlists_wlist l, unsigned int c);
void wlists_removeItemFromList (wlists_wlist l, unsigned int c);
unsigned int wlists_isItemInList (wlists_wlist l, unsigned int c);
void wlists_foreachItemInListDo (wlists_wlist l, wlists_performOperation p);
wlists_wlist wlists_duplicateList (wlists_wlist l);
void keyc_useStorage (void);
void keyc_useFree (void);
void keyc_useMalloc (void);
void keyc_useProc (void);
void keyc_useTrue (void);
void keyc_useFalse (void);
void keyc_useNull (void);
void keyc_useMemcpy (void);
void keyc_useIntMin (void);
void keyc_useUIntMin (void);
void keyc_useLongMin (void);
void keyc_useULongMin (void);
void keyc_useCharMin (void);
void keyc_useUCharMin (void);
void keyc_useIntMax (void);
void keyc_useUIntMax (void);
void keyc_useLongMax (void);
void keyc_useULongMax (void);
void keyc_useCharMax (void);
void keyc_useUCharMax (void);
void keyc_useLabs (void);
void keyc_useAbs (void);
void keyc_useFabs (void);
void keyc_useFabsl (void);
void keyc_genDefs (mcPretty_pretty p);
void keyc_enterScope (decl_node n);
void keyc_leaveScope (decl_node n);
DynamicStrings_String keyc_cname (nameKey_Name n, unsigned int scopes);
FIO_File mcStream_openFrag (unsigned int id);
void mcStream_setDest (FIO_File f);
FIO_File mcStream_combine (void);
void StrIO_WriteLn (void);
void StrIO_ReadString (char *a, unsigned int _a_high);
void StrIO_WriteString (char *a_, unsigned int _a_high);
void NumberIO_ReadCard (unsigned int *x);
void NumberIO_WriteCard (unsigned int x, unsigned int n);
void NumberIO_ReadHex (unsigned int *x);
void NumberIO_WriteHex (unsigned int x, unsigned int n);
void NumberIO_ReadInt (int *x);
void NumberIO_WriteInt (int x, unsigned int n);
void NumberIO_CardToStr (unsigned int x, unsigned int n, char *a, unsigned int _a_high);
void NumberIO_StrToCard (char *a_, unsigned int _a_high, unsigned int *x);
void NumberIO_HexToStr (unsigned int x, unsigned int n, char *a, unsigned int _a_high);
void NumberIO_StrToHex (char *a_, unsigned int _a_high, unsigned int *x);
void NumberIO_IntToStr (int x, unsigned int n, char *a, unsigned int _a_high);
void NumberIO_StrToInt (char *a_, unsigned int _a_high, int *x);
void NumberIO_ReadOct (unsigned int *x);
void NumberIO_WriteOct (unsigned int x, unsigned int n);
void NumberIO_OctToStr (unsigned int x, unsigned int n, char *a, unsigned int _a_high);
void NumberIO_StrToOct (char *a_, unsigned int _a_high, unsigned int *x);
void NumberIO_ReadBin (unsigned int *x);
void NumberIO_WriteBin (unsigned int x, unsigned int n);
void NumberIO_BinToStr (unsigned int x, unsigned int n, char *a, unsigned int _a_high);
void NumberIO_StrToBin (char *a_, unsigned int _a_high, unsigned int *x);
void NumberIO_StrToBinInt (char *a_, unsigned int _a_high, int *x);
void NumberIO_StrToHexInt (char *a_, unsigned int _a_high, int *x);
void NumberIO_StrToOctInt (char *a_, unsigned int _a_high, int *x);
void Debug_Halt (char *Message_, unsigned int _Message_high, unsigned int LineNo, char *Module_, unsigned int _Module_high);
void Debug_DebugString (char *a_, unsigned int _a_high);
void Assertion_Assert (unsigned int Condition);
void StdIO_Read (char *ch);
void StdIO_Write (char ch);
void StdIO_PushOutput (StdIO_ProcWrite p);
void StdIO_PopOutput (void);
StdIO_ProcWrite StdIO_GetCurrentOutput (void);
void StdIO_PushInput (StdIO_ProcRead p);
void StdIO_PopInput (void);
StdIO_ProcRead StdIO_GetCurrentInput (void);
void mcPrintf_printf0 (char *a_, unsigned int _a_high);
void mcPrintf_printf1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcPrintf_printf2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
void mcPrintf_printf3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
void mcPrintf_printf4 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
void mcPrintf_fprintf0 (FIO_File file, char *a_, unsigned int _a_high);
void mcPrintf_fprintf1 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcPrintf_fprintf2 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
void mcPrintf_fprintf3 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
void mcPrintf_fprintf4 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
static decl_node newNode (nodeT k);
static void disposeNode (decl_node *n);
static unsigned int isLocal (decl_node n);
static void importEnumFields (decl_node m, decl_node n);
static fixupInfo initFixupInfo (void);
static decl_node makeDef (nameKey_Name n);
static decl_node makeImp (nameKey_Name n);
static decl_node makeModule (nameKey_Name n);
static unsigned int isDefForC (decl_node n);
static void initDecls (scopeT *decls);
static decl_node addTo (scopeT *decls, decl_node d);
static void export (decl_node d, decl_node n);
static decl_node addToScope (decl_node n);
static void addModuleToScope (decl_node m, decl_node i);
static void completedEnum (decl_node n);
static void setUnary (decl_node u, nodeT k, decl_node a, decl_node t);
static void putVarBool (decl_node v, unsigned int init, unsigned int param, unsigned int isvar);
static unsigned int isVarDecl (decl_node n);
static void makeVariablesFromParameters (decl_node proc, decl_node id, decl_node type, unsigned int isvar);
static decl_node addProcedureToScope (decl_node d, nameKey_Name n);
static void putProcTypeReturn (decl_node proc, decl_node type);
static decl_node makeOptParameter (decl_node l, decl_node type, decl_node init);
static unsigned int setwatch (decl_node n);
static unsigned int runwatch (void);
static unsigned int isIdentList (decl_node n);
static unsigned int identListLen (decl_node n);
static void checkParameters (decl_node p, decl_node i, decl_node type, unsigned int var);
static void checkMakeVariables (decl_node n, decl_node i, decl_node type, unsigned int isvar);
static decl_node makeVarientField (decl_node v, decl_node p);
static void putFieldVarient (decl_node f, decl_node v);
static decl_node putFieldRecord (decl_node r, nameKey_Name tag, decl_node type, decl_node v);
static void ensureOrder (Indexing_Index i, decl_node a, decl_node b);
static void putVarientTag (decl_node v, decl_node tag);
static decl_node getParent (decl_node n);
static decl_node getRecord (decl_node n);
static unsigned int isConstExp (decl_node c);
static void addEnumToModule (decl_node m, decl_node e);
static decl_node getNextFixup (fixupInfo *f);
static decl_node doMakeEnum (void);
static decl_node doMakeEnumField (decl_node e, nameKey_Name n);
static decl_node getExpList (decl_node p, unsigned int n);
static unsigned int expListLen (decl_node p);
static unsigned int getConstExpComplete (decl_node n);
static void addConstToModule (decl_node m, decl_node e);
static decl_node doMakeConstExp (void);
static unsigned int isAnyType (decl_node n);
static decl_node makeCast (decl_node c, decl_node p);
static unsigned int isFuncCall (decl_node n);
static void putTypeInternal (decl_node des);
static unsigned int isTypeInternal (decl_node n);
static decl_node lookupBase (nameKey_Name n);
static void dumpScopes (void);
static void out0 (char *a_, unsigned int _a_high);
static void out1 (char *a_, unsigned int _a_high, decl_node s);
static void out2 (char *a_, unsigned int _a_high, unsigned int c, decl_node s);
static void out3 (char *a_, unsigned int _a_high, unsigned int l, nameKey_Name n, decl_node s);
static unsigned int isUnary (decl_node n);
static decl_node makeUnary (nodeT k, decl_node e, decl_node res);
static unsigned int isLeafString (decl_node n);
static DynamicStrings_String getStringContents (decl_node n);
static decl_node foldBinary (nodeT k, decl_node l, decl_node r, decl_node res);
static decl_node makeBinary (nodeT k, decl_node l, decl_node r, decl_node res);
static decl_node doMakeBinary (nodeT k, decl_node l, decl_node r, decl_node res);
static decl_node doMakeComponentRef (decl_node rec, decl_node field);
static unsigned int isComponentRef (decl_node n);
static unsigned int isArrayRef (decl_node n);
static unsigned int isDeref (decl_node n);
static decl_node makeBase (nodeT k);
static unsigned int isOrdinal (decl_node n);
static decl_node mixTypes (decl_node a, decl_node b);
static decl_node doSetExprType (decl_node *t, decl_node n);
static decl_node getMaxMinType (decl_node n);
static decl_node doGetFuncType (decl_node n);
static decl_node doGetExprType (decl_node n);
static decl_node getExprType (decl_node n);
static void openOutput (void);
static void closeOutput (void);
static void write (char ch);
static void writeln (void);
static void doIncludeC (decl_node n);
static decl_node getSymScope (decl_node n);
static DynamicStrings_String getFQstring (decl_node n);
static DynamicStrings_String getFQDstring (decl_node n, unsigned int scopes);
static DynamicStrings_String getString (decl_node n);
static void doNone (decl_node n);
static void doNothing (decl_node n);
static void doConstC (decl_node n);
static unsigned int needsParen (decl_node n);
static void doUnary (mcPretty_pretty p, char *op_, unsigned int _op_high, decl_node expr, decl_node type, unsigned int l, unsigned int r);
static void doSetSub (mcPretty_pretty p, decl_node left, decl_node right);
static void doPolyBinary (mcPretty_pretty p, nodeT op, decl_node left, decl_node right, unsigned int l, unsigned int r);
static void doBinary (mcPretty_pretty p, char *op_, unsigned int _op_high, decl_node left, decl_node right, unsigned int l, unsigned int r);
static void doPostUnary (mcPretty_pretty p, char *op_, unsigned int _op_high, decl_node expr);
static void doDeRefC (mcPretty_pretty p, decl_node expr);
static decl_node doGetLastOp (decl_node a, decl_node b);
static void doComponentRefC (mcPretty_pretty p, decl_node l, decl_node r);
static void doPointerRefC (mcPretty_pretty p, decl_node l, decl_node r);
static void doPreBinary (mcPretty_pretty p, char *op_, unsigned int _op_high, decl_node left, decl_node right, unsigned int l, unsigned int r);
static void doConstExpr (mcPretty_pretty p, decl_node n);
static void doEnumerationField (mcPretty_pretty p, decl_node n);
static unsigned int isZero (decl_node n);
static void doArrayRef (mcPretty_pretty p, decl_node n);
static void doProcedure (mcPretty_pretty p, decl_node n);
static void doRecordfield (mcPretty_pretty p, decl_node n);
static void doCastC (mcPretty_pretty p, decl_node t, decl_node e);
static void doSetValueC (mcPretty_pretty p, decl_node n);
static decl_node getSetLow (decl_node n);
static void doInC (mcPretty_pretty p, decl_node l, decl_node r);
static void doThrowC (mcPretty_pretty p, decl_node n);
static void outNull (mcPretty_pretty p);
static void outTrue (mcPretty_pretty p);
static void outFalse (mcPretty_pretty p);
static void doExprC (mcPretty_pretty p, decl_node n);
static void doExprM2 (mcPretty_pretty p, decl_node n);
static void doVar (mcPretty_pretty p, decl_node n);
static void doLiteralC (mcPretty_pretty p, decl_node n);
static void doLiteral (mcPretty_pretty p, decl_node n);
static unsigned int isString (decl_node n);
static void doString (mcPretty_pretty p, decl_node n);
static DynamicStrings_String doEscapeC (DynamicStrings_String s, char ch);
static DynamicStrings_String escapeContentsC (DynamicStrings_String s, char ch);
static DynamicStrings_String replaceChar (DynamicStrings_String s, char ch, char *a_, unsigned int _a_high);
static DynamicStrings_String toCstring (nameKey_Name n);
static unsigned int countChar (DynamicStrings_String s, char ch);
static unsigned int lenCstring (DynamicStrings_String s);
static void outCstring (mcPretty_pretty p, decl_node s, unsigned int aString);
static void doStringC (mcPretty_pretty p, decl_node n);
static unsigned int isPunct (char ch);
static unsigned int isWhite (char ch);
static void outText (mcPretty_pretty p, char *a_, unsigned int _a_high);
static void outRawS (mcPretty_pretty p, DynamicStrings_String s);
static mcPretty_pretty outKm2 (mcPretty_pretty p, char *a_, unsigned int _a_high);
static mcPretty_pretty outKc (mcPretty_pretty p, char *a_, unsigned int _a_high);
static void outTextS (mcPretty_pretty p, DynamicStrings_String s);
static void outCard (mcPretty_pretty p, unsigned int c);
static void outTextN (mcPretty_pretty p, nameKey_Name n);
static void doTypeAliasC (mcPretty_pretty p, decl_node n, decl_node *m);
static void doEnumerationC (mcPretty_pretty p, decl_node n);
static void doNamesC (mcPretty_pretty p, nameKey_Name n);
static void doNameC (mcPretty_pretty p, decl_node n);
static void initCname (cnameT *c);
static nameKey_Name doCname (nameKey_Name n, cnameT *c, unsigned int scopes);
static nameKey_Name getDName (decl_node n, unsigned int scopes);
static void doDNameC (mcPretty_pretty p, decl_node n, unsigned int scopes);
static void doFQDNameC (mcPretty_pretty p, decl_node n, unsigned int scopes);
static void doFQNameC (mcPretty_pretty p, decl_node n);
static void doNameM2 (mcPretty_pretty p, decl_node n);
static void doHighC (mcPretty_pretty p, decl_node a, nameKey_Name n);
static void doParamC (mcPretty_pretty p, decl_node n);
static void doVarParamC (mcPretty_pretty p, decl_node n);
static void doOptargC (mcPretty_pretty p, decl_node n);
static void doParameterC (mcPretty_pretty p, decl_node n);
static void doProcTypeC (mcPretty_pretty p, decl_node t, decl_node n);
static void doTypesC (decl_node n);
static void doCompletePartialC (decl_node n);
static void doCompletePartialRecord (mcPretty_pretty p, decl_node t, decl_node r);
static void doCompletePartialArray (mcPretty_pretty p, decl_node t, decl_node r);
static decl_node lookupConst (decl_node type, nameKey_Name n);
static decl_node doMin (decl_node n);
static decl_node doMax (decl_node n);
static decl_node getMax (decl_node n);
static decl_node getMin (decl_node n);
static void doSubtractC (mcPretty_pretty p, decl_node s);
static void doSubrC (mcPretty_pretty p, decl_node s);
static void doCompletePartialProcType (mcPretty_pretty p, decl_node t, decl_node n);
static unsigned int isBase (decl_node n);
static void doBaseC (mcPretty_pretty p, decl_node n);
static unsigned int isSystem (decl_node n);
static void doSystemC (mcPretty_pretty p, decl_node n);
static void doArrayC (mcPretty_pretty p, decl_node n);
static void doPointerC (mcPretty_pretty p, decl_node n, decl_node *m);
static void doRecordFieldC (mcPretty_pretty p, decl_node f);
static void doVarientFieldC (mcPretty_pretty p, decl_node n);
static void doVarientC (mcPretty_pretty p, decl_node n);
static void doRecordC (mcPretty_pretty p, decl_node n, decl_node *m);
static unsigned int isBitset (decl_node n);
static unsigned int isNegative (decl_node n);
static void doSubrangeC (mcPretty_pretty p, decl_node n);
static void doSetC (mcPretty_pretty p, decl_node n);
static void doTypeC (mcPretty_pretty p, decl_node n, decl_node *m);
static void doArrayNameC (mcPretty_pretty p, decl_node n);
static void doRecordNameC (mcPretty_pretty p, decl_node n);
static void doTypeNameC (mcPretty_pretty p, decl_node n);
static void doVarC (decl_node n);
static void doProcedureHeadingC (decl_node n);
static unsigned int checkDeclareUnboundedParamCopyC (mcPretty_pretty p, decl_node n);
static void checkUnboundedParamCopyC (mcPretty_pretty p, decl_node n);
static void doUnboundedParamCopyC (mcPretty_pretty p, decl_node n);
static void doPrototypeC (decl_node n);
static void addTodo (decl_node n);
static void addVariablesTodo (decl_node n);
static void addTypesTodo (decl_node n);
static DynamicStrings_String tempName (void);
static decl_node makeIntermediateType (DynamicStrings_String s, decl_node p);
static void simplifyType (alists_alist l, decl_node *p);
static void simplifyVar (alists_alist l, decl_node n);
static void simplifyRecord (alists_alist l, decl_node n);
static void simplifyVarient (alists_alist l, decl_node n);
static void simplifyVarientField (alists_alist l, decl_node n);
static void doSimplifyNode (alists_alist l, decl_node n);
static void simplifyNode (alists_alist l, decl_node n);
static void doSimplify (decl_node n);
static void simplifyTypes (scopeT s);
static void outDeclsDefC (mcPretty_pretty p, decl_node n);
static void includeConstType (scopeT s);
static void includeVarProcedure (scopeT s);
static void includeVar (scopeT s);
static void includeExternals (decl_node n);
static void checkSystemInclude (decl_node n);
static void addExported (decl_node n);
static void addExternal (decl_node n);
static void includeDefConstType (decl_node n);
static void runIncludeDefConstType (decl_node n);
static void joinProcedures (decl_node i, decl_node d);
static void includeDefVarProcedure (decl_node n);
static void foreachModuleDo (decl_node n, symbolKey_performOperation p);
static void outDeclsImpC (mcPretty_pretty p, scopeT s);
static void doStatementSequenceC (mcPretty_pretty p, decl_node s);
static unsigned int isStatementSequenceEmpty (decl_node s);
static unsigned int isSingleStatement (decl_node s);
static void doCommentC (mcPretty_pretty p, decl_node s);
static void doReturnC (mcPretty_pretty p, decl_node s);
static void doAssignmentC (mcPretty_pretty p, decl_node s);
static unsigned int containsStatement (decl_node s);
static void doCompoundStmt (mcPretty_pretty p, decl_node s);
static void doElsifC (mcPretty_pretty p, decl_node s);
static unsigned int noElse (decl_node n);
static void doIfC (mcPretty_pretty p, decl_node s);
static void doForC (mcPretty_pretty p, decl_node s);
static void doRepeatC (mcPretty_pretty p, decl_node s);
static void doWhileC (mcPretty_pretty p, decl_node s);
static void doFuncHighC (mcPretty_pretty p, decl_node a);
static void doMultiplyBySize (mcPretty_pretty p, decl_node a);
static void doTotype (mcPretty_pretty p, decl_node a, decl_node t);
static void doFuncUnbounded (mcPretty_pretty p, decl_node actual, decl_node formal, decl_node func);
static void doProcedureParamC (mcPretty_pretty p, decl_node actual, decl_node formal);
static void doAdrExprC (mcPretty_pretty p, decl_node n);
static unsigned int typePair (decl_node a, decl_node b, decl_node x, decl_node y);
static unsigned int needsCast (decl_node at, decl_node ft);
static void checkSystemCast (mcPretty_pretty p, decl_node actual, decl_node formal);
static void doFuncParamC (mcPretty_pretty p, decl_node actual, decl_node formal, decl_node func);
static decl_node getNthParamType (Indexing_Index l, unsigned int i);
static decl_node getNthParam (Indexing_Index l, unsigned int i);
static void doFuncArgsC (mcPretty_pretty p, decl_node s, Indexing_Index l, unsigned int needParen);
static void doProcTypeArgsC (mcPretty_pretty p, decl_node s, Indexing_Index args, unsigned int needParen);
static void doAdrArgC (mcPretty_pretty p, decl_node n);
static void doAdrC (mcPretty_pretty p, decl_node n);
static void doIncDecC (mcPretty_pretty p, decl_node n, char *op_, unsigned int _op_high);
static void doInclC (mcPretty_pretty p, decl_node n);
static void doExclC (mcPretty_pretty p, decl_node n);
static void doNewC (mcPretty_pretty p, decl_node n);
static void doDisposeC (mcPretty_pretty p, decl_node n);
static void doAbsC (mcPretty_pretty p, decl_node n);
static void doValC (mcPretty_pretty p, decl_node n);
static void doMinC (mcPretty_pretty p, decl_node n);
static void doMaxC (mcPretty_pretty p, decl_node n);
static unsigned int isIntrinsic (decl_node n);
static void doHalt (mcPretty_pretty p, decl_node n);
static void doIntrinsicC (mcPretty_pretty p, decl_node n);
static decl_node getFuncFromExpr (decl_node n);
static void doFuncExprC (mcPretty_pretty p, decl_node n);
static void doFuncCallC (mcPretty_pretty p, decl_node n);
static void doCaseStatementC (mcPretty_pretty p, decl_node n, unsigned int needBreak);
static void doExceptionC (mcPretty_pretty p, char *a_, unsigned int _a_high, decl_node n);
static void doRangeListC (mcPretty_pretty p, decl_node c);
static void doRangeIfListC (mcPretty_pretty p, decl_node e, decl_node c);
static void doCaseLabels (mcPretty_pretty p, decl_node n, unsigned int needBreak);
static void doCaseLabelListC (mcPretty_pretty p, decl_node n, unsigned int haveElse);
static void doCaseIfLabels (mcPretty_pretty p, decl_node e, decl_node n, unsigned int i, unsigned int h);
static void doCaseIfLabelListC (mcPretty_pretty p, decl_node n);
static void doCaseElseC (mcPretty_pretty p, decl_node n);
static void doCaseIfElseC (mcPretty_pretty p, decl_node n);
static unsigned int canUseSwitchCaseLabels (decl_node n);
static unsigned int canUseSwitch (decl_node n);
static void doCaseC (mcPretty_pretty p, decl_node n);
static void doLoopC (mcPretty_pretty p, decl_node s);
static void doExitC (mcPretty_pretty p, decl_node s);
static void doStatementsC (mcPretty_pretty p, decl_node s);
static void stop (void);
static void doLocalVarC (mcPretty_pretty p, scopeT s);
static void doLocalConstTypesC (mcPretty_pretty p, scopeT s);
static void addParamDone (decl_node n);
static void includeParameters (decl_node n);
static void doProcedureC (decl_node n);
static void outProceduresC (mcPretty_pretty p, scopeT s);
static void output (decl_node n, nodeProcedure c, nodeProcedure t, nodeProcedure v);
static dependentState allDependants (decl_node n);
static dependentState walkDependants (alists_alist l, decl_node n);
static dependentState walkType (alists_alist l, decl_node n);
static void db (char *a_, unsigned int _a_high, decl_node n);
static void dbt (char *a_, unsigned int _a_high);
static void dbs (dependentState s, decl_node n);
static void dbq (decl_node n);
static dependentState walkRecord (alists_alist l, decl_node n);
static dependentState walkVarient (alists_alist l, decl_node n);
static void queueBlocked (decl_node n);
static dependentState walkVar (alists_alist l, decl_node n);
static dependentState walkEnumeration (alists_alist l, decl_node n);
static dependentState walkSubrange (alists_alist l, decl_node n);
static dependentState walkSubscript (alists_alist l, decl_node n);
static dependentState walkPointer (alists_alist l, decl_node n);
static dependentState walkArray (alists_alist l, decl_node n);
static dependentState walkConst (alists_alist l, decl_node n);
static dependentState walkVarParam (alists_alist l, decl_node n);
static dependentState walkParam (alists_alist l, decl_node n);
static dependentState walkOptarg (alists_alist l, decl_node n);
static dependentState walkRecordField (alists_alist l, decl_node n);
static dependentState walkVarientField (alists_alist l, decl_node n);
static dependentState walkEnumerationField (alists_alist l, decl_node n);
static dependentState walkSet (alists_alist l, decl_node n);
static dependentState walkProcType (alists_alist l, decl_node n);
static dependentState walkProcedure (alists_alist l, decl_node n);
static dependentState walkParameters (alists_alist l, Indexing_Index p);
static dependentState walkFuncCall (alists_alist l, decl_node n);
static dependentState walkUnary (alists_alist l, decl_node n);
static dependentState walkBinary (alists_alist l, decl_node n);
static dependentState walkComponentRef (alists_alist l, decl_node n);
static dependentState walkPointerRef (alists_alist l, decl_node n);
static dependentState doDependants (alists_alist l, decl_node n);
static unsigned int tryComplete (decl_node n, nodeProcedure c, nodeProcedure t, nodeProcedure v);
static unsigned int tryCompleteFromPartial (decl_node n, nodeProcedure t);
static void visitUnary (alists_alist v, decl_node n, nodeProcedure p);
static void visitBinary (alists_alist v, decl_node n, nodeProcedure p);
static void visitBoolean (alists_alist v, decl_node n, nodeProcedure p);
static void visitScope (alists_alist v, decl_node n, nodeProcedure p);
static void visitType (alists_alist v, decl_node n, nodeProcedure p);
static void visitIndex (alists_alist v, Indexing_Index i, nodeProcedure p);
static void visitRecord (alists_alist v, decl_node n, nodeProcedure p);
static void visitVarient (alists_alist v, decl_node n, nodeProcedure p);
static void visitVar (alists_alist v, decl_node n, nodeProcedure p);
static void visitEnumeration (alists_alist v, decl_node n, nodeProcedure p);
static void visitSubrange (alists_alist v, decl_node n, nodeProcedure p);
static void visitPointer (alists_alist v, decl_node n, nodeProcedure p);
static void visitArray (alists_alist v, decl_node n, nodeProcedure p);
static void visitConst (alists_alist v, decl_node n, nodeProcedure p);
static void visitVarParam (alists_alist v, decl_node n, nodeProcedure p);
static void visitParam (alists_alist v, decl_node n, nodeProcedure p);
static void visitOptarg (alists_alist v, decl_node n, nodeProcedure p);
static void visitRecordField (alists_alist v, decl_node n, nodeProcedure p);
static void visitVarientField (alists_alist v, decl_node n, nodeProcedure p);
static void visitEnumerationField (alists_alist v, decl_node n, nodeProcedure p);
static void visitSet (alists_alist v, decl_node n, nodeProcedure p);
static void visitProcType (alists_alist v, decl_node n, nodeProcedure p);
static void visitSubscript (alists_alist v, decl_node n, nodeProcedure p);
static void visitDecls (alists_alist v, scopeT s, nodeProcedure p);
static void visitProcedure (alists_alist v, decl_node n, nodeProcedure p);
static void visitDef (alists_alist v, decl_node n, nodeProcedure p);
static void visitImp (alists_alist v, decl_node n, nodeProcedure p);
static void visitModule (alists_alist v, decl_node n, nodeProcedure p);
static void visitLoop (alists_alist v, decl_node n, nodeProcedure p);
static void visitWhile (alists_alist v, decl_node n, nodeProcedure p);
static void visitRepeat (alists_alist v, decl_node n, nodeProcedure p);
static void visitCase (alists_alist v, decl_node n, nodeProcedure p);
static void visitCaseLabelList (alists_alist v, decl_node n, nodeProcedure p);
static void visitCaseList (alists_alist v, decl_node n, nodeProcedure p);
static void visitRange (alists_alist v, decl_node n, nodeProcedure p);
static void visitIf (alists_alist v, decl_node n, nodeProcedure p);
static void visitElsif (alists_alist v, decl_node n, nodeProcedure p);
static void visitFor (alists_alist v, decl_node n, nodeProcedure p);
static void visitAssignment (alists_alist v, decl_node n, nodeProcedure p);
static void visitComponentRef (alists_alist v, decl_node n, nodeProcedure p);
static void visitPointerRef (alists_alist v, decl_node n, nodeProcedure p);
static void visitArrayRef (alists_alist v, decl_node n, nodeProcedure p);
static void visitFunccall (alists_alist v, decl_node n, nodeProcedure p);
static void visitVarDecl (alists_alist v, decl_node n, nodeProcedure p);
static void visitExplist (alists_alist v, decl_node n, nodeProcedure p);
static void visitExit (alists_alist v, decl_node n, nodeProcedure p);
static void visitReturn (alists_alist v, decl_node n, nodeProcedure p);
static void visitStmtSeq (alists_alist v, decl_node n, nodeProcedure p);
static void visitVarargs (alists_alist v, decl_node n, nodeProcedure p);
static void visitSetValue (alists_alist v, decl_node n, nodeProcedure p);
static void visitDependants (alists_alist v, decl_node n, nodeProcedure p);
static void visitNode (alists_alist v, decl_node n, nodeProcedure p);
static DynamicStrings_String genKind (decl_node n);
static DynamicStrings_String gen (decl_node n);
static void dumpQ (char *q_, unsigned int _q_high, alists_alist l);
static void dumpLists (void);
static void outputHidden (decl_node n);
static void outputHiddenComplete (decl_node n);
static unsigned int tryPartial (decl_node n, nodeProcedure pt);
static void outputPartial (decl_node n);
static void tryOutputTodo (nodeProcedure c, nodeProcedure t, nodeProcedure v, nodeProcedure pt);
static void tryOutputPartial (nodeProcedure t);
static void debugList (char *a_, unsigned int _a_high, alists_alist l);
static void debugLists (void);
static void addEnumConst (decl_node n);
static void populateTodo (nodeProcedure p);
static void topologicallyOut (nodeProcedure c, nodeProcedure t, nodeProcedure v, nodeProcedure tp, nodeProcedure pc, nodeProcedure pt, nodeProcedure pv);
static void outImpInitC (mcPretty_pretty p, decl_node n);
static void runSimplifyTypes (decl_node n);
static void outDefC (mcPretty_pretty p, decl_node n);
static void runPrototypeExported (decl_node n);
static void runPrototypeDefC (decl_node n);
static void outImpC (mcPretty_pretty p, decl_node n);
static void outDeclsModuleC (mcPretty_pretty p, scopeT s);
static void outModuleInitC (mcPretty_pretty p, decl_node n);
static void outModuleC (mcPretty_pretty p, decl_node n);
static void outC (mcPretty_pretty p, decl_node n);
static void outCP (mcPretty_pretty p, decl_node n);
static void doIncludeM2 (decl_node n);
static void doConstM2 (decl_node n);
static void doProcTypeM2 (mcPretty_pretty p, decl_node n);
static void doRecordFieldM2 (mcPretty_pretty p, decl_node f);
static void doVarientFieldM2 (mcPretty_pretty p, decl_node n);
static void doVarientM2 (mcPretty_pretty p, decl_node n);
static void doRecordM2 (mcPretty_pretty p, decl_node n);
static void doPointerM2 (mcPretty_pretty p, decl_node n);
static void doTypeAliasM2 (mcPretty_pretty p, decl_node n);
static void doEnumerationM2 (mcPretty_pretty p, decl_node n);
static void doBaseM2 (mcPretty_pretty p, decl_node n);
static void doSystemM2 (mcPretty_pretty p, decl_node n);
static void doTypeM2 (mcPretty_pretty p, decl_node n);
static void doTypesM2 (decl_node n);
static void doVarM2 (decl_node n);
static void doVarsM2 (decl_node n);
static void doTypeNameM2 (mcPretty_pretty p, decl_node n);
static void doParamM2 (mcPretty_pretty p, decl_node n);
static void doVarParamM2 (mcPretty_pretty p, decl_node n);
static void doParameterM2 (mcPretty_pretty p, decl_node n);
static void doPrototypeM2 (decl_node n);
static void outputPartialM2 (decl_node n);
static void outDeclsDefM2 (mcPretty_pretty p, scopeT s);
static void outDefM2 (mcPretty_pretty p, decl_node n);
static void outDeclsImpM2 (mcPretty_pretty p, scopeT s);
static void outImpM2 (mcPretty_pretty p, decl_node n);
static void outModuleM2 (mcPretty_pretty p, decl_node n);
static void outM2 (mcPretty_pretty p, decl_node n);
static void addDone (decl_node n);
static void addDoneDef (decl_node n);
static decl_node dbgAdd (alists_alist l, decl_node n);
static void dbgType (alists_alist l, decl_node n);
static void dbgPointer (alists_alist l, decl_node n);
static void dbgRecord (alists_alist l, decl_node n);
static void dbgVarient (alists_alist l, decl_node n);
static void dbgEnumeration (alists_alist l, decl_node n);
static void dbgVar (alists_alist l, decl_node n);
static void dbgSubrange (alists_alist l, decl_node n);
static void dbgArray (alists_alist l, decl_node n);
static void doDbg (alists_alist l, decl_node n);
static void dbg (decl_node n);
static unsigned int isAssignment (decl_node n);
static unsigned int isComment (decl_node n);
static decl_node dupExplist (decl_node n);
static decl_node dupArrayref (decl_node n);
static decl_node dupPointerref (decl_node n);
static decl_node dupComponentref (decl_node n);
static decl_node dupBinary (decl_node n);
static decl_node dupUnary (decl_node n);
static decl_node dupFunccall (decl_node n);
static decl_node dupSetValue (decl_node n);
static void makeSystem (void);
static void makeM2rts (void);
static decl_node makeBitnum (void);
static void makeBaseSymbols (void);
static void makeBuiltins (void);
static void init (void);

static decl_node newNode (nodeT k)
{
  decl_node d;

  Storage_ALLOCATE ((void **) &d, sizeof (_T1));
  if (d == NULL)
    M2RTS_HALT (0);
  else
    {
      d->kind = k;
      return d;
    }
}

static void disposeNode (decl_node *n)
{
  Storage_DEALLOCATE ((void **) &(*n), sizeof (_T1));
  (*n) = NULL;
}

static unsigned int isLocal (decl_node n)
{
  decl_node s;

  s = decl_getScope (n);
  if (s != NULL)
    return decl_isProcedure (s);
  return FALSE;
}

static void importEnumFields (decl_node m, decl_node n)
{
  decl_node r;
  decl_node e;
  unsigned int i;
  unsigned int h;

  mcDebug_assert (((decl_isDef (m)) || (decl_isModule (m))) || (decl_isImp (m)));
  n = decl_skipType (n);
  if ((n != NULL) && (decl_isEnumeration (n)))
    {
      i = Indexing_LowIndice (n->enumerationF.listOfSons);
      h = Indexing_HighIndice (n->enumerationF.listOfSons);
      while (i <= h)
        {
          e = Indexing_GetIndice (n->enumerationF.listOfSons, i);
          r = decl_import (m, e);
          if (e != r)
            mcMetaError_metaError2 ((char *) "enumeration field {%1ad} cannot be imported implicitly into {%2d} due to a name clash", 85, (unsigned char *) &e, sizeof (e), (unsigned char *) &m, sizeof (m));
          i += 1;
        }
    }
}

static fixupInfo initFixupInfo (void)
{
  fixupInfo f;

  f.count = 0;
  f.info = Indexing_InitIndex (1);
  return f;
}

static decl_node makeDef (nameKey_Name n)
{
  decl_node d;

  d = newNode ((nodeT) def);
  d->defF.name = n;
  d->defF.source = nameKey_NulName;
  d->defF.hasHidden = FALSE;
  d->defF.forC = FALSE;
  d->defF.exported = Indexing_InitIndex (1);
  d->defF.importedModules = Indexing_InitIndex (1);
  d->defF.constFixup = initFixupInfo ();
  d->defF.enumFixup = initFixupInfo ();
  initDecls (&d->defF.decls);
  d->defF.enumsComplete = FALSE;
  d->defF.constsComplete = FALSE;
  d->defF.visited = FALSE;
  return d;
}

static decl_node makeImp (nameKey_Name n)
{
  decl_node d;

  d = newNode ((nodeT) imp);
  d->impF.name = n;
  d->impF.source = nameKey_NulName;
  d->impF.importedModules = Indexing_InitIndex (1);
  d->impF.constFixup = initFixupInfo ();
  d->impF.enumFixup = initFixupInfo ();
  initDecls (&d->impF.decls);
  d->impF.beginStatements = NULL;
  d->impF.finallyStatements = NULL;
  d->impF.definitionModule = NULL;
  d->impF.enumsComplete = FALSE;
  d->impF.constsComplete = FALSE;
  d->impF.visited = FALSE;
  return d;
}

static decl_node makeModule (nameKey_Name n)
{
  decl_node d;

  d = newNode ((nodeT) module);
  d->moduleF.name = n;
  d->moduleF.source = nameKey_NulName;
  d->moduleF.importedModules = Indexing_InitIndex (1);
  d->moduleF.constFixup = initFixupInfo ();
  d->moduleF.enumFixup = initFixupInfo ();
  initDecls (&d->moduleF.decls);
  d->moduleF.beginStatements = NULL;
  d->moduleF.finallyStatements = NULL;
  d->moduleF.enumsComplete = FALSE;
  d->moduleF.constsComplete = FALSE;
  d->moduleF.visited = FALSE;
  return d;
}

static unsigned int isDefForC (decl_node n)
{
  return (decl_isDef (n)) && n->defF.forC;
}

static void initDecls (scopeT *decls)
{
  (*decls).symbols = symbolKey_initTree ();
  (*decls).constants = Indexing_InitIndex (1);
  (*decls).types = Indexing_InitIndex (1);
  (*decls).procedures = Indexing_InitIndex (1);
  (*decls).variables = Indexing_InitIndex (1);
}

static decl_node addTo (scopeT *decls, decl_node d)
{
  nameKey_Name n;

  n = decl_getSymName (d);
  if (n != nameKey_NulName)
    if ((symbolKey_getSymKey ((*decls).symbols, n)) == NULL)
      symbolKey_putSymKey ((*decls).symbols, n, (void *) d);
    else
      {
        mcMetaError_metaError1 ((char *) "{%1DMad} was declared", 21, (unsigned char *) &d, sizeof (d));
        mcMetaError_metaError1 ((char *) "{%1k} and is being declared again", 33, (unsigned char *) &n, sizeof (n));
      }
  if (decl_isConst (d))
    Indexing_IncludeIndiceIntoIndex ((*decls).constants, (void *) d);
  else if (decl_isVar (d))
    Indexing_IncludeIndiceIntoIndex ((*decls).variables, (void *) d);
  else if (decl_isType (d))
    Indexing_IncludeIndiceIntoIndex ((*decls).types, (void *) d);
  else if (decl_isProcedure (d))
    {
      Indexing_IncludeIndiceIntoIndex ((*decls).procedures, (void *) d);
      if (debugDecl)
        libc_printf ((char *) "%d procedures on the dynamic array\\n", 36, Indexing_HighIndice ((*decls).procedures));
    }
  return d;
}

static void export (decl_node d, decl_node n)
{
  mcDebug_assert (decl_isDef (d));
  Indexing_IncludeIndiceIntoIndex (d->defF.exported, (void *) n);
}

static decl_node addToScope (decl_node n)
{
  decl_node s;
  unsigned int i;

  i = Indexing_HighIndice (scopeStack);
  s = Indexing_GetIndice (scopeStack, i);
  if (decl_isProcedure (s))
    {
      if (debugDecl)
        {
          outText (doP, (char *) "adding ", 7);
          doNameC (doP, n);
          outText (doP, (char *) " to procedure\\n", 15);
        }
      return addTo (&s->procedureF.decls, n);
    }
  else if (decl_isModule (s))
    {
      if (debugDecl)
        {
          outText (doP, (char *) "adding ", 7);
          doNameC (doP, n);
          outText (doP, (char *) " to module\\n", 12);
        }
      return addTo (&s->moduleF.decls, n);
    }
  else if (decl_isDef (s))
    {
      if (debugDecl)
        {
          outText (doP, (char *) "adding ", 7);
          doNameC (doP, n);
          outText (doP, (char *) " to definition module\\n", 23);
        }
      export (s, n);
      return addTo (&s->defF.decls, n);
    }
  else if (decl_isImp (s))
    {
      if (debugDecl)
        {
          outText (doP, (char *) "adding ", 7);
          doNameC (doP, n);
          outText (doP, (char *) " to implementation module\\n", 27);
        }
      return addTo (&s->impF.decls, n);
    }
  M2RTS_HALT (0);
}

static void addModuleToScope (decl_node m, decl_node i)
{
  mcDebug_assert ((decl_getDeclScope ()) == m);
  if ((decl_lookupSym (decl_getSymName (i))) == NULL)
    i = addToScope (i);
}

static void completedEnum (decl_node n)
{
  mcDebug_assert (((decl_isDef (n)) || (decl_isImp (n))) || (decl_isModule (n)));
  if (decl_isDef (n))
    n->defF.enumsComplete = TRUE;
  else if (decl_isImp (n))
    n->impF.enumsComplete = TRUE;
  else if (decl_isModule (n))
    n->moduleF.enumsComplete = TRUE;
}

static void setUnary (decl_node u, nodeT k, decl_node a, decl_node t)
{
  switch (k)
    {
      case constexp:
      case deref:
      case chr:
      case abs_:
      case float_:
      case trunc:
      case ord:
      case high:
      case throw:
      case not:
      case neg:
      case adr:
      case size:
      case tsize:
      case min:
      case max:
        u->kind = k;
        u->unaryF.arg = a;
        u->unaryF.resultType = t;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static void putVarBool (decl_node v, unsigned int init, unsigned int param, unsigned int isvar)
{
  mcDebug_assert (decl_isVar (v));
  v->varF.isInitialised = init;
  v->varF.isParameter = param;
  v->varF.isVarParameter = isvar;
}

static unsigned int isVarDecl (decl_node n)
{
  return n->kind == vardecl;
}

static void makeVariablesFromParameters (decl_node proc, decl_node id, decl_node type, unsigned int isvar)
{
  decl_node v;
  unsigned int i;
  unsigned int n;
  nameKey_Name m;
  DynamicStrings_String s;

  mcDebug_assert (decl_isProcedure (proc));
  mcDebug_assert (isIdentList (id));
  i = 1;
  n = wlists_noOfItemsInList (id->identlistF.names);
  while (i <= n)
    {
      m = wlists_getItemFromList (id->identlistF.names, i);
      v = decl_makeVar (m);
      decl_putVar (v, type, (decl_node) NULL);
      putVarBool (v, TRUE, TRUE, isvar);
      if (debugScopes)
        {
          libc_printf ((char *) "adding parameter variable into top scope\\n", 42);
          dumpScopes ();
          libc_printf ((char *) " variable name is: ", 19);
          s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (m));
          if ((DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, s))) == NULL)
            ;  /* empty.  */
          libc_printf ((char *) "\\n", 2);
        }
      i += 1;
    }
}

static decl_node addProcedureToScope (decl_node d, nameKey_Name n)
{
  decl_node m;
  unsigned int i;

  i = Indexing_HighIndice (scopeStack);
  m = Indexing_GetIndice (scopeStack, i);
  if (((decl_isDef (m)) && ((decl_getSymName (m)) == (nameKey_makeKey ((char *) "M2RTS", 5)))) && ((decl_getSymName (d)) == (nameKey_makeKey ((char *) "HALT", 4))))
    {
      haltN = d;
      symbolKey_putSymKey (baseSymbols, n, (void *) haltN);
    }
  return addToScope (d);
}

static void putProcTypeReturn (decl_node proc, decl_node type)
{
  proc->proctypeF.returnType = type;
}

static decl_node makeOptParameter (decl_node l, decl_node type, decl_node init)
{
  decl_node n;

  n = newNode ((nodeT) optarg_);
  n->optargF.namelist = l;
  n->optargF.type = type;
  n->optargF.init = init;
  n->optargF.scope = NULL;
  return n;
}

static unsigned int setwatch (decl_node n)
{
  globalNode = n;
  return TRUE;
}

static unsigned int runwatch (void)
{
  return globalNode->kind == identlist;
}

static unsigned int isIdentList (decl_node n)
{
  return n->kind == identlist;
}

static unsigned int identListLen (decl_node n)
{
  if (n == NULL)
    return 0;
  else
    {
      mcDebug_assert (isIdentList (n));
      return wlists_noOfItemsInList (n->identlistF.names);
    }
}

static void checkParameters (decl_node p, decl_node i, decl_node type, unsigned int var)
{
  disposeNode (&i);
}

static void checkMakeVariables (decl_node n, decl_node i, decl_node type, unsigned int isvar)
{
  if (((decl_isImp (currentModule)) || (decl_isModule (currentModule))) && ! n->procedureF.built)
    makeVariablesFromParameters (n, i, type, isvar);
}

static decl_node makeVarientField (decl_node v, decl_node p)
{
  decl_node n;

  n = newNode ((nodeT) varientfield);
  n->varientfieldF.name = nameKey_NulName;
  n->varientfieldF.parent = p;
  n->varientfieldF.varient = v;
  n->varientfieldF.simple = FALSE;
  n->varientfieldF.listOfSons = Indexing_InitIndex (1);
  n->varientfieldF.scope = decl_getDeclScope ();
  return n;
}

static void putFieldVarient (decl_node f, decl_node v)
{
  mcDebug_assert (decl_isVarient (v));
  mcDebug_assert (decl_isVarientField (f));
  switch (v->kind)
    {
      case varient:
        Indexing_IncludeIndiceIntoIndex (v->varientF.listOfSons, (void *) f);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  switch (f->kind)
    {
      case varientfield:
        f->varientfieldF.varient = v;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static decl_node putFieldRecord (decl_node r, nameKey_Name tag, decl_node type, decl_node v)
{
  decl_node f;
  decl_node n;
  decl_node p;

  n = newNode ((nodeT) recordfield);
  switch (r->kind)
    {
      case record:
        Indexing_IncludeIndiceIntoIndex (r->recordF.listOfSons, (void *) n);
        if (tag != nameKey_NulName)
          if ((symbolKey_getSymKey (r->recordF.localSymbols, tag)) == nameKey_NulName)
            symbolKey_putSymKey (r->recordF.localSymbols, tag, (void *) n);
          else
            {
              f = symbolKey_getSymKey (r->recordF.localSymbols, tag);
              mcMetaError_metaErrors1 ((char *) "field record {%1Dad} has already been declared", 46, (char *) "field record duplicate", 22, (unsigned char *) &f, sizeof (f));
            }
        break;

      case varientfield:
        Indexing_IncludeIndiceIntoIndex (r->varientfieldF.listOfSons, (void *) n);
        p = getParent (r);
        mcDebug_assert (p->kind == record);
        if (tag != nameKey_NulName)
          symbolKey_putSymKey (p->recordF.localSymbols, tag, (void *) n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  n->recordfieldF.type = type;
  n->recordfieldF.name = tag;
  n->recordfieldF.parent = r;
  n->recordfieldF.varient = v;
  n->recordfieldF.tag = FALSE;
  initCname (&n->recordfieldF.cname);
  return n;
}

static void ensureOrder (Indexing_Index i, decl_node a, decl_node b)
{
  mcDebug_assert (Indexing_IsIndiceInIndex (i, (void *) a));
  mcDebug_assert (Indexing_IsIndiceInIndex (i, (void *) b));
  Indexing_RemoveIndiceFromIndex (i, (void *) a);
  Indexing_RemoveIndiceFromIndex (i, (void *) b);
  Indexing_IncludeIndiceIntoIndex (i, (void *) a);
  Indexing_IncludeIndiceIntoIndex (i, (void *) b);
  mcDebug_assert (Indexing_IsIndiceInIndex (i, (void *) a));
  mcDebug_assert (Indexing_IsIndiceInIndex (i, (void *) b));
}

static void putVarientTag (decl_node v, decl_node tag)
{
  decl_node p;

  mcDebug_assert (decl_isVarient (v));
  switch (v->kind)
    {
      case varient:
        v->varientF.tag = tag;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static decl_node getParent (decl_node n)
{
  switch (n->kind)
    {
      case recordfield:
        return n->recordfieldF.parent;
        break;

      case varientfield:
        return n->varientfieldF.parent;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static decl_node getRecord (decl_node n)
{
  mcDebug_assert (n->kind != varient);
  switch (n->kind)
    {
      case record:
        return n;
        break;

      case varientfield:
        return getRecord (getParent (n));
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static unsigned int isConstExp (decl_node c)
{
  mcDebug_assert (c != NULL);
  return c->kind == constexp;
}

static void addEnumToModule (decl_node m, decl_node e)
{
  mcDebug_assert ((decl_isEnumeration (e)) || (decl_isEnumerationField (e)));
  mcDebug_assert (((decl_isModule (m)) || (decl_isDef (m))) || (decl_isImp (m)));
  if (decl_isModule (m))
    Indexing_IncludeIndiceIntoIndex (m->moduleF.enumFixup.info, (void *) e);
  else if (decl_isDef (m))
    Indexing_IncludeIndiceIntoIndex (m->defF.enumFixup.info, (void *) e);
  else if (decl_isImp (m))
    Indexing_IncludeIndiceIntoIndex (m->impF.enumFixup.info, (void *) e);
}

static decl_node getNextFixup (fixupInfo *f)
{
  (*f).count += 1;
  return Indexing_GetIndice ((*f).info, (*f).count);
}

static decl_node doMakeEnum (void)
{
  decl_node e;

  e = newNode ((nodeT) enumeration);
  e->enumerationF.noOfElements = 0;
  e->enumerationF.localSymbols = symbolKey_initTree ();
  e->enumerationF.scope = decl_getDeclScope ();
  e->enumerationF.listOfSons = Indexing_InitIndex (1);
  e->enumerationF.low = NULL;
  e->enumerationF.high = NULL;
  addEnumToModule (currentModule, e);
  return e;
}

static decl_node doMakeEnumField (decl_node e, nameKey_Name n)
{
  decl_node f;

  mcDebug_assert (decl_isEnumeration (e));
  f = decl_lookupSym (n);
  if (f == NULL)
    {
      f = newNode ((nodeT) enumerationfield);
      symbolKey_putSymKey (e->enumerationF.localSymbols, n, (void *) f);
      Indexing_IncludeIndiceIntoIndex (e->enumerationF.listOfSons, (void *) f);
      f->enumerationfieldF.name = n;
      f->enumerationfieldF.type = e;
      f->enumerationfieldF.scope = decl_getDeclScope ();
      f->enumerationfieldF.value = e->enumerationF.noOfElements;
      initCname (&f->enumerationfieldF.cname);
      e->enumerationF.noOfElements += 1;
      mcDebug_assert ((Indexing_GetIndice (e->enumerationF.listOfSons, e->enumerationF.noOfElements)) == f);
      addEnumToModule (currentModule, f);
      if (e->enumerationF.low == NULL)
        e->enumerationF.low = f;
      e->enumerationF.high = f;
      return addToScope (f);
    }
  else
    mcMetaError_metaErrors2 ((char *) "cannot create enumeration field {%1k} as the name is already in use", 67, (char *) "{%2DMad} was declared elsewhere", 31, (unsigned char *) &n, sizeof (n), (unsigned char *) &f, sizeof (f));
}

static decl_node getExpList (decl_node p, unsigned int n)
{
  mcDebug_assert (p != NULL);
  mcDebug_assert (decl_isExpList (p));
  mcDebug_assert (n <= (Indexing_HighIndice (p->explistF.exp)));
  return Indexing_GetIndice (p->explistF.exp, n);
}

static unsigned int expListLen (decl_node p)
{
  mcDebug_assert (p != NULL);
  mcDebug_assert (decl_isExpList (p));
  return Indexing_HighIndice (p->explistF.exp);
}

static unsigned int getConstExpComplete (decl_node n)
{
  switch (n->kind)
    {
      case def:
        return n->defF.constsComplete;
        break;

      case imp:
        return n->impF.constsComplete;
        break;

      case module:
        return n->moduleF.constsComplete;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static void addConstToModule (decl_node m, decl_node e)
{
  mcDebug_assert (((decl_isModule (m)) || (decl_isDef (m))) || (decl_isImp (m)));
  if (decl_isModule (m))
    Indexing_IncludeIndiceIntoIndex (m->moduleF.constFixup.info, (void *) e);
  else if (decl_isDef (m))
    Indexing_IncludeIndiceIntoIndex (m->defF.constFixup.info, (void *) e);
  else if (decl_isImp (m))
    Indexing_IncludeIndiceIntoIndex (m->impF.constFixup.info, (void *) e);
}

static decl_node doMakeConstExp (void)
{
  decl_node c;

  c = makeUnary ((nodeT) constexp, (decl_node) NULL, (decl_node) NULL);
  addConstToModule (currentModule, c);
  return c;
}

static unsigned int isAnyType (decl_node n)
{
  mcDebug_assert (n != NULL);
  switch (n->kind)
    {
      case address:
      case loc:
      case byte:
      case word:
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case bitset:
      case boolean:
      case proc:
      case type:
        return TRUE;
        break;


      default:
        return FALSE;
        break;
    }
}

static decl_node makeCast (decl_node c, decl_node p)
{
  mcDebug_assert (decl_isExpList (p));
  if ((expListLen (p)) == 1)
    return makeBinary ((nodeT) cast, c, getExpList (p, 1), c);
  else
    M2RTS_HALT (0);
}

static unsigned int isFuncCall (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == funccall;
}

static void putTypeInternal (decl_node des)
{
  mcDebug_assert (des != NULL);
  mcDebug_assert (decl_isType (des));
  des->typeF.isInternal = TRUE;
}

static unsigned int isTypeInternal (decl_node n)
{
  mcDebug_assert (n != NULL);
  mcDebug_assert (decl_isType (n));
  return n->typeF.isInternal;
}

static decl_node lookupBase (nameKey_Name n)
{
  decl_node m;

  m = symbolKey_getSymKey (baseSymbols, n);
  if (m == procN)
    keyc_useProc ();
  return m;
}

static void dumpScopes (void)
{
  unsigned int h;
  decl_node s;

  h = Indexing_HighIndice (scopeStack);
  libc_printf ((char *) "total scopes stacked %d\\n", 25, h);
  while (h >= 1)
    {
      s = Indexing_GetIndice (scopeStack, h);
      out2 ((char *) " scope [%d] is %s\\n", 19, h, s);
      h -= 1;
    }
}

static void out0 (char *a_, unsigned int _a_high)
{
  DynamicStrings_String m;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  m = FormatStrings_Sprintf0 (DynamicStrings_InitString ((char *) a, _a_high));
  m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
}

static void out1 (char *a_, unsigned int _a_high, decl_node s)
{
  DynamicStrings_String m;
  unsigned int d;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  m = getFQstring (s);
  if (DynamicStrings_EqualArray (m, (char *) "", 0))
    {
      d = (unsigned int ) ((long unsigned int ) (s));
      m = DynamicStrings_KillString (m);
      m = FormatStrings_Sprintf1 (DynamicStrings_InitString ((char *) "[%d]", 4), (unsigned char *) &d, sizeof (d));
    }
  m = FormatStrings_Sprintf1 (DynamicStrings_InitString ((char *) a, _a_high), (unsigned char *) &m, sizeof (m));
  m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
}

static void out2 (char *a_, unsigned int _a_high, unsigned int c, decl_node s)
{
  DynamicStrings_String m;
  DynamicStrings_String m1;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  m1 = getString (s);
  m = FormatStrings_Sprintf2 (DynamicStrings_InitString ((char *) a, _a_high), (unsigned char *) &c, sizeof (c), (unsigned char *) &m1, sizeof (m1));
  m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
  m1 = DynamicStrings_KillString (m1);
}

static void out3 (char *a_, unsigned int _a_high, unsigned int l, nameKey_Name n, decl_node s)
{
  DynamicStrings_String m;
  DynamicStrings_String m1;
  DynamicStrings_String m2;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  m1 = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  m2 = getString (s);
  m = FormatStrings_Sprintf3 (DynamicStrings_InitString ((char *) a, _a_high), (unsigned char *) &l, sizeof (l), (unsigned char *) &m1, sizeof (m1), (unsigned char *) &m2, sizeof (m2));
  m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
  m1 = DynamicStrings_KillString (m1);
  m2 = DynamicStrings_KillString (m2);
}

static unsigned int isUnary (decl_node n)
{
  mcDebug_assert (n != NULL);
  switch (n->kind)
    {
      case throw:
      case deref:
      case high:
      case chr:
      case abs_:
      case ord:
      case float_:
      case trunc:
      case constexp:
      case not:
      case neg:
      case adr:
      case size:
      case tsize:
      case min:
      case max:
        return TRUE;
        break;


      default:
        return FALSE;
        break;
    }
}

static decl_node makeUnary (nodeT k, decl_node e, decl_node res)
{
  decl_node n;

  Storage_ALLOCATE ((void **) &n, sizeof (_T1));
  n->kind = k;
  switch (n->kind)
    {
      case throw:
      case deref:
      case high:
      case chr:
      case abs_:
      case ord:
      case float_:
      case trunc:
      case constexp:
      case not:
      case neg:
      case adr:
      case size:
      case tsize:
        n->unaryF.arg = e;
        n->unaryF.resultType = res;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  return n;
}

static unsigned int isLeafString (decl_node n)
{
  return ((isString (n)) || ((decl_isLiteral (n)) && ((decl_getType (n)) == charN))) || ((decl_isConst (n)) && ((getExprType (n)) == charN));
}

static DynamicStrings_String getStringContents (decl_node n)
{
  if (decl_isConst (n))
    return getStringContents (n->constF.value);
  else if (decl_isLiteral (n))
    {
      M2RTS_HALT (0);
      return NULL;
    }
  else if (isString (n))
    return getString (n);
  else if (isConstExp (n))
    return getStringContents (n->unaryF.arg);
  M2RTS_HALT (0);
}

static decl_node foldBinary (nodeT k, decl_node l, decl_node r, decl_node res)
{
  decl_node n;
  DynamicStrings_String ls;
  DynamicStrings_String rs;

  n = NULL;
  if (((k == plus) && (isLeafString (l))) && (isLeafString (r)))
    {
      ls = getStringContents (l);
      rs = getStringContents (r);
      ls = DynamicStrings_Add (ls, rs);
      n = decl_makeString (nameKey_makekey (DynamicStrings_string (ls)));
      ls = DynamicStrings_KillString (ls);
      rs = DynamicStrings_KillString (rs);
    }
  return n;
}

static decl_node makeBinary (nodeT k, decl_node l, decl_node r, decl_node res)
{
  decl_node n;

  n = foldBinary (k, l, r, res);
  if (n == NULL)
    n = doMakeBinary (k, l, r, res);
  return n;
}

static decl_node doMakeBinary (nodeT k, decl_node l, decl_node r, decl_node res)
{
  decl_node n;

  Storage_ALLOCATE ((void **) &n, sizeof (_T1));
  n->kind = k;
  switch (n->kind)
    {
      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
      case and:
      case or:
      case cast:
      case val:
      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
      case in:
        n->binaryF.left = l;
        n->binaryF.right = r;
        n->binaryF.resultType = res;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  return n;
}

static decl_node doMakeComponentRef (decl_node rec, decl_node field)
{
  decl_node n;

  n = newNode ((nodeT) componentref);
  n->componentrefF.rec = rec;
  n->componentrefF.field = field;
  n->componentrefF.resultType = decl_getType (field);
  return n;
}

static unsigned int isComponentRef (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == componentref;
}

static unsigned int isArrayRef (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == arrayref;
}

static unsigned int isDeref (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == deref;
}

static decl_node makeBase (nodeT k)
{
  decl_node n;

  Storage_ALLOCATE ((void **) &n, sizeof (_T1));
  n->kind = k;
  switch (k)
    {
      case new:
      case dispose:
      case inc:
      case dec:
      case incl:
      case excl:
      case nil:
      case true:
      case false:
      case address:
      case loc:
      case byte:
      case word:
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case boolean:
      case proc:
      case ztype:
      case rtype:
      case adr:
      case chr:
      case abs_:
      case float_:
      case trunc:
      case ord:
      case high:
      case throw:
      case size:
      case tsize:
      case val:
      case min:
      case max:
        break;


      default:
        M2RTS_HALT (0);
        break;
    }
  return n;
}

static unsigned int isOrdinal (decl_node n)
{
  switch (n->kind)
    {
      case address:
      case loc:
      case byte:
      case word:
      case char_:
      case integer:
      case longint:
      case shortint:
      case cardinal:
      case longcard:
      case shortcard:
      case bitset:
        return TRUE;
        break;


      default:
        return FALSE;
        break;
    }
}

static decl_node mixTypes (decl_node a, decl_node b)
{
  if ((a == addressN) || (b == addressN))
    return addressN;
  return a;
}

static decl_node doSetExprType (decl_node *t, decl_node n)
{
  if ((*t) == NULL)
    (*t) = n;
  return (*t);
}

static decl_node getMaxMinType (decl_node n)
{
  if ((decl_isVar (n)) || (decl_isConst (n)))
    return decl_getType (n);
  else
    return n;
}

static decl_node doGetFuncType (decl_node n)
{
  mcDebug_assert (isFuncCall (n));
  if (isIntrinsic (n))
    switch (n->funccallF.function->kind)
      {
        case max:
        case min:
          return getMaxMinType (getExpList (n->funccallF.args, 1));
          break;

        case cast:
        case val:
          return getExpList (n->funccallF.args, 1);
          break;

        case adr:
          return addressN;
          break;

        case size:
        case tsize:
        case float_:
          return realN;
          break;

        case trunc:
          return integerN;
          break;

        case ord:
          return cardinalN;
          break;

        case chr:
          return charN;
          break;

        case abs_:
          return getExprType (getExpList (n->funccallF.args, 1));
          break;

        case high:
          return cardinalN;
          break;

        case halt:
        case inc:
        case dec:
        case incl:
        case excl:
        case new:
        case dispose:
          M2RTS_HALT (0);
          break;


        default:
          CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
      }
  else
    return doSetExprType (&n->funccallF.type, decl_getType (n->funccallF.function));
}

static decl_node doGetExprType (decl_node n)
{
  switch (n->kind)
    {
      case halt:
      case new:
      case dispose:
      case inc:
      case dec:
      case incl:
      case excl:
        return NULL;
        break;

      case nil:
        return addressN;
        break;

      case true:
      case false:
        return booleanN;
        break;

      case address:
        return n;
        break;

      case loc:
        return n;
        break;

      case byte:
        return n;
        break;

      case word:
        return n;
        break;

      case boolean:
        return n;
        break;

      case proc:
        return n;
        break;

      case char_:
        return n;
        break;

      case cardinal:
        return n;
        break;

      case longcard:
        return n;
        break;

      case shortcard:
        return n;
        break;

      case integer:
        return n;
        break;

      case longint:
        return n;
        break;

      case shortint:
        return n;
        break;

      case real:
        return n;
        break;

      case longreal:
        return n;
        break;

      case shortreal:
        return n;
        break;

      case bitset:
        return n;
        break;

      case ztype:
        return n;
        break;

      case rtype:
        return n;
        break;

      case type:
        return n->typeF.type;
        break;

      case record:
        return n;
        break;

      case varient:
        return n;
        break;

      case var:
        return n->varF.type;
        break;

      case enumeration:
        return n;
        break;

      case subrange:
        return n->subrangeF.type;
        break;

      case array:
        return n->arrayF.type;
        break;

      case string:
        return charN;
        break;

      case const_:
        return doSetExprType (&n->constF.type, getExprType (n->constF.value));
        break;

      case literal:
        return n->literalF.type;
        break;

      case varparam:
        return n->varparamF.type;
        break;

      case param:
        return n->paramF.type;
        break;

      case optarg_:
        return n->optargF.type;
        break;

      case pointer:
        return n->pointerF.type;
        break;

      case recordfield:
        return n->recordfieldF.type;
        break;

      case varientfield:
        return n;
        break;

      case enumerationfield:
        return n->enumerationfieldF.type;
        break;

      case set:
        return n->setF.type;
        break;

      case proctype:
        return n->proctypeF.returnType;
        break;

      case subscript:
        return n->subscriptF.type;
        break;

      case procedure:
        return n->procedureF.returnType;
        break;

      case throw:
        return NULL;
        break;

      case def:
      case imp:
      case module:
      case loop:
      case while_:
      case for_:
      case repeat:
      case if_:
      case elsif:
      case assignment:
        M2RTS_HALT (0);
        break;

      case cast:
      case val:
        return doSetExprType (&n->binaryF.resultType, n->binaryF.left);
        break;

      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
        return doSetExprType (&n->binaryF.resultType, mixTypes (getExprType (n->binaryF.left), getExprType (n->binaryF.right)));
        break;

      case in:
      case and:
      case or:
      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
        return doSetExprType (&n->binaryF.resultType, booleanN);
        break;

      case abs_:
      case constexp:
      case deref:
      case neg:
        return doSetExprType (&n->unaryF.resultType, getExprType (n->unaryF.arg));
        break;

      case adr:
        return doSetExprType (&n->unaryF.resultType, addressN);
        break;

      case size:
      case tsize:
        return doSetExprType (&n->unaryF.resultType, cardinalN);
        break;

      case high:
      case ord:
        return doSetExprType (&n->unaryF.resultType, cardinalN);
        break;

      case float_:
        return doSetExprType (&n->unaryF.resultType, realN);
        break;

      case trunc:
        return doSetExprType (&n->unaryF.resultType, integerN);
        break;

      case chr:
        return doSetExprType (&n->unaryF.resultType, charN);
        break;

      case not:
        return doSetExprType (&n->unaryF.resultType, booleanN);
        break;

      case arrayref:
        return n->arrayrefF.resultType;
        break;

      case componentref:
        return n->componentrefF.resultType;
        break;

      case pointerref:
        return n->pointerrefF.resultType;
        break;

      case funccall:
        return doSetExprType (&n->funccallF.type, doGetFuncType (n));
        break;

      case setvalue:
        return n->setvalueF.type;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  M2RTS_HALT (0);
}

static decl_node getExprType (decl_node n)
{
  decl_node t;

  t = decl_getType (n);
  if (t == NULL)
    t = doGetExprType (n);
  return t;
}

static void openOutput (void)
{
  DynamicStrings_String s;

  s = mcOptions_getOutputFile ();
  if (DynamicStrings_EqualArray (s, (char *) "-", 1))
    outputFile = FIO_StdOut;
  else
    outputFile = SFIO_OpenToWrite (s);
  mcStream_setDest (outputFile);
}

static void closeOutput (void)
{
  DynamicStrings_String s;

  s = mcOptions_getOutputFile ();
  outputFile = mcStream_combine ();
  if (! (DynamicStrings_EqualArray (s, (char *) "-", 1)))
    FIO_Close (outputFile);
}

static void write (char ch)
{
  FIO_WriteChar (outputFile, ch);
  FIO_FlushBuffer (outputFile);
}

static void writeln (void)
{
  FIO_WriteLine (outputFile);
  FIO_FlushBuffer (outputFile);
}

static void doIncludeC (decl_node n)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  if (isDefForC (n))
    {
      mcPretty_print (doP, (char *) "#   include \"mc-", 16);
      mcPretty_prints (doP, s);
      mcPretty_print (doP, (char *) ".h\"\\n", 5);
      symbolKey_foreachNodeDo (n->defF.decls.symbols, (symbolKey_performOperation) {(symbolKey_performOperation_t) addDoneDef});
    }
  else if (mcOptions_getExtendedOpaque ())
    ;  /* empty.  */
  else if (decl_isDef (n))
    {
      mcPretty_print (doP, (char *) "#   include \"", 13);
      mcPretty_prints (doP, mcOptions_getHPrefix ());
      mcPretty_prints (doP, s);
      mcPretty_print (doP, (char *) ".h\"\\n", 5);
      symbolKey_foreachNodeDo (n->defF.decls.symbols, (symbolKey_performOperation) {(symbolKey_performOperation_t) addDoneDef});
    }
  s = DynamicStrings_KillString (s);
}

static decl_node getSymScope (decl_node n)
{
  switch (n->kind)
    {
      case const_:
        return n->constF.scope;
        break;

      case type:
        return n->typeF.scope;
        break;

      case var:
        return n->varF.scope;
        break;

      case procedure:
        return n->procedureF.scope;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  M2RTS_HALT (0);
}

static DynamicStrings_String getFQstring (decl_node n)
{
  DynamicStrings_String i;
  DynamicStrings_String s;

  if (((! (decl_isExported (n))) || (mcOptions_getIgnoreFQ ())) || (isDefForC (decl_getScope (n))))
    return DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  else
    {
      i = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
      s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (decl_getScope (n))));
      return FormatStrings_Sprintf2 (DynamicStrings_InitString ((char *) "%s_%s", 5), (unsigned char *) &s, sizeof (s), (unsigned char *) &i, sizeof (i));
    }
}

static DynamicStrings_String getFQDstring (decl_node n, unsigned int scopes)
{
  DynamicStrings_String i;
  DynamicStrings_String s;

  if ((! (decl_isExported (n))) || (mcOptions_getIgnoreFQ ()))
    return DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (getDName (n, scopes)));
  else
    {
      i = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
      s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (decl_getScope (n))));
      return FormatStrings_Sprintf2 (DynamicStrings_InitString ((char *) "%s_%s", 5), (unsigned char *) &s, sizeof (s), (unsigned char *) &i, sizeof (i));
    }
}

static DynamicStrings_String getString (decl_node n)
{
  if ((decl_getSymName (n)) == nameKey_NulName)
    return DynamicStrings_InitString ((char *) "", 0);
  else
    return DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
}

static void doNone (decl_node n)
{
  M2RTS_HALT (0);
}

static void doNothing (decl_node n)
{
}

static void doConstC (decl_node n)
{
  if (! (alists_isItemInList (doneQ, (void *) n)))
    {
      mcPretty_print (doP, (char *) "#   define ", 11);
      doFQNameC (doP, n);
      mcPretty_setNeedSpace (doP);
      doExprC (doP, n->constF.value);
      mcPretty_print (doP, (char *) "\\n", 2);
      alists_includeItemIntoList (doneQ, (void *) n);
    }
}

static unsigned int needsParen (decl_node n)
{
  mcDebug_assert (n != NULL);
  switch (n->kind)
    {
      case nil:
      case true:
      case false:
        return FALSE;
        break;

      case constexp:
        return needsParen (n->unaryF.arg);
        break;

      case neg:
        return needsParen (n->unaryF.arg);
        break;

      case not:
        return needsParen (n->unaryF.arg);
        break;

      case adr:
      case size:
      case tsize:
      case ord:
      case float_:
      case trunc:
      case chr:
      case high:
        return FALSE;
        break;

      case deref:
        return FALSE;
        break;

      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
        return TRUE;
        break;

      case componentref:
        return FALSE;
        break;

      case pointerref:
        return FALSE;
        break;

      case cast:
        return TRUE;
        break;

      case val:
        return TRUE;
        break;

      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
      case in:
        return TRUE;
        break;

      case literal:
      case const_:
      case enumerationfield:
      case string:
        return FALSE;
        break;

      case max:
        return TRUE;
        break;

      case min:
        return TRUE;
        break;

      case var:
        return FALSE;
        break;

      case arrayref:
        return FALSE;
        break;

      case and:
      case or:
        return TRUE;
        break;

      case funccall:
        return TRUE;
        break;

      case recordfield:
        return FALSE;
        break;

      case type:
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case boolean:
      case proc:
        return FALSE;
        break;

      case setvalue:
        return FALSE;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  return TRUE;
}

static void doUnary (mcPretty_pretty p, char *op_, unsigned int _op_high, decl_node expr, decl_node type, unsigned int l, unsigned int r)
{
  char op[_op_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (op, op_, _op_high+1);

  if (l)
    mcPretty_setNeedSpace (p);
  mcPretty_print (p, (char *) op, _op_high);
  if (r)
    mcPretty_setNeedSpace (p);
  if (needsParen (expr))
    {
      outText (p, (char *) "(", 1);
      doExprC (p, expr);
      outText (p, (char *) ")", 1);
    }
  else
    doExprC (p, expr);
}

static void doSetSub (mcPretty_pretty p, decl_node left, decl_node right)
{
  if (needsParen (left))
    {
      outText (p, (char *) "(", 1);
      doExprC (p, left);
      outText (p, (char *) ")", 1);
    }
  else
    doExprC (p, left);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "&", 1);
  mcPretty_setNeedSpace (p);
  if (needsParen (right))
    {
      outText (p, (char *) "(~(", 3);
      doExprC (p, right);
      outText (p, (char *) "))", 2);
    }
  else
    {
      outText (p, (char *) "(~", 2);
      doExprC (p, right);
      outText (p, (char *) ")", 1);
    }
}

static void doPolyBinary (mcPretty_pretty p, nodeT op, decl_node left, decl_node right, unsigned int l, unsigned int r)
{
  decl_node lt;
  decl_node rt;

  lt = decl_skipType (getExprType (left));
  rt = decl_skipType (getExprType (right));
  if (((lt != NULL) && ((decl_isSet (lt)) || (isBitset (lt)))) || ((rt != NULL) && ((decl_isSet (rt)) || (isBitset (rt)))))
    switch (op)
      {
        case plus:
          doBinary (p, (char *) "|", 1, left, right, l, r);
          break;

        case sub:
          doSetSub (p, left, right);
          break;

        case mult:
          doBinary (p, (char *) "&", 1, left, right, l, r);
          break;

        case divide:
          doBinary (p, (char *) "^", 1, left, right, l, r);
          break;


        default:
          CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
      }
  else
    switch (op)
      {
        case plus:
          doBinary (p, (char *) "+", 1, left, right, l, r);
          break;

        case sub:
          doBinary (p, (char *) "-", 1, left, right, l, r);
          break;

        case mult:
          doBinary (p, (char *) "*", 1, left, right, l, r);
          break;

        case divide:
          doBinary (p, (char *) "/", 1, left, right, l, r);
          break;


        default:
          CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
      }
}

static void doBinary (mcPretty_pretty p, char *op_, unsigned int _op_high, decl_node left, decl_node right, unsigned int l, unsigned int r)
{
  char op[_op_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (op, op_, _op_high+1);

  if (needsParen (left))
    {
      outText (p, (char *) "(", 1);
      doExprC (p, left);
      outText (p, (char *) ")", 1);
    }
  else
    doExprC (p, left);
  if (l)
    mcPretty_setNeedSpace (p);
  outText (p, (char *) op, _op_high);
  if (r)
    mcPretty_setNeedSpace (p);
  if (needsParen (right))
    {
      outText (p, (char *) "(", 1);
      doExprC (p, right);
      outText (p, (char *) ")", 1);
    }
  else
    doExprC (p, right);
}

static void doPostUnary (mcPretty_pretty p, char *op_, unsigned int _op_high, decl_node expr)
{
  char op[_op_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (op, op_, _op_high+1);

  doExprC (p, expr);
  outText (p, (char *) op, _op_high);
}

static void doDeRefC (mcPretty_pretty p, decl_node expr)
{
  outText (p, (char *) "(*", 2);
  doExprC (p, expr);
  outText (p, (char *) ")", 1);
}

static decl_node doGetLastOp (decl_node a, decl_node b)
{
  switch (b->kind)
    {
      case nil:
        return a;
        break;

      case true:
        return a;
        break;

      case false:
        return a;
        break;

      case constexp:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case neg:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case not:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case adr:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case size:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case tsize:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case ord:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case float_:
      case trunc:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case chr:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case high:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case deref:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case equal:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case notequal:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case less:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case greater:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case greequal:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case lessequal:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case componentref:
        return doGetLastOp (b, b->componentrefF.field);
        break;

      case pointerref:
        return doGetLastOp (b, b->pointerrefF.field);
        break;

      case cast:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case val:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case plus:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case sub:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case div_:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case mod:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case mult:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case divide:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case in:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case and:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case or:
        return doGetLastOp (b, b->binaryF.right);
        break;

      case literal:
        return a;
        break;

      case const_:
        return a;
        break;

      case enumerationfield:
        return a;
        break;

      case string:
        return a;
        break;

      case max:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case min:
        return doGetLastOp (b, b->unaryF.arg);
        break;

      case var:
        return a;
        break;

      case arrayref:
        return a;
        break;

      case funccall:
        return a;
        break;

      case procedure:
        return a;
        break;

      case recordfield:
        return a;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static void doComponentRefC (mcPretty_pretty p, decl_node l, decl_node r)
{
  doExprC (p, l);
  outText (p, (char *) ".", 1);
  doExprC (p, r);
}

static void doPointerRefC (mcPretty_pretty p, decl_node l, decl_node r)
{
  doExprC (p, l);
  outText (p, (char *) "->", 2);
  doExprC (p, r);
}

static void doPreBinary (mcPretty_pretty p, char *op_, unsigned int _op_high, decl_node left, decl_node right, unsigned int l, unsigned int r)
{
  char op[_op_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (op, op_, _op_high+1);

  if (l)
    mcPretty_setNeedSpace (p);
  outText (p, (char *) op, _op_high);
  if (r)
    mcPretty_setNeedSpace (p);
  outText (p, (char *) "(", 1);
  doExprC (p, left);
  outText (p, (char *) ",", 1);
  mcPretty_setNeedSpace (p);
  doExprC (p, right);
  outText (p, (char *) ")", 1);
}

static void doConstExpr (mcPretty_pretty p, decl_node n)
{
  doFQNameC (p, n);
}

static void doEnumerationField (mcPretty_pretty p, decl_node n)
{
  doFQDNameC (p, n, FALSE);
}

static unsigned int isZero (decl_node n)
{
  if (isConstExp (n))
    return isZero (n->unaryF.arg);
  return (decl_getSymName (n)) == (nameKey_makeKey ((char *) "0", 1));
}

static void doArrayRef (mcPretty_pretty p, decl_node n)
{
  decl_node t;
  unsigned int i;
  unsigned int c;

  mcDebug_assert (n != NULL);
  mcDebug_assert (isArrayRef (n));
  t = decl_skipType (decl_getType (n->arrayrefF.array));
  if (decl_isUnbounded (t))
    outTextN (p, decl_getSymName (n->arrayrefF.array));
  else
    {
      doExprC (p, n->arrayrefF.array);
      mcDebug_assert (decl_isArray (t));
      outText (p, (char *) ".array", 6);
    }
  outText (p, (char *) "[", 1);
  i = 1;
  c = expListLen (n->arrayrefF.index);
  mcDebug_assert (c == 1);
  while (i <= c)
    {
      doExprC (p, getExpList (n->arrayrefF.index, i));
      if (! (decl_isUnbounded (t)))
        doSubtractC (p, getMin (t->arrayF.subr));
      i += 1;
      if (i < c)
        {
          outText (p, (char *) ",", 1);
          mcPretty_setNeedSpace (p);
        }
    }
  outText (p, (char *) "]", 1);
}

static void doProcedure (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (decl_isProcedure (n));
  doFQNameC (p, n);
}

static void doRecordfield (mcPretty_pretty p, decl_node n)
{
  doDNameC (p, n, FALSE);
}

static void doCastC (mcPretty_pretty p, decl_node t, decl_node e)
{
  decl_node et;

  outText (p, (char *) "(", 1);
  doTypeNameC (p, t);
  outText (p, (char *) ")", 1);
  mcPretty_setNeedSpace (p);
  et = decl_skipType (decl_getType (e));
  if (((et != NULL) && (decl_isProcType (et))) && (decl_isProcType (decl_skipType (t))))
    {
      outText (p, (char *) "{(", 2);
      doFQNameC (p, t);
      outText (p, (char *) "_t)", 3);
      mcPretty_setNeedSpace (p);
      doExprC (p, e);
      outText (p, (char *) ".proc}", 6);
    }
  else
    {
      outText (p, (char *) "(", 1);
      doExprC (p, e);
      outText (p, (char *) ")", 1);
    }
}

static void doSetValueC (mcPretty_pretty p, decl_node n)
{
  decl_node lo;
  unsigned int i;
  unsigned int h;

  mcDebug_assert (decl_isSetValue (n));
  lo = getSetLow (n);
  if (n->setvalueF.type != NULL)
    {
      outText (p, (char *) "(", 1);
      doTypeNameC (p, n->setvalueF.type);
      mcPretty_noSpace (p);
      outText (p, (char *) ")", 1);
      mcPretty_setNeedSpace (p);
    }
  if ((Indexing_HighIndice (n->setvalueF.values)) == 0)
    outText (p, (char *) "0", 1);
  else
    {
      i = Indexing_LowIndice (n->setvalueF.values);
      h = Indexing_HighIndice (n->setvalueF.values);
      outText (p, (char *) "(", 1);
      while (i <= h)
        {
          outText (p, (char *) "(1", 2);
          mcPretty_setNeedSpace (p);
          outText (p, (char *) "<<", 2);
          mcPretty_setNeedSpace (p);
          outText (p, (char *) "(", 1);
          doExprC (p, (decl_node) Indexing_GetIndice (n->setvalueF.values, i));
          doSubtractC (p, lo);
          outText (p, (char *) ")", 1);
          outText (p, (char *) ")", 1);
          if (i < h)
            {
              mcPretty_setNeedSpace (p);
              outText (p, (char *) "|", 1);
              mcPretty_setNeedSpace (p);
            }
          i += 1;
        }
      outText (p, (char *) ")", 1);
    }
}

static decl_node getSetLow (decl_node n)
{
  decl_node type;

  if ((decl_getType (n)) == NULL)
    return decl_makeLiteralInt (nameKey_makeKey ((char *) "0", 1));
  else
    {
      type = decl_skipType (decl_getType (n));
      if (decl_isSet (type))
        return getMin (decl_skipType (decl_getType (type)));
      else
        return decl_makeLiteralInt (nameKey_makeKey ((char *) "0", 1));
    }
}

static void doInC (mcPretty_pretty p, decl_node l, decl_node r)
{
  decl_node lo;

  lo = getSetLow (r);
  outText (p, (char *) "(((1", 4);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "<<", 2);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(", 1);
  doExprC (p, l);
  doSubtractC (p, lo);
  outText (p, (char *) "))", 2);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "&", 1);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(", 1);
  doExprC (p, r);
  outText (p, (char *) "))", 2);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "!=", 2);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "0)", 2);
}

static void doThrowC (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args == NULL)
    M2RTS_HALT (0);
  else
    if ((expListLen (n->funccallF.args)) == 1)
      {
        outText (p, (char *) "throw", 5);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "(", 1);
        doExprC (p, getExpList (n->funccallF.args, 1));
        outText (p, (char *) ")", 1);
      }
    else
      M2RTS_HALT (0);
}

static void outNull (mcPretty_pretty p)
{
  keyc_useNull ();
  outText (p, (char *) "NULL", 4);
}

static void outTrue (mcPretty_pretty p)
{
  keyc_useTrue ();
  outText (p, (char *) "TRUE", 4);
}

static void outFalse (mcPretty_pretty p)
{
  keyc_useFalse ();
  outText (p, (char *) "FALSE", 5);
}

static void doExprC (mcPretty_pretty p, decl_node n)
{
  decl_node t;

  mcDebug_assert (n != NULL);
  t = getExprType (n);
  switch (n->kind)
    {
      case nil:
        outNull (p);
        break;

      case true:
        outTrue (p);
        break;

      case false:
        outFalse (p);
        break;

      case constexp:
        doUnary (p, (char *) "", 0, n->unaryF.arg, n->unaryF.resultType, FALSE, FALSE);
        break;

      case neg:
        doUnary (p, (char *) "-", 1, n->unaryF.arg, n->unaryF.resultType, FALSE, FALSE);
        break;

      case not:
        doUnary (p, (char *) "!", 1, n->unaryF.arg, n->unaryF.resultType, FALSE, TRUE);
        break;

      case adr:
        doUnary (p, (char *) "&", 1, n->unaryF.arg, n->unaryF.resultType, TRUE, FALSE);
        break;

      case size:
        doUnary (p, (char *) "sizeof", 6, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case tsize:
        doUnary (p, (char *) "sizeof", 6, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case trunc:
        doUnary (p, (char *) "TRUNC", 5, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case float_:
        doUnary (p, (char *) "FLOAT", 5, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case ord:
        doUnary (p, (char *) "ORD", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case chr:
        doUnary (p, (char *) "CHR", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case high:
        doUnary (p, (char *) "HIGH", 4, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case deref:
        doDeRefC (p, n->unaryF.arg);
        break;

      case equal:
        doBinary (p, (char *) "==", 2, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case notequal:
        doBinary (p, (char *) "!=", 2, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case less:
        doBinary (p, (char *) "<", 1, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case greater:
        doBinary (p, (char *) ">", 1, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case greequal:
        doBinary (p, (char *) ">=", 2, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case lessequal:
        doBinary (p, (char *) "<=", 2, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case componentref:
        doComponentRefC (p, n->componentrefF.rec, n->componentrefF.field);
        break;

      case pointerref:
        doPointerRefC (p, n->pointerrefF.ptr, n->pointerrefF.field);
        break;

      case cast:
        doCastC (p, n->binaryF.left, n->binaryF.right);
        break;

      case val:
        doPreBinary (p, (char *) "VAL", 3, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case plus:
        doPolyBinary (p, (nodeT) plus, n->binaryF.left, n->binaryF.right, FALSE, FALSE);
        break;

      case sub:
        doPolyBinary (p, (nodeT) sub, n->binaryF.left, n->binaryF.right, FALSE, FALSE);
        break;

      case div_:
        doBinary (p, (char *) "/", 1, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case mod:
        doBinary (p, (char *) "%", 1, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case mult:
        doPolyBinary (p, (nodeT) mult, n->binaryF.left, n->binaryF.right, FALSE, FALSE);
        break;

      case divide:
        doPolyBinary (p, (nodeT) divide, n->binaryF.left, n->binaryF.right, FALSE, FALSE);
        break;

      case in:
        doInC (p, n->binaryF.left, n->binaryF.right);
        break;

      case and:
        doBinary (p, (char *) "&&", 2, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case or:
        doBinary (p, (char *) "||", 2, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case literal:
        doLiteralC (p, n);
        break;

      case const_:
        doConstExpr (p, n);
        break;

      case enumerationfield:
        doEnumerationField (p, n);
        break;

      case string:
        doStringC (p, n);
        break;

      case max:
        doUnary (p, (char *) "MAX", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case min:
        doUnary (p, (char *) "MIN", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case var:
        doVar (p, n);
        break;

      case arrayref:
        doArrayRef (p, n);
        break;

      case funccall:
        doFuncExprC (p, n);
        break;

      case procedure:
        doProcedure (p, n);
        break;

      case recordfield:
        doRecordfield (p, n);
        break;

      case setvalue:
        doSetValueC (p, n);
        break;

      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case boolean:
      case proc:
        doBaseC (p, n);
        break;

      case address:
      case loc:
      case byte:
      case word:
        doSystemC (p, n);
        break;

      case type:
        doTypeNameC (p, n);
        break;

      case pointer:
        doTypeNameC (p, n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static void doExprM2 (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (n != NULL);
  switch (n->kind)
    {
      case nil:
        outText (p, (char *) "NIL", 3);
        break;

      case true:
        outText (p, (char *) "TRUE", 4);
        break;

      case false:
        outText (p, (char *) "FALSE", 5);
        break;

      case constexp:
        doUnary (p, (char *) "", 0, n->unaryF.arg, n->unaryF.resultType, FALSE, FALSE);
        break;

      case neg:
        doUnary (p, (char *) "-", 1, n->unaryF.arg, n->unaryF.resultType, FALSE, FALSE);
        break;

      case not:
        doUnary (p, (char *) "NOT", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case adr:
        doUnary (p, (char *) "ADR", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case size:
        doUnary (p, (char *) "SIZE", 4, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case tsize:
        doUnary (p, (char *) "TSIZE", 5, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case float_:
        doUnary (p, (char *) "FLOAT", 5, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case trunc:
        doUnary (p, (char *) "TRUNC", 5, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case ord:
        doUnary (p, (char *) "ORD", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case chr:
        doUnary (p, (char *) "CHR", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case high:
        doUnary (p, (char *) "HIGH", 4, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case deref:
        doPostUnary (p, (char *) "^", 1, n->unaryF.arg);
        break;

      case equal:
        doBinary (p, (char *) "=", 1, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case notequal:
        doBinary (p, (char *) "#", 1, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case less:
        doBinary (p, (char *) "<", 1, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case greater:
        doBinary (p, (char *) ">", 1, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case greequal:
        doBinary (p, (char *) ">=", 2, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case lessequal:
        doBinary (p, (char *) "<=", 2, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case componentref:
        doBinary (p, (char *) ".", 1, n->componentrefF.rec, n->componentrefF.field, FALSE, FALSE);
        break;

      case pointerref:
        doBinary (p, (char *) "^.", 2, n->pointerrefF.ptr, n->pointerrefF.field, FALSE, FALSE);
        break;

      case cast:
        doPreBinary (p, (char *) "CAST", 4, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case val:
        doPreBinary (p, (char *) "VAL", 3, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case plus:
        doBinary (p, (char *) "+", 1, n->binaryF.left, n->binaryF.right, FALSE, FALSE);
        break;

      case sub:
        doBinary (p, (char *) "-", 1, n->binaryF.left, n->binaryF.right, FALSE, FALSE);
        break;

      case div_:
        doBinary (p, (char *) "DIV", 3, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case mod:
        doBinary (p, (char *) "MOD", 3, n->binaryF.left, n->binaryF.right, TRUE, TRUE);
        break;

      case mult:
        doBinary (p, (char *) "*", 1, n->binaryF.left, n->binaryF.right, FALSE, FALSE);
        break;

      case divide:
        doBinary (p, (char *) "/", 1, n->binaryF.left, n->binaryF.right, FALSE, FALSE);
        break;

      case literal:
        doLiteral (p, n);
        break;

      case const_:
        doConstExpr (p, n);
        break;

      case enumerationfield:
        doEnumerationField (p, n);
        break;

      case string:
        doString (p, n);
        break;

      case max:
        doUnary (p, (char *) "MAX", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case min:
        doUnary (p, (char *) "MIN", 3, n->unaryF.arg, n->unaryF.resultType, TRUE, TRUE);
        break;

      case var:
        doVar (p, n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static void doVar (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (decl_isVar (n));
  if (n->varF.isVarParameter)
    {
      outText (p, (char *) "(*", 2);
      doFQNameC (p, n);
      outText (p, (char *) ")", 1);
    }
  else
    doFQNameC (p, n);
}

static void doLiteralC (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  mcDebug_assert (decl_isLiteral (n));
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  if (n->literalF.type == charN)
    {
      if ((DynamicStrings_char (s, -1)) == 'C')
        {
          s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, -1);
          if ((DynamicStrings_char (s, 0)) != '0')
            s = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "0", 1), DynamicStrings_Mark (s));
        }
      outText (p, (char *) "(char)", 6);
      mcPretty_setNeedSpace (p);
    }
  else if ((DynamicStrings_char (s, -1)) == 'H')
    {
      outText (p, (char *) "0x", 2);
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, -1);
    }
  else if ((DynamicStrings_char (s, -1)) == 'B')
    {
      outText (p, (char *) "0", 1);
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, -1);
    }
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
}

static void doLiteral (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  mcDebug_assert (decl_isLiteral (n));
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  if (n->literalF.type == charN)
    {
      if ((DynamicStrings_char (s, -1)) == 'C')
        {
          s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, -1);
          if ((DynamicStrings_char (s, 0)) != '0')
            s = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "0", 1), DynamicStrings_Mark (s));
        }
      outText (p, (char *) "(char)", 6);
      mcPretty_setNeedSpace (p);
    }
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
}

static unsigned int isString (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == string;
}

static void doString (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  mcDebug_assert (isString (n));
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
  M2RTS_HALT (0);
}

static DynamicStrings_String doEscapeC (DynamicStrings_String s, char ch)
{
  DynamicStrings_String t;
  DynamicStrings_String r;
  int h;
  int l;
  int i;

  h = DynamicStrings_Length (s);
  l = 0;
  r = DynamicStrings_InitString ((char *) "", 0);
  i = DynamicStrings_Index (s, ch, (unsigned int ) l);
  for (;;)
  {
    if (i == -1)
      return DynamicStrings_ConCat (r, DynamicStrings_Mark (DynamicStrings_Slice (s, l, 0)));
    else
      {
        while (l < i)
          {
            r = DynamicStrings_ConCatChar (r, DynamicStrings_char (s, l));
            l += 1;
          }
        if (((ch == '\\') && (i < h)) && ((DynamicStrings_char (s, i+1)) == '\\'))
          {
            l = i+2;
            r = DynamicStrings_ConCatChar (r, '\\');
            r = DynamicStrings_ConCatChar (r, '\\');
          }
        else
          {
            if ((i > 0) && ((DynamicStrings_char (s, i-1)) == '\\'))
              ;  /* empty.  */
            else
              {
                r = DynamicStrings_ConCatChar (r, '\\');
                r = DynamicStrings_ConCatChar (r, DynamicStrings_char (s, i));
              }
            mcDebug_assert (l == i);
            l += 1;
          }
      }
    i = DynamicStrings_Index (s, ch, (unsigned int ) l);
  }
}

static DynamicStrings_String escapeContentsC (DynamicStrings_String s, char ch)
{
  return doEscapeC (s, ch);
}

static DynamicStrings_String replaceChar (DynamicStrings_String s, char ch, char *a_, unsigned int _a_high)
{
  int i;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  i = 0;
  for (;;)
  {
    i = DynamicStrings_Index (s, ch, (unsigned int ) i);
    if (i >= 0)
      {
        s = DynamicStrings_ConCat (DynamicStrings_ConCat (DynamicStrings_Slice (s, 0, i), DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high))), DynamicStrings_Slice (s, i+1, 0));
        i += StrLib_StrLen ((char *) a, _a_high);
      }
    else
      return s;
  }
}

static DynamicStrings_String toCstring (nameKey_Name n)
{
  DynamicStrings_String s;

  s = DynamicStrings_Slice (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)), 1, -1);
  return escapeContentsC (escapeContentsC (s, '\\'), '\\');
}

static unsigned int countChar (DynamicStrings_String s, char ch)
{
  int i;
  unsigned int c;

  c = 0;
  i = 0;
  for (;;)
  {
    i = DynamicStrings_Index (s, ch, (unsigned int ) i);
    if (i >= 0)
      {
        i += 1;
        c += 1;
      }
    else
      return c;
  }
}

static unsigned int lenCstring (DynamicStrings_String s)
{
  return (DynamicStrings_Length (s))-(countChar (s, '\\'));
}

static void outCstring (mcPretty_pretty p, decl_node s, unsigned int aString)
{
  if (aString)
    {
      outText (p, (char *) "\"", 1);
      outRawS (p, s->stringF.cstring);
      outText (p, (char *) "\"", 1);
    }
  else
    {
      outText (p, (char *) "'", 1);
      if ((DynamicStrings_char (s->stringF.cstring, 0)) == '\'')
        outText (p, (char *) "\\'", 2);
      else if ((DynamicStrings_char (s->stringF.cstring, 0)) == '\\')
        outText (p, (char *) "\\", 2);
      else
        outRawS (p, s->stringF.cstring);
      outText (p, (char *) "'", 1);
    }
}

static void doStringC (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  mcDebug_assert (isString (n));
  outCstring (p, n, ! n->stringF.isCharCompatible);
}

static unsigned int isPunct (char ch)
{
  return (((((((((ch == '.') || (ch == '(')) || (ch == ')')) || (ch == '^')) || (ch == ':')) || (ch == ';')) || (ch == '{')) || (ch == '}')) || (ch == ',')) || (ch == '*');
}

static unsigned int isWhite (char ch)
{
  return ((ch == ' ') || (ch == ASCII_tab)) || (ch == ASCII_lf);
}

static void outText (mcPretty_pretty p, char *a_, unsigned int _a_high)
{
  DynamicStrings_String s;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  s = DynamicStrings_InitString ((char *) a, _a_high);
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
}

static void outRawS (mcPretty_pretty p, DynamicStrings_String s)
{
  mcPretty_raw (p, s);
}

static mcPretty_pretty outKm2 (mcPretty_pretty p, char *a_, unsigned int _a_high)
{
  unsigned int i;
  DynamicStrings_String s;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  if (StrLib_StrEqual ((char *) a, _a_high, (char *) "RECORD", 6))
    {
      p = mcPretty_pushPretty (p);
      i = mcPretty_getcurpos (p);
      mcPretty_setindent (p, i);
      outText (p, (char *) a, _a_high);
      p = mcPretty_pushPretty (p);
      mcPretty_setindent (p, i+indentation);
    }
  else if (StrLib_StrEqual ((char *) a, _a_high, (char *) "END", 3))
    {
      p = mcPretty_popPretty (p);
      outText (p, (char *) a, _a_high);
      p = mcPretty_popPretty (p);
    }
  return p;
}

static mcPretty_pretty outKc (mcPretty_pretty p, char *a_, unsigned int _a_high)
{
  int i;
  unsigned int c;
  DynamicStrings_String s;
  DynamicStrings_String t;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  s = DynamicStrings_InitString ((char *) a, _a_high);
  i = DynamicStrings_Index (s, '\\', 0);
  if (i == -1)
    t = NULL;
  else
    {
      t = DynamicStrings_Slice (s, i, 0);
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, i);
    }
  if ((DynamicStrings_char (s, 0)) == '{')
    {
      p = mcPretty_pushPretty (p);
      c = mcPretty_getcurpos (p);
      mcPretty_setindent (p, c);
      outTextS (p, s);
      p = mcPretty_pushPretty (p);
      mcPretty_setindent (p, c+indentationC);
    }
  else if ((DynamicStrings_char (s, 0)) == '}')
    {
      p = mcPretty_popPretty (p);
      outTextS (p, s);
      p = mcPretty_popPretty (p);
    }
  outTextS (p, t);
  t = DynamicStrings_KillString (t);
  s = DynamicStrings_KillString (s);
  return p;
}

static void outTextS (mcPretty_pretty p, DynamicStrings_String s)
{
  if (s != NULL)
    mcPretty_prints (p, s);
}

static void outCard (mcPretty_pretty p, unsigned int c)
{
  DynamicStrings_String s;

  s = StringConvert_CardinalToString (c, 0, ' ', 10, FALSE);
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
}

static void outTextN (mcPretty_pretty p, nameKey_Name n)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  mcPretty_prints (p, s);
  s = DynamicStrings_KillString (s);
}

static void doTypeAliasC (mcPretty_pretty p, decl_node n, decl_node *m)
{
  mcPretty_print (p, (char *) "typedef", 7);
  mcPretty_setNeedSpace (p);
  if ((decl_isTypeHidden (n)) && ((decl_isDef (decl_getMainModule ())) || ((decl_getScope (n)) != (decl_getMainModule ()))))
    outText (p, (char *) "void *", 6);
  else
    doTypeC (p, decl_getType (n), m);
  if ((*m) != NULL)
    doFQNameC (p, (*m));
  mcPretty_print (p, (char *) ";\\n\\n", 5);
}

static void doEnumerationC (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node s;
  DynamicStrings_String t;

  outText (p, (char *) "enum {", 6);
  i = Indexing_LowIndice (n->enumerationF.listOfSons);
  h = Indexing_HighIndice (n->enumerationF.listOfSons);
  while (i <= h)
    {
      s = Indexing_GetIndice (n->enumerationF.listOfSons, i);
      doFQDNameC (p, s, FALSE);
      if (i < h)
        {
          outText (p, (char *) ",", 1);
          mcPretty_setNeedSpace (p);
        }
      i += 1;
    }
  outText (p, (char *) "}", 1);
}

static void doNamesC (mcPretty_pretty p, nameKey_Name n)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
}

static void doNameC (mcPretty_pretty p, decl_node n)
{
  if ((n != NULL) && ((decl_getSymName (n)) != nameKey_NulName))
    doNamesC (p, decl_getSymName (n));
}

static void initCname (cnameT *c)
{
  (*c).init = FALSE;
}

static nameKey_Name doCname (nameKey_Name n, cnameT *c, unsigned int scopes)
{
  DynamicStrings_String s;

  if ((*c).init)
    return (*c).name;
  else
    {
      (*c).init = TRUE;
      s = keyc_cname (n, scopes);
      if (s == NULL)
        (*c).name = n;
      else
        {
          (*c).name = nameKey_makekey (DynamicStrings_string (s));
          s = DynamicStrings_KillString (s);
        }
      return (*c).name;
    }
}

static nameKey_Name getDName (decl_node n, unsigned int scopes)
{
  nameKey_Name m;

  m = decl_getSymName (n);
  switch (n->kind)
    {
      case recordfield:
        return doCname (m, &n->recordfieldF.cname, scopes);
        break;

      case enumerationfield:
        return doCname (m, &n->enumerationfieldF.cname, scopes);
        break;


      default:
        break;
    }
  return m;
}

static void doDNameC (mcPretty_pretty p, decl_node n, unsigned int scopes)
{
  if ((n != NULL) && ((decl_getSymName (n)) != nameKey_NulName))
    doNamesC (p, getDName (n, scopes));
}

static void doFQDNameC (mcPretty_pretty p, decl_node n, unsigned int scopes)
{
  DynamicStrings_String s;

  s = getFQDstring (n, scopes);
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
}

static void doFQNameC (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  s = getFQstring (n);
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
}

static void doNameM2 (mcPretty_pretty p, decl_node n)
{
  doNameC (p, n);
}

static void doHighC (mcPretty_pretty p, decl_node a, nameKey_Name n)
{
  if ((decl_isArray (a)) && (decl_isUnbounded (a)))
    {
      mcPretty_print (p, (char *) ",", 1);
      mcPretty_setNeedSpace (p);
      doTypeNameC (p, cardinalN);
      mcPretty_setNeedSpace (p);
      mcPretty_print (p, (char *) "_", 1);
      outTextN (p, n);
      mcPretty_print (p, (char *) "_high", 5);
    }
}

static void doParamC (mcPretty_pretty p, decl_node n)
{
  decl_node ptype;
  nameKey_Name i;
  unsigned int c;
  unsigned int t;
  wlists_wlist l;

  mcDebug_assert (decl_isParam (n));
  ptype = decl_getType (n);
  if (n->paramF.namelist == NULL)
    doTypeNameC (p, ptype);
  else
    {
      mcDebug_assert (isIdentList (n->paramF.namelist));
      l = n->paramF.namelist->identlistF.names;
      if (l == NULL)
        doTypeNameC (p, ptype);
      else
        {
          t = wlists_noOfItemsInList (l);
          c = 1;
          while (c <= t)
            {
              doTypeNameC (p, ptype);
              i = wlists_getItemFromList (l, c);
              if ((decl_isArray (ptype)) && (decl_isUnbounded (ptype)))
                mcPretty_noSpace (p);
              else
                mcPretty_setNeedSpace (p);
              doNamesC (p, i);
              if ((decl_isArray (ptype)) && (decl_isUnbounded (ptype)))
                outText (p, (char *) "_", 1);
              doHighC (p, ptype, i);
              if (c < t)
                {
                  outText (p, (char *) ",", 1);
                  mcPretty_setNeedSpace (p);
                }
              c += 1;
            }
        }
    }
}

static void doVarParamC (mcPretty_pretty p, decl_node n)
{
  decl_node ptype;
  nameKey_Name i;
  unsigned int c;
  unsigned int t;
  wlists_wlist l;

  mcDebug_assert (decl_isVarParam (n));
  ptype = decl_getType (n);
  if (n->varparamF.namelist == NULL)
    {
      doTypeC (p, ptype, &n);
      if (! (decl_isArray (ptype)))
        {
          mcPretty_setNeedSpace (p);
          outText (p, (char *) "*", 1);
        }
    }
  else
    {
      mcDebug_assert (isIdentList (n->varparamF.namelist));
      l = n->varparamF.namelist->identlistF.names;
      if (l == NULL)
        doTypeNameC (p, ptype);
      else
        {
          t = wlists_noOfItemsInList (l);
          c = 1;
          while (c <= t)
            {
              doTypeNameC (p, ptype);
              if (! (decl_isArray (ptype)))
                {
                  mcPretty_setNeedSpace (p);
                  outText (p, (char *) "*", 1);
                }
              i = wlists_getItemFromList (l, c);
              doNamesC (p, i);
              doHighC (p, ptype, i);
              if (c < t)
                {
                  outText (p, (char *) ",", 1);
                  mcPretty_setNeedSpace (p);
                }
              c += 1;
            }
        }
    }
}

static void doOptargC (mcPretty_pretty p, decl_node n)
{
  decl_node ptype;
  nameKey_Name i;
  unsigned int t;
  wlists_wlist l;

  mcDebug_assert (decl_isOptarg (n));
  ptype = decl_getType (n);
  mcDebug_assert (n->optargF.namelist != NULL);
  mcDebug_assert (isIdentList (n->paramF.namelist));
  l = n->paramF.namelist->identlistF.names;
  mcDebug_assert (l != NULL);
  t = wlists_noOfItemsInList (l);
  mcDebug_assert (t == 1);
  doTypeNameC (p, ptype);
  i = wlists_getItemFromList (l, 1);
  mcPretty_setNeedSpace (p);
  doNamesC (p, i);
}

static void doParameterC (mcPretty_pretty p, decl_node n)
{
  if (decl_isParam (n))
    doParamC (p, n);
  else if (decl_isVarParam (n))
    doVarParamC (p, n);
  else if (decl_isVarargs (n))
    mcPretty_print (p, (char *) "...", 3);
  else if (decl_isOptarg (n))
    doOptargC (p, n);
}

static void doProcTypeC (mcPretty_pretty p, decl_node t, decl_node n)
{
  mcDebug_assert (decl_isType (t));
  outputPartial (t);
  doCompletePartialProcType (p, t, n);
}

static void doTypesC (decl_node n)
{
  decl_node m;

  if (decl_isType (n))
    {
      m = decl_getType (n);
      if (decl_isProcType (m))
        doProcTypeC (doP, n, m);
      else if ((decl_isType (m)) || (decl_isPointer (m)))
        {
          outText (doP, (char *) "typedef", 7);
          mcPretty_setNeedSpace (doP);
          doTypeC (doP, m, &m);
          if (decl_isType (m))
            mcPretty_setNeedSpace (doP);
          doTypeNameC (doP, n);
          outText (doP, (char *) ";\\n\\n", 5);
        }
      else if (decl_isEnumeration (m))
        {
          outText (doP, (char *) "typedef", 7);
          mcPretty_setNeedSpace (doP);
          doTypeC (doP, m, &m);
          mcPretty_setNeedSpace (doP);
          doTypeNameC (doP, n);
          outText (doP, (char *) ";\\n\\n", 5);
        }
      else
        {
          outText (doP, (char *) "typedef", 7);
          mcPretty_setNeedSpace (doP);
          doTypeC (doP, m, &m);
          if (decl_isType (m))
            mcPretty_setNeedSpace (doP);
          doTypeNameC (doP, n);
          outText (doP, (char *) ";\\n\\n", 5);
        }
    }
}

static void doCompletePartialC (decl_node n)
{
  decl_node m;

  if (decl_isType (n))
    {
      m = decl_getType (n);
      if (decl_isRecord (m))
        doCompletePartialRecord (doP, n, m);
      else if (decl_isArray (m))
        doCompletePartialArray (doP, n, m);
      else if (decl_isProcType (m))
        doCompletePartialProcType (doP, n, m);
    }
}

static void doCompletePartialRecord (mcPretty_pretty p, decl_node t, decl_node r)
{
  unsigned int i;
  unsigned int h;
  decl_node f;

  mcDebug_assert (decl_isRecord (r));
  mcDebug_assert (decl_isType (t));
  outText (p, (char *) "struct", 6);
  mcPretty_setNeedSpace (p);
  doFQNameC (p, t);
  outText (p, (char *) "_r", 2);
  mcPretty_setNeedSpace (p);
  p = outKc (p, (char *) "{\\n", 3);
  i = Indexing_LowIndice (r->recordF.listOfSons);
  h = Indexing_HighIndice (r->recordF.listOfSons);
  while (i <= h)
    {
      f = Indexing_GetIndice (r->recordF.listOfSons, i);
      if (decl_isRecordField (f))
        if (! f->recordfieldF.tag)
          {
            mcPretty_setNeedSpace (p);
            doRecordFieldC (p, f);
            outText (p, (char *) ";\\n", 3);
          }
      else if (decl_isVarient (f))
        {
          doVarientC (p, f);
          outText (p, (char *) ";\\n", 3);
        }
      else if (decl_isVarientField (f))
        doVarientFieldC (p, f);
      i += 1;
    }
  p = outKc (p, (char *) "};\\n\\n", 6);
}

static void doCompletePartialArray (mcPretty_pretty p, decl_node t, decl_node r)
{
  decl_node type;
  decl_node s;

  mcDebug_assert (decl_isArray (r));
  type = r->arrayF.type;
  s = NULL;
  outText (p, (char *) "struct", 6);
  mcPretty_setNeedSpace (p);
  doFQNameC (p, t);
  outText (p, (char *) "_a {", 4);
  mcPretty_setNeedSpace (p);
  doTypeC (p, type, &s);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "array[", 6);
  doSubrC (p, r->arrayF.subr);
  outText (p, (char *) "];", 2);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "};\\n", 4);
}

static decl_node lookupConst (decl_node type, nameKey_Name n)
{
  return decl_makeLiteralInt (n);
}

static decl_node doMin (decl_node n)
{
  if (n == booleanN)
    return falseN;
  else if (n == integerN)
    {
      keyc_useIntMin ();
      return lookupConst (integerN, nameKey_makeKey ((char *) "INT_MIN", 7));
    }
  else if (n == cardinalN)
    {
      keyc_useUIntMin ();
      return lookupConst (cardinalN, nameKey_makeKey ((char *) "UINT_MIN", 8));
    }
  else if (n == longintN)
    {
      keyc_useLongMin ();
      return lookupConst (longintN, nameKey_makeKey ((char *) "LONG_MIN", 8));
    }
  else if (n == longcardN)
    {
      keyc_useULongMin ();
      return lookupConst (longcardN, nameKey_makeKey ((char *) "LONG_MIN", 8));
    }
  else if (n == charN)
    {
      keyc_useCharMin ();
      return lookupConst (charN, nameKey_makeKey ((char *) "CHAR_MIN", 8));
    }
  else if (n == bitsetN)
    return lookupConst (bitnumN, nameKey_makeKey ((char *) "0", 1));
  else if (n == locN)
    {
      keyc_useUCharMin ();
      return lookupConst (locN, nameKey_makeKey ((char *) "UCHAR_MIN", 9));
    }
  else if (n == byteN)
    {
      keyc_useUCharMin ();
      return lookupConst (byteN, nameKey_makeKey ((char *) "UCHAR_MIN", 9));
    }
  else if (n == wordN)
    {
      keyc_useUIntMin ();
      return lookupConst (wordN, nameKey_makeKey ((char *) "UCHAR_MIN", 9));
    }
  else if (n == addressN)
    return lookupConst (addressN, nameKey_makeKey ((char *) "((void *) 0)", 12));
  else
    M2RTS_HALT (0);
}

static decl_node doMax (decl_node n)
{
  if (n == booleanN)
    return trueN;
  else if (n == integerN)
    {
      keyc_useIntMax ();
      return lookupConst (integerN, nameKey_makeKey ((char *) "INT_MAX", 7));
    }
  else if (n == cardinalN)
    {
      keyc_useUIntMax ();
      return lookupConst (cardinalN, nameKey_makeKey ((char *) "UINT_MAX", 8));
    }
  else if (n == longintN)
    {
      keyc_useLongMax ();
      return lookupConst (longintN, nameKey_makeKey ((char *) "LONG_MAX", 8));
    }
  else if (n == longcardN)
    {
      keyc_useULongMax ();
      return lookupConst (longcardN, nameKey_makeKey ((char *) "ULONG_MAX", 9));
    }
  else if (n == charN)
    {
      keyc_useCharMax ();
      return lookupConst (charN, nameKey_makeKey ((char *) "CHAR_MAX", 8));
    }
  else if (n == bitsetN)
    return lookupConst (bitnumN, nameKey_makeKey ((char *) "(sizeof (unsigned int)*8)", 25));
  else if (n == locN)
    {
      keyc_useUCharMax ();
      return lookupConst (locN, nameKey_makeKey ((char *) "UCHAR_MAX", 9));
    }
  else if (n == byteN)
    {
      keyc_useUCharMax ();
      return lookupConst (byteN, nameKey_makeKey ((char *) "UCHAR_MAX", 9));
    }
  else if (n == wordN)
    {
      keyc_useUIntMax ();
      return lookupConst (wordN, nameKey_makeKey ((char *) "UINT_MAX", 8));
    }
  else if (n == addressN)
    {
      mcMetaError_metaError1 ((char *) "trying to obtain MAX ({%1ad}) is illegal", 40, (unsigned char *) &n, sizeof (n));
      return NULL;
    }
  else
    M2RTS_HALT (0);
}

static decl_node getMax (decl_node n)
{
  if (decl_isSubrange (n))
    return n->subrangeF.high;
  else if (decl_isEnumeration (n))
    return n->enumerationF.high;
  else
    {
      mcDebug_assert (isOrdinal (n));
      return doMax (n);
    }
}

static decl_node getMin (decl_node n)
{
  if (decl_isSubrange (n))
    return n->subrangeF.low;
  else if (decl_isEnumeration (n))
    return n->enumerationF.low;
  else
    {
      mcDebug_assert (isOrdinal (n));
      return doMin (n);
    }
}

static void doSubtractC (mcPretty_pretty p, decl_node s)
{
  if (! (isZero (s)))
    {
      outText (p, (char *) "-", 1);
      doExprC (p, s);
    }
}

static void doSubrC (mcPretty_pretty p, decl_node s)
{
  decl_node low;
  decl_node high;

  s = decl_skipType (s);
  if (isOrdinal (s))
    {
      low = getMin (s);
      high = getMax (s);
      doExprC (p, high);
      doSubtractC (p, low);
      outText (p, (char *) "+1", 2);
    }
  else
    {
      mcDebug_assert (decl_isSubrange (s));
      if ((s->subrangeF.high == NULL) || (s->subrangeF.low == NULL))
        doSubrC (p, decl_getType (s));
      else
        {
          doExprC (p, s->subrangeF.high);
          doSubtractC (p, s->subrangeF.low);
          outText (p, (char *) "+1", 2);
        }
    }
}

static void doCompletePartialProcType (mcPretty_pretty p, decl_node t, decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node v;
  decl_node u;

  mcDebug_assert (decl_isProcType (n));
  u = NULL;
  outText (p, (char *) "typedef", 7);
  mcPretty_setNeedSpace (p);
  doTypeC (p, n->proctypeF.returnType, &u);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(*", 2);
  doFQNameC (p, t);
  outText (p, (char *) "_t) (", 5);
  i = Indexing_LowIndice (n->proctypeF.parameters);
  h = Indexing_HighIndice (n->proctypeF.parameters);
  while (i <= h)
    {
      v = Indexing_GetIndice (n->proctypeF.parameters, i);
      doParameterC (p, v);
      mcPretty_noSpace (p);
      if (i < h)
        {
          outText (p, (char *) ",", 1);
          mcPretty_setNeedSpace (p);
        }
      i += 1;
    }
  if (h == 0)
    outText (p, (char *) "void", 4);
  outText (p, (char *) ");\\n", 4);
  outText (p, (char *) "struct", 6);
  mcPretty_setNeedSpace (p);
  doFQNameC (p, t);
  outText (p, (char *) "_p {", 4);
  mcPretty_setNeedSpace (p);
  doFQNameC (p, t);
  outText (p, (char *) "_t proc; };\\n\\n", 15);
}

static unsigned int isBase (decl_node n)
{
  switch (n->kind)
    {
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case boolean:
      case proc:
        return TRUE;
        break;


      default:
        return FALSE;
        break;
    }
}

static void doBaseC (mcPretty_pretty p, decl_node n)
{
  switch (n->kind)
    {
      case char_:
        outText (p, (char *) "char", 4);
        break;

      case cardinal:
        outText (p, (char *) "unsigned int", 12);
        break;

      case longcard:
        outText (p, (char *) "long unsigned int", 17);
        break;

      case shortcard:
        outText (p, (char *) "short unsigned int", 18);
        break;

      case integer:
        outText (p, (char *) "int", 3);
        break;

      case longint:
        outText (p, (char *) "long int", 8);
        break;

      case shortint:
        outText (p, (char *) "short int", 9);
        break;

      case real:
        outText (p, (char *) "double", 6);
        break;

      case longreal:
        outText (p, (char *) "long double", 11);
        break;

      case shortreal:
        outText (p, (char *) "float", 5);
        break;

      case bitset:
        outText (p, (char *) "unsigned int", 12);
        break;

      case boolean:
        outText (p, (char *) "unsigned int", 12);
        break;

      case proc:
        outText (p, (char *) "PROC", 4);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  mcPretty_setNeedSpace (p);
}

static unsigned int isSystem (decl_node n)
{
  switch (n->kind)
    {
      case address:
        return TRUE;
        break;

      case loc:
        return TRUE;
        break;

      case byte:
        return TRUE;
        break;

      case word:
        return TRUE;
        break;


      default:
        return FALSE;
        break;
    }
}

static void doSystemC (mcPretty_pretty p, decl_node n)
{
  switch (n->kind)
    {
      case address:
        outText (p, (char *) "void *", 6);
        break;

      case loc:
        outText (p, (char *) "unsigned char", 13);
        mcPretty_setNeedSpace (p);
        break;

      case byte:
        outText (p, (char *) "unsigned char", 13);
        mcPretty_setNeedSpace (p);
        break;

      case word:
        outText (p, (char *) "unsigned int", 12);
        mcPretty_setNeedSpace (p);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static void doArrayC (mcPretty_pretty p, decl_node n)
{
  decl_node t;
  decl_node s;
  decl_node u;

  mcDebug_assert (decl_isArray (n));
  t = n->arrayF.type;
  s = n->arrayF.subr;
  u = NULL;
  if (s == NULL)
    {
      doTypeC (p, t, &u);
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "*", 1);
    }
  else
    {
      outText (p, (char *) "struct", 6);
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "{", 1);
      mcPretty_setNeedSpace (p);
      doTypeC (p, t, &u);
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "array[", 6);
      if (isZero (getMin (s)))
        doExprC (p, getMax (s));
      else
        {
          doExprC (p, getMax (s));
          doSubtractC (p, getMin (s));
        }
      outText (p, (char *) "];", 2);
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "}", 1);
    }
}

static void doPointerC (mcPretty_pretty p, decl_node n, decl_node *m)
{
  decl_node t;
  decl_node s;

  t = n->pointerF.type;
  s = NULL;
  doTypeC (p, t, &s);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "*", 1);
}

static void doRecordFieldC (mcPretty_pretty p, decl_node f)
{
  decl_node m;

  m = NULL;
  mcPretty_setNeedSpace (p);
  doTypeC (p, f->recordfieldF.type, &m);
  doDNameC (p, f, FALSE);
}

static void doVarientFieldC (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  mcDebug_assert (decl_isVarientField (n));
  if (! n->varientfieldF.simple)
    {
      outText (p, (char *) "struct", 6);
      mcPretty_setNeedSpace (p);
      p = outKc (p, (char *) "{\\n", 3);
    }
  i = Indexing_LowIndice (n->varientfieldF.listOfSons);
  t = Indexing_HighIndice (n->varientfieldF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientfieldF.listOfSons, i);
      if (decl_isRecordField (q))
        if (! q->recordfieldF.tag)
          {
            doRecordFieldC (p, q);
            outText (p, (char *) ";\\n", 3);
          }
      else if (decl_isVarient (q))
        {
          doVarientC (p, q);
          outText (p, (char *) ";\\n", 3);
        }
      else
        M2RTS_HALT (0);
      i += 1;
    }
  if (! n->varientfieldF.simple)
    p = outKc (p, (char *) "};\\n", 4);
}

static void doVarientC (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  mcDebug_assert (decl_isVarient (n));
  if (n->varientF.tag != NULL)
    if (decl_isRecordField (n->varientF.tag))
      {
        doRecordFieldC (p, n->varientF.tag);
        outText (p, (char *) ";  /* case tag */\\n", 19);
      }
    else if (decl_isVarientField (n->varientF.tag))
      M2RTS_HALT (0);
    else
      M2RTS_HALT (0);
  outText (p, (char *) "union", 5);
  mcPretty_setNeedSpace (p);
  p = outKc (p, (char *) "{\\n", 3);
  i = Indexing_LowIndice (n->varientF.listOfSons);
  t = Indexing_HighIndice (n->varientF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientF.listOfSons, i);
      if (decl_isRecordField (q))
        if (! q->recordfieldF.tag)
          {
            doRecordFieldC (p, q);
            outText (p, (char *) ";\\n", 3);
          }
      else if (decl_isVarientField (q))
        doVarientFieldC (p, q);
      else
        M2RTS_HALT (0);
      i += 1;
    }
  p = outKc (p, (char *) "}", 1);
}

static void doRecordC (mcPretty_pretty p, decl_node n, decl_node *m)
{
  unsigned int i;
  unsigned int h;
  decl_node f;

  mcDebug_assert (decl_isRecord (n));
  outText (p, (char *) "struct", 6);
  mcPretty_setNeedSpace (p);
  p = outKc (p, (char *) "{", 1);
  i = Indexing_LowIndice (n->recordF.listOfSons);
  h = Indexing_HighIndice (n->recordF.listOfSons);
  mcPretty_setindent (p, (mcPretty_getcurpos (p))+indentation);
  outText (p, (char *) "\\n", 2);
  while (i <= h)
    {
      f = Indexing_GetIndice (n->recordF.listOfSons, i);
      if (decl_isRecordField (f))
        if (! f->recordfieldF.tag)
          {
            doRecordFieldC (p, f);
            outText (p, (char *) ";\\n", 3);
          }
      else if (decl_isVarient (f))
        {
          doVarientC (p, f);
          outText (p, (char *) ";\\n", 3);
        }
      else if (decl_isVarientField (f))
        doVarientFieldC (p, f);
      i += 1;
    }
  p = outKc (p, (char *) "}", 1);
  mcPretty_setNeedSpace (p);
}

static unsigned int isBitset (decl_node n)
{
  return n == bitsetN;
}

static unsigned int isNegative (decl_node n)
{
  return FALSE;
}

static void doSubrangeC (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (decl_isSubrange (n));
  if (isNegative (n->subrangeF.low))
    {
      outText (p, (char *) "int", 3);
      mcPretty_setNeedSpace (p);
    }
  else
    {
      outText (p, (char *) "unsigned int", 12);
      mcPretty_setNeedSpace (p);
    }
}

static void doSetC (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (decl_isSet (n));
  outText (p, (char *) "unsigned int", 12);
  mcPretty_setNeedSpace (p);
}

static void doTypeC (mcPretty_pretty p, decl_node n, decl_node *m)
{
  if (n == NULL)
    outText (p, (char *) "void", 4);
  else if (isBase (n))
    doBaseC (p, n);
  else if (isSystem (n))
    doSystemC (p, n);
  else if (decl_isEnumeration (n))
    doEnumerationC (p, n);
  else if (decl_isType (n))
    {
      doFQNameC (p, n);
      mcPretty_setNeedSpace (p);
    }
  else if (decl_isProcType (n))
    doProcTypeC (p, n, (*m));
  else if (decl_isArray (n))
    doArrayC (p, n);
  else if (decl_isRecord (n))
    doRecordC (p, n, m);
  else if (decl_isPointer (n))
    doPointerC (p, n, m);
  else if (decl_isSubrange (n))
    doSubrangeC (p, n);
  else if (decl_isSet (n))
    doSetC (p, n);
  else
    {
      mcPretty_print (p, (char *) "to do ...  typedef etc etc ", 27);
      doFQNameC (p, n);
      mcPretty_print (p, (char *) ";\\n", 3);
      M2RTS_HALT (0);
    }
}

static void doArrayNameC (mcPretty_pretty p, decl_node n)
{
  doTypeNameC (p, decl_getType (n));
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "*", 1);
}

static void doRecordNameC (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  s = getFQstring (n);
  s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "_r", 2)));
  outTextS (p, s);
  s = DynamicStrings_KillString (s);
}

static void doTypeNameC (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String t;

  if (n == NULL)
    {
      outText (p, (char *) "void", 4);
      mcPretty_setNeedSpace (p);
    }
  else if (isBase (n))
    doBaseC (p, n);
  else if (isSystem (n))
    doSystemC (p, n);
  else if (decl_isEnumeration (n))
    mcPretty_print (p, (char *) "is enumeration type name required\\n", 35);
  else if (decl_isType (n))
    doFQNameC (p, n);
  else if (decl_isProcType (n))
    mcPretty_print (p, (char *) "is proc type name required\\n", 28);
  else if (decl_isArray (n))
    doArrayNameC (p, n);
  else if (decl_isRecord (n))
    doRecordNameC (p, n);
  else
    {
      mcPretty_print (p, (char *) "some other kind of name required\\n", 34);
      stop ();
    }
}

static void doVarC (decl_node n)
{
  decl_node s;

  if (decl_isDef (decl_getMainModule ()))
    {
      mcPretty_print (doP, (char *) "EXTERN", 6);
      mcPretty_setNeedSpace (doP);
    }
  else if ((! (decl_isExported (n))) && (! (isLocal (n))))
    {
      mcPretty_print (doP, (char *) "static", 6);
      mcPretty_setNeedSpace (doP);
    }
  s = NULL;
  doTypeC (doP, decl_getType (n), &s);
  mcPretty_setNeedSpace (doP);
  doFQNameC (doP, n);
  mcPretty_print (doP, (char *) ";\\n", 3);
}

static void doProcedureHeadingC (decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node p;
  decl_node q;

  mcDebug_assert (decl_isProcedure (n));
  mcPretty_noSpace (doP);
  if (decl_isDef (decl_getMainModule ()))
    {
      outText (doP, (char *) "EXTERN", 6);
      mcPretty_setNeedSpace (doP);
    }
  else if (! (decl_isExported (n)))
    {
      outText (doP, (char *) "static", 6);
      mcPretty_setNeedSpace (doP);
    }
  q = NULL;
  doTypeC (doP, n->procedureF.returnType, &q);
  mcPretty_setNeedSpace (doP);
  doFQDNameC (doP, n, FALSE);
  mcPretty_setNeedSpace (doP);
  outText (doP, (char *) "(", 1);
  i = Indexing_LowIndice (n->procedureF.parameters);
  h = Indexing_HighIndice (n->procedureF.parameters);
  while (i <= h)
    {
      p = Indexing_GetIndice (n->procedureF.parameters, i);
      doParameterC (doP, p);
      mcPretty_noSpace (doP);
      if (i < h)
        {
          mcPretty_print (doP, (char *) ",", 1);
          mcPretty_setNeedSpace (doP);
        }
      i += 1;
    }
  if (h == 0)
    outText (doP, (char *) "void", 4);
  mcPretty_print (doP, (char *) ")", 1);
}

static unsigned int checkDeclareUnboundedParamCopyC (mcPretty_pretty p, decl_node n)
{
  decl_node t;
  unsigned int i;
  unsigned int c;
  wlists_wlist l;
  unsigned int seen;

  seen = FALSE;
  t = decl_getType (n);
  l = n->paramF.namelist->identlistF.names;
  if (((decl_isArray (t)) && (decl_isUnbounded (t))) && (l != NULL))
    {
      t = decl_getType (t);
      c = wlists_noOfItemsInList (l);
      i = 1;
      while (i <= c)
        {
          doTypeNameC (p, t);
          mcPretty_setNeedSpace (p);
          doNamesC (p, (nameKey_Name) wlists_getItemFromList (l, i));
          outText (p, (char *) "[_", 2);
          doNamesC (p, (nameKey_Name) wlists_getItemFromList (l, i));
          outText (p, (char *) "_high+1];\\n", 11);
          seen = TRUE;
          i += 1;
        }
    }
  return seen;
}

static void checkUnboundedParamCopyC (mcPretty_pretty p, decl_node n)
{
  decl_node t;
  unsigned int i;
  unsigned int c;
  wlists_wlist l;

  t = decl_getType (n);
  l = n->paramF.namelist->identlistF.names;
  if (((decl_isArray (t)) && (decl_isUnbounded (t))) && (l != NULL))
    {
      c = wlists_noOfItemsInList (l);
      i = 1;
      t = decl_skipType (decl_getType (t));
      while (i <= c)
        {
          keyc_useMemcpy ();
          outText (p, (char *) "memcpy (", 8);
          doNamesC (p, (nameKey_Name) wlists_getItemFromList (l, i));
          outText (p, (char *) ",", 1);
          mcPretty_setNeedSpace (p);
          doNamesC (p, (nameKey_Name) wlists_getItemFromList (l, i));
          outText (p, (char *) "_, ", 3);
          if (((t == charN) || (t == byteN)) || (t == locN))
            {
              outText (p, (char *) "_", 1);
              doNamesC (p, (nameKey_Name) wlists_getItemFromList (l, i));
              outText (p, (char *) "_high+1);\\n", 11);
            }
          else
            {
              outText (p, (char *) "(_", 2);
              doNamesC (p, (nameKey_Name) wlists_getItemFromList (l, i));
              outText (p, (char *) "_high+1)", 8);
              mcPretty_setNeedSpace (p);
              doMultiplyBySize (p, t);
              outText (p, (char *) ");\\n", 4);
            }
          i += 1;
        }
    }
}

static void doUnboundedParamCopyC (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node q;
  unsigned int seen;

  mcDebug_assert (decl_isProcedure (n));
  i = Indexing_LowIndice (n->procedureF.parameters);
  h = Indexing_HighIndice (n->procedureF.parameters);
  seen = FALSE;
  while (i <= h)
    {
      q = Indexing_GetIndice (n->procedureF.parameters, i);
      if (decl_isParam (q))
        seen = (checkDeclareUnboundedParamCopyC (p, q)) || seen;
      i += 1;
    }
  if (seen)
    {
      outText (p, (char *) "\\n", 2);
      outText (p, (char *) "/* make a local copy of each unbounded array.  */\\n", 51);
      i = Indexing_LowIndice (n->procedureF.parameters);
      while (i <= h)
        {
          q = Indexing_GetIndice (n->procedureF.parameters, i);
          if (decl_isParam (q))
            checkUnboundedParamCopyC (p, q);
          i += 1;
        }
    }
}

static void doPrototypeC (decl_node n)
{
  if (! (decl_isExported (n)))
    if (! ((mcOptions_getExtendedOpaque ()) && (isDefForC (decl_getScope (n)))))
      {
        keyc_enterScope (n);
        doProcedureHeadingC (n);
        mcPretty_print (doP, (char *) ";\\n", 3);
        keyc_leaveScope (n);
      }
}

static void addTodo (decl_node n)
{
  if (((n != NULL) && (! (alists_isItemInList (partialQ, (void *) n)))) && (! (alists_isItemInList (doneQ, (void *) n))))
    {
      mcDebug_assert (! (decl_isVarient (n)));
      mcDebug_assert (! (decl_isVarientField (n)));
      alists_includeItemIntoList (todoQ, (void *) n);
    }
}

static void addVariablesTodo (decl_node n)
{
  if (decl_isVar (n))
    if (n->varF.isParameter || n->varF.isVarParameter)
      {
        addDone (n);
        addTodo (decl_getType (n));
      }
    else
      addTodo (n);
}

static void addTypesTodo (decl_node n)
{
  if (decl_isUnbounded (n))
    addDone (n);
  else
    addTodo (n);
}

static DynamicStrings_String tempName (void)
{
  tempCount += 1;
  return FormatStrings_Sprintf1 (DynamicStrings_InitString ((char *) "_T%d", 4), (unsigned char *) &tempCount, sizeof (tempCount));
}

static decl_node makeIntermediateType (DynamicStrings_String s, decl_node p)
{
  nameKey_Name n;
  decl_node o;

  n = nameKey_makekey (DynamicStrings_string (s));
  decl_enterScope (decl_getScope (p));
  o = p;
  p = decl_makeType (nameKey_makekey (DynamicStrings_string (s)));
  decl_putType (p, o);
  putTypeInternal (p);
  decl_leaveScope ();
  return p;
}

static void simplifyType (alists_alist l, decl_node *p)
{
  DynamicStrings_String s;

  if ((((*p) != NULL) && (((decl_isRecord ((*p))) || (decl_isArray ((*p)))) || (decl_isProcType ((*p))))) && (! (decl_isUnbounded ((*p)))))
    {
      s = tempName ();
      (*p) = makeIntermediateType (s, (*p));
      s = DynamicStrings_KillString (s);
      simplified = FALSE;
    }
}

static void simplifyVar (alists_alist l, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node v;
  decl_node d;
  decl_node o;

  mcDebug_assert (decl_isVar (n));
  o = n->varF.type;
  simplifyType (l, &n->varF.type);
  if (o != n->varF.type)
    {
      d = n->varF.decl;
      mcDebug_assert (isVarDecl (d));
      t = wlists_noOfItemsInList (d->vardeclF.names);
      i = 1;
      while (i <= t)
        {
          v = decl_lookupInScope (n->varF.scope, (nameKey_Name) wlists_getItemFromList (d->vardeclF.names, i));
          mcDebug_assert (decl_isVar (v));
          v->varF.type = n->varF.type;
          i += 1;
        }
    }
}

static void simplifyRecord (alists_alist l, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  i = Indexing_LowIndice (n->recordF.listOfSons);
  t = Indexing_HighIndice (n->recordF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->recordF.listOfSons, i);
      simplifyNode (l, q);
      i += 1;
    }
}

static void simplifyVarient (alists_alist l, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  simplifyNode (l, n->varientF.tag);
  i = Indexing_LowIndice (n->varientF.listOfSons);
  t = Indexing_HighIndice (n->varientF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientF.listOfSons, i);
      simplifyNode (l, q);
      i += 1;
    }
}

static void simplifyVarientField (alists_alist l, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  i = Indexing_LowIndice (n->varientfieldF.listOfSons);
  t = Indexing_HighIndice (n->varientfieldF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientfieldF.listOfSons, i);
      simplifyNode (l, q);
      i += 1;
    }
}

static void doSimplifyNode (alists_alist l, decl_node n)
{
  if (n == NULL)
    ;  /* empty.  */
  else if (decl_isType (n))
    simplifyNode (l, decl_getType (n));
  else if (decl_isVar (n))
    simplifyVar (l, n);
  else if (decl_isRecord (n))
    simplifyRecord (l, n);
  else if (decl_isRecordField (n))
    simplifyType (l, &n->recordfieldF.type);
  else if (decl_isArray (n))
    simplifyType (l, &n->arrayF.type);
  else if (decl_isVarient (n))
    simplifyVarient (l, n);
  else if (decl_isVarientField (n))
    simplifyVarientField (l, n);
  else if (decl_isPointer (n))
    simplifyType (l, &n->pointerF.type);
}

static void simplifyNode (alists_alist l, decl_node n)
{
  if (! (alists_isItemInList (l, (void *) n)))
    {
      alists_includeItemIntoList (l, (void *) n);
      doSimplifyNode (l, n);
    }
}

static void doSimplify (decl_node n)
{
  alists_alist l;

  l = alists_initList ();
  simplifyNode (l, n);
  alists_killList (&l);
}

static void simplifyTypes (scopeT s)
{
  do {
    simplified = TRUE;
    Indexing_ForeachIndiceInIndexDo (s.types, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doSimplify});
    Indexing_ForeachIndiceInIndexDo (s.variables, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doSimplify});
  } while (! (simplified));
}

static void outDeclsDefC (mcPretty_pretty p, decl_node n)
{
  scopeT s;

  s = n->defF.decls;
  simplifyTypes (s);
  includeConstType (s);
  doP = p;
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstC}, (nodeProcedure) {(nodeProcedure_t) doTypesC}, (nodeProcedure) {(nodeProcedure_t) doVarC}, (nodeProcedure) {(nodeProcedure_t) outputPartial}, (nodeProcedure) {(nodeProcedure_t) doNone}, (nodeProcedure) {(nodeProcedure_t) doCompletePartialC}, (nodeProcedure) {(nodeProcedure_t) doNone});
  includeDefVarProcedure (n);
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstC}, (nodeProcedure) {(nodeProcedure_t) doTypesC}, (nodeProcedure) {(nodeProcedure_t) doVarC}, (nodeProcedure) {(nodeProcedure_t) outputPartial}, (nodeProcedure) {(nodeProcedure_t) doNone}, (nodeProcedure) {(nodeProcedure_t) doCompletePartialC}, (nodeProcedure) {(nodeProcedure_t) doNone});
  Indexing_ForeachIndiceInIndexDo (s.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doPrototypeC});
}

static void includeConstType (scopeT s)
{
  Indexing_ForeachIndiceInIndexDo (s.constants, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) addTodo});
  Indexing_ForeachIndiceInIndexDo (s.types, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) addTypesTodo});
}

static void includeVarProcedure (scopeT s)
{
  Indexing_ForeachIndiceInIndexDo (s.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) addTodo});
  Indexing_ForeachIndiceInIndexDo (s.variables, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) addVariablesTodo});
}

static void includeVar (scopeT s)
{
  Indexing_ForeachIndiceInIndexDo (s.variables, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) addTodo});
}

static void includeExternals (decl_node n)
{
  alists_alist l;

  l = alists_initList ();
  visitNode (l, n, (nodeProcedure) {(nodeProcedure_t) addExported});
  alists_killList (&l);
}

static void checkSystemInclude (decl_node n)
{
}

static void addExported (decl_node n)
{
  decl_node s;

  s = decl_getScope (n);
  if (((s != NULL) && (decl_isDef (s))) && (s != defModule))
    if (((decl_isType (n)) || (decl_isVar (n))) || (decl_isConst (n)))
      addTodo (n);
}

static void addExternal (decl_node n)
{
  if (((((decl_getScope (n)) == defModule) && (decl_isType (n))) && (decl_isTypeHidden (n))) && (! (mcOptions_getExtendedOpaque ())))
    ;  /* empty.  */
  else
    addTodo (n);
}

static void includeDefConstType (decl_node n)
{
  decl_node d;

  if (decl_isImp (n))
    {
      defModule = decl_lookupDef (decl_getSymName (n));
      if (defModule != NULL)
        {
          simplifyTypes (defModule->defF.decls);
          includeConstType (defModule->defF.decls);
          symbolKey_foreachNodeDo (defModule->defF.decls.symbols, (symbolKey_performOperation) {(symbolKey_performOperation_t) addExternal});
        }
    }
}

static void runIncludeDefConstType (decl_node n)
{
  decl_node d;

  if (decl_isDef (n))
    {
      simplifyTypes (n->defF.decls);
      includeConstType (n->defF.decls);
      symbolKey_foreachNodeDo (n->defF.decls.symbols, (symbolKey_performOperation) {(symbolKey_performOperation_t) addExternal});
    }
}

static void joinProcedures (decl_node i, decl_node d)
{
  unsigned int h;
  unsigned int j;

  mcDebug_assert (decl_isDef (d));
  mcDebug_assert (decl_isImp (i));
  j = 1;
  h = Indexing_HighIndice (d->defF.decls.procedures);
  while (j <= h)
    {
      Indexing_IncludeIndiceIntoIndex (i->impF.decls.procedures, Indexing_GetIndice (d->defF.decls.procedures, j));
      j += 1;
    }
}

static void includeDefVarProcedure (decl_node n)
{
  decl_node d;

  if (decl_isImp (n))
    {
      defModule = decl_lookupDef (decl_getSymName (n));
      if (defModule != NULL)
        joinProcedures (n, defModule);
    }
  else if (decl_isDef (n))
    {
      includeVar (n->defF.decls);
      simplifyTypes (n->defF.decls);
    }
}

static void foreachModuleDo (decl_node n, symbolKey_performOperation p)
{
  decl_foreachDefModuleDo (p);
  decl_foreachModModuleDo (p);
}

static void outDeclsImpC (mcPretty_pretty p, scopeT s)
{
  simplifyTypes (s);
  includeConstType (s);
  doP = p;
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstC}, (nodeProcedure) {(nodeProcedure_t) doTypesC}, (nodeProcedure) {(nodeProcedure_t) doVarC}, (nodeProcedure) {(nodeProcedure_t) outputPartial}, (nodeProcedure) {(nodeProcedure_t) doNone}, (nodeProcedure) {(nodeProcedure_t) doCompletePartialC}, (nodeProcedure) {(nodeProcedure_t) doNone});
  includeVarProcedure (s);
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstC}, (nodeProcedure) {(nodeProcedure_t) doTypesC}, (nodeProcedure) {(nodeProcedure_t) doVarC}, (nodeProcedure) {(nodeProcedure_t) outputPartial}, (nodeProcedure) {(nodeProcedure_t) doNone}, (nodeProcedure) {(nodeProcedure_t) doCompletePartialC}, (nodeProcedure) {(nodeProcedure_t) doNone});
}

static void doStatementSequenceC (mcPretty_pretty p, decl_node s)
{
  unsigned int i;
  unsigned int h;

  mcDebug_assert (decl_isStatementSequence (s));
  h = Indexing_HighIndice (s->stmtF.statements);
  i = 1;
  while (i <= h)
    {
      doStatementsC (p, (decl_node) Indexing_GetIndice (s->stmtF.statements, i));
      i += 1;
    }
}

static unsigned int isStatementSequenceEmpty (decl_node s)
{
  mcDebug_assert (decl_isStatementSequence (s));
  return (Indexing_HighIndice (s->stmtF.statements)) == 0;
}

static unsigned int isSingleStatement (decl_node s)
{
  unsigned int h;

  mcDebug_assert (decl_isStatementSequence (s));
  h = Indexing_HighIndice (s->stmtF.statements);
  if ((h == 0) || (h > 1))
    return FALSE;
  s = Indexing_GetIndice (s->stmtF.statements, 1);
  return (! (decl_isStatementSequence (s))) || (isSingleStatement (s));
}

static void doCommentC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (isComment (s));
  outText (p, (char *) "/* ", 3);
  outTextS (p, s->commentF.content);
  outText (p, (char *) "  */\\n", 6);
}

static void doReturnC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (decl_isReturn (s));
  outText (p, (char *) "return", 6);
  if (s->returnF.exp != NULL)
    {
      mcPretty_setNeedSpace (p);
      doExprC (p, s->returnF.exp);
    }
  outText (p, (char *) ";\\n", 3);
}

static void doAssignmentC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (isAssignment (s));
  doExprC (p, s->assignmentF.des);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "=", 1);
  mcPretty_setNeedSpace (p);
  doExprC (p, s->assignmentF.expr);
  outText (p, (char *) ";\\n", 3);
}

static unsigned int containsStatement (decl_node s)
{
  return ((s != NULL) && (decl_isStatementSequence (s))) && (! (isStatementSequenceEmpty (s)));
}

static void doCompoundStmt (mcPretty_pretty p, decl_node s)
{
  if ((s == NULL) || ((decl_isStatementSequence (s)) && (isStatementSequenceEmpty (s))))
    {
      p = mcPretty_pushPretty (p);
      mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
      outText (p, (char *) ";  /* empty.  */\\n", 18);
      p = mcPretty_popPretty (p);
    }
  else if ((decl_isStatementSequence (s)) && (isSingleStatement (s)))
    {
      p = mcPretty_pushPretty (p);
      mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
      doStatementSequenceC (p, s);
      p = mcPretty_popPretty (p);
    }
  else
    {
      p = mcPretty_pushPretty (p);
      mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
      outText (p, (char *) "{\\n", 3);
      p = mcPretty_pushPretty (p);
      mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
      doStatementSequenceC (p, s);
      p = mcPretty_popPretty (p);
      outText (p, (char *) "}\\n", 3);
      p = mcPretty_popPretty (p);
    }
}

static void doElsifC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (decl_isElsif (s));
  outText (p, (char *) "else if", 7);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(", 1);
  doExprC (p, s->elsifF.expr);
  outText (p, (char *) ")\\n", 3);
  doCompoundStmt (p, s->elsifF.then);
  mcDebug_assert ((s->elsifF.else_ == NULL) || (s->elsifF.elsif == NULL));
  if (containsStatement (s->elsifF.else_))
    {
      outText (p, (char *) "else\\n", 6);
      doCompoundStmt (p, s->elsifF.else_);
    }
  else if ((s->elsifF.elsif != NULL) && (decl_isElsif (s->elsifF.elsif)))
    doElsifC (p, s->elsifF.elsif);
}

static unsigned int noElse (decl_node n)
{
  if (n != NULL)
    if (decl_isStatementSequence (n))
      if (isStatementSequenceEmpty (n))
        return FALSE;
      else if (isSingleStatement (n))
        {
          n = Indexing_GetIndice (n->stmtF.statements, 1);
          return (((n != NULL) && (decl_isIf (n))) && (n->ifF.else_ == NULL)) && (n->ifF.elsif == NULL);
        }
  return FALSE;
}

static void doIfC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (decl_isIf (s));
  outText (p, (char *) "if", 2);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(", 1);
  doExprC (p, s->ifF.expr);
  outText (p, (char *) ")\\n", 3);
  if ((noElse (s->ifF.then)) && ((containsStatement (s->ifF.else_)) || (containsStatement (s->ifF.elsif))))
    {
      outText (p, (char *) "{\\n", 3);
      p = mcPretty_pushPretty (p);
      mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
      outText (p, (char *) "/* avoid dangling else.  */\\n", 29);
      doStatementSequenceC (p, s->ifF.then);
      p = mcPretty_popPretty (p);
      outText (p, (char *) "}\\n", 3);
    }
  else
    doCompoundStmt (p, s->ifF.then);
  mcDebug_assert ((s->ifF.else_ == NULL) || (s->ifF.elsif == NULL));
  if (containsStatement (s->ifF.else_))
    {
      outText (p, (char *) "else\\n", 6);
      doCompoundStmt (p, s->ifF.else_);
    }
  else if ((s->ifF.elsif != NULL) && (decl_isElsif (s->ifF.elsif)))
    doElsifC (p, s->ifF.elsif);
}

static void doForC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (decl_isFor (s));
  outText (p, (char *) "for (", 5);
  doExprC (p, s->forF.des);
  outText (p, (char *) "=", 1);
  doExprC (p, s->forF.start);
  outText (p, (char *) ";", 1);
  mcPretty_setNeedSpace (p);
  doExprC (p, s->forF.des);
  outText (p, (char *) "<=", 2);
  doExprC (p, s->forF.end);
  outText (p, (char *) ";", 1);
  mcPretty_setNeedSpace (p);
  if (s->forF.increment == NULL)
    {
      doExprC (p, s->forF.des);
      outText (p, (char *) "++", 2);
    }
  else
    {
      doExprC (p, s->forF.des);
      outText (p, (char *) "=", 1);
      doExprC (p, s->forF.des);
      outText (p, (char *) "+", 1);
      doExprC (p, s->forF.increment);
    }
  outText (p, (char *) ")\\n", 3);
  doCompoundStmt (p, s->forF.statements);
}

static void doRepeatC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (decl_isRepeat (s));
  outText (p, (char *) "do {\\n", 6);
  p = mcPretty_pushPretty (p);
  mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
  doStatementSequenceC (p, s->repeatF.statements);
  p = mcPretty_popPretty (p);
  outText (p, (char *) "} while (! (", 12);
  doExprC (p, s->repeatF.expr);
  outText (p, (char *) "));\\n", 5);
}

static void doWhileC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (decl_isWhile (s));
  outText (p, (char *) "while (", 7);
  doExprC (p, s->whileF.expr);
  outText (p, (char *) ")\\n", 3);
  doCompoundStmt (p, s->whileF.statements);
}

static void doFuncHighC (mcPretty_pretty p, decl_node a)
{
  decl_node s;
  decl_node n;

  if ((decl_isLiteral (a)) && ((decl_getType (a)) == charN))
    outCard (p, 1);
  else if (isString (a))
    outCard (p, a->stringF.length-2);
  else if (decl_isUnbounded (decl_getType (a)))
    {
      outText (p, (char *) "_", 1);
      outTextN (p, decl_getSymName (a));
      outText (p, (char *) "_high", 5);
    }
  else if (decl_isArray (decl_skipType (decl_getType (a))))
    {
      n = decl_skipType (decl_getType (a));
      s = n->arrayF.subr;
      if (isZero (getMin (s)))
        doExprC (p, getMax (s));
      else
        {
          outText (p, (char *) "(", 1);
          doExprC (p, getMax (s));
          doSubtractC (p, getMin (s));
          outText (p, (char *) ")", 1);
        }
    }
  else
    {
      outText (p, (char *) "sizeof", 6);
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "(", 1);
      doExprC (p, a);
      outText (p, (char *) ")", 1);
    }
}

static void doMultiplyBySize (mcPretty_pretty p, decl_node a)
{
  if (((a != charN) && (a != byteN)) && (a != locN))
    {
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "* sizeof (", 10);
      doTypeNameC (p, a);
      mcPretty_noSpace (p);
      outText (p, (char *) ")", 1);
    }
}

static void doTotype (mcPretty_pretty p, decl_node a, decl_node t)
{
  if ((! (isString (a))) && (! (decl_isLiteral (a))))
    if (decl_isVar (a))
      {
        a = decl_getType (a);
        if (decl_isArray (a))
          doMultiplyBySize (p, decl_skipType (decl_getType (a)));
      }
  if (t == wordN)
    {
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "/ sizeof (", 10);
      doTypeNameC (p, wordN);
      mcPretty_noSpace (p);
      outText (p, (char *) ")", 1);
    }
}

static void doFuncUnbounded (mcPretty_pretty p, decl_node actual, decl_node formal, decl_node func)
{
  decl_node h;
  DynamicStrings_String s;

  mcDebug_assert (decl_isUnbounded (formal));
  outText (p, (char *) "(", 1);
  doTypeC (p, decl_getType (formal), &formal);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "*)", 2);
  mcPretty_setNeedSpace (p);
  if ((decl_isLiteral (actual)) && ((decl_getType (actual)) == charN))
    {
      outText (p, (char *) "\"\\0", 3);
      s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (actual->literalF.name));
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, -1);
      outTextS (p, s);
      outText (p, (char *) "\"", 1);
      s = DynamicStrings_KillString (s);
    }
  else if (isString (actual))
    outCstring (p, actual, TRUE);
  else if (decl_isUnbounded (decl_getType (actual)))
    doFQNameC (p, actual);
  else
    {
      outText (p, (char *) "&", 1);
      doExprC (p, actual);
      if (decl_isArray (decl_skipType (decl_getType (actual))))
        outText (p, (char *) ".array[0]", 9);
    }
  if (! (isDefForC (decl_getScope (func))))
    {
      outText (p, (char *) ",", 1);
      mcPretty_setNeedSpace (p);
      doFuncHighC (p, actual);
      doTotype (p, actual, formal);
    }
}

static void doProcedureParamC (mcPretty_pretty p, decl_node actual, decl_node formal)
{
  outText (p, (char *) "(", 1);
  doTypeNameC (p, decl_getType (formal));
  outText (p, (char *) ")", 1);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "{", 1);
  outText (p, (char *) "(", 1);
  doFQNameC (p, decl_getType (formal));
  outText (p, (char *) "_t)", 3);
  mcPretty_setNeedSpace (p);
  doExprC (p, actual);
  outText (p, (char *) "}", 1);
}

static void doAdrExprC (mcPretty_pretty p, decl_node n)
{
  if (isDeref (n))
    doExprC (p, n->unaryF.arg);
  else if ((decl_isVar (n)) && n->varF.isVarParameter)
    doFQNameC (p, n);
  else
    {
      outText (p, (char *) "&", 1);
      doExprC (p, n);
    }
}

static unsigned int typePair (decl_node a, decl_node b, decl_node x, decl_node y)
{
  return ((a == x) && (b == y)) || ((a == y) && (b == x));
}

static unsigned int needsCast (decl_node at, decl_node ft)
{
  if ((((((((((((at == nilN) || (at == ft)) || (typePair (at, ft, cardinalN, wordN))) || (typePair (at, ft, cardinalN, ztypeN))) || (typePair (at, ft, integerN, ztypeN))) || (typePair (at, ft, longcardN, ztypeN))) || (typePair (at, ft, shortcardN, ztypeN))) || (typePair (at, ft, longintN, ztypeN))) || (typePair (at, ft, shortintN, ztypeN))) || (typePair (at, ft, realN, rtypeN))) || (typePair (at, ft, longrealN, rtypeN))) || (typePair (at, ft, shortrealN, rtypeN)))
    return FALSE;
  else
    return TRUE;
}

static void checkSystemCast (mcPretty_pretty p, decl_node actual, decl_node formal)
{
  decl_node at;
  decl_node ft;

  at = getExprType (actual);
  ft = decl_getType (formal);
  if (needsCast (at, ft))
    {
      outText (p, (char *) "(", 1);
      doTypeNameC (p, ft);
      outText (p, (char *) ")", 1);
      mcPretty_setNeedSpace (p);
    }
}

static void doFuncParamC (mcPretty_pretty p, decl_node actual, decl_node formal, decl_node func)
{
  decl_node ft;
  decl_node at;

  if (formal == NULL)
    doExprC (p, actual);
  else
    {
      ft = decl_skipType (decl_getType (formal));
      if (decl_isUnbounded (ft))
        doFuncUnbounded (p, actual, ft, func);
      else
        if (((ft == procN) || (decl_isProcType (ft))) && (decl_isProcedure (actual)))
          if (decl_isVarParam (formal))
            mcMetaError_metaError1 ((char *) "{%1MDad} cannot be passed as a VAR parameter", 44, (unsigned char *) &actual, sizeof (actual));
          else
            doProcedureParamC (p, actual, formal);
        else if (((decl_isVar (actual)) && (decl_isProcType (decl_skipType (decl_getType (actual))))) && ((decl_getType (actual)) != (decl_getType (formal))))
          if (decl_isVarParam (formal))
            mcMetaError_metaError2 ((char *) "{%1MDad} cannot be passed as a VAR parameter as the parameter requires a cast to the formal type {%2MDtad}", 106, (unsigned char *) &actual, sizeof (actual), (unsigned char *) &formal, sizeof (formal));
          else
            doCastC (p, decl_getType (formal), actual);
        else
          {
            checkSystemCast (p, actual, formal);
            if (decl_isVarParam (formal))
              doAdrExprC (p, actual);
            else
              doExprC (p, actual);
          }
    }
}

static decl_node getNthParamType (Indexing_Index l, unsigned int i)
{
  decl_node p;

  p = getNthParam (l, i);
  if (p != NULL)
    return decl_getType (p);
  return NULL;
}

static decl_node getNthParam (Indexing_Index l, unsigned int i)
{
  decl_node p;
  unsigned int j;
  unsigned int k;
  unsigned int h;

  if (l != NULL)
    {
      j = Indexing_LowIndice (l);
      h = Indexing_HighIndice (l);
      while (j <= h)
        {
          p = Indexing_GetIndice (l, j);
          if (decl_isParam (p))
            k = identListLen (p->paramF.namelist);
          else if (decl_isVarParam (p))
            k = identListLen (p->varparamF.namelist);
          else
            {
              mcDebug_assert (decl_isVarargs (p));
              return NULL;
            }
          if (i <= k)
            return p;
          else
            {
              i -= k;
              j += 1;
            }
        }
    }
  return NULL;
}

static void doFuncArgsC (mcPretty_pretty p, decl_node s, Indexing_Index l, unsigned int needParen)
{
  decl_node actual;
  decl_node formal;
  unsigned int i;
  unsigned int n;

  if (needParen)
    outText (p, (char *) "(", 1);
  if (s->funccallF.args != NULL)
    {
      i = 1;
      n = expListLen (s->funccallF.args);
      while (i <= n)
        {
          actual = getExpList (s->funccallF.args, i);
          formal = getNthParam (l, i);
          doFuncParamC (p, actual, formal, s->funccallF.function);
          if (i < n)
            {
              outText (p, (char *) ",", 1);
              mcPretty_setNeedSpace (p);
            }
          i += 1;
        }
    }
  if (needParen)
    {
      mcPretty_noSpace (p);
      outText (p, (char *) ")", 1);
    }
}

static void doProcTypeArgsC (mcPretty_pretty p, decl_node s, Indexing_Index args, unsigned int needParen)
{
  decl_node a;
  decl_node b;
  unsigned int i;
  unsigned int n;

  if (needParen)
    outText (p, (char *) "(", 1);
  if (s->funccallF.args != NULL)
    {
      i = 1;
      n = expListLen (s->funccallF.args);
      while (i <= n)
        {
          a = getExpList (s->funccallF.args, i);
          b = Indexing_GetIndice (args, i);
          doFuncParamC (p, a, b, s->funccallF.function);
          if (i < n)
            {
              outText (p, (char *) ",", 1);
              mcPretty_setNeedSpace (p);
            }
          i += 1;
        }
    }
  if (needParen)
    {
      mcPretty_noSpace (p);
      outText (p, (char *) ")", 1);
    }
}

static void doAdrArgC (mcPretty_pretty p, decl_node n)
{
  if (isDeref (n))
    doExprC (p, n->unaryF.arg);
  else if ((decl_isVar (n)) && n->varF.isVarParameter)
    outTextN (p, decl_getSymName (n));
  else
    {
      if (! (isString (n)))
        outText (p, (char *) "&", 1);
      doExprC (p, n);
    }
}

static void doAdrC (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args != NULL)
    if ((expListLen (n->funccallF.args)) == 1)
      doAdrArgC (p, getExpList (n->funccallF.args, 1));
}

static void doIncDecC (mcPretty_pretty p, decl_node n, char *op_, unsigned int _op_high)
{
  char op[_op_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (op, op_, _op_high+1);

  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args != NULL)
    {
      doExprC (p, getExpList (n->funccallF.args, 1));
      mcPretty_setNeedSpace (p);
      outText (p, (char *) op, _op_high);
      mcPretty_setNeedSpace (p);
      if ((expListLen (n->funccallF.args)) == 1)
        outText (p, (char *) "1", 1);
      else
        doExprC (p, getExpList (n->funccallF.args, 2));
    }
}

static void doInclC (mcPretty_pretty p, decl_node n)
{
  decl_node lo;

  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args != NULL)
    if ((expListLen (n->funccallF.args)) == 2)
      {
        doExprC (p, getExpList (n->funccallF.args, 1));
        lo = getSetLow (getExpList (n->funccallF.args, 1));
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "|=", 2);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "(1", 2);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "<<", 2);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "(", 1);
        doExprC (p, getExpList (n->funccallF.args, 2));
        doSubtractC (p, lo);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "))", 2);
      }
    else
      M2RTS_HALT (0);
}

static void doExclC (mcPretty_pretty p, decl_node n)
{
  decl_node lo;

  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args != NULL)
    if ((expListLen (n->funccallF.args)) == 2)
      {
        doExprC (p, getExpList (n->funccallF.args, 1));
        lo = getSetLow (getExpList (n->funccallF.args, 1));
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "&=", 2);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "(~(1", 4);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "<<", 2);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "(", 1);
        doExprC (p, getExpList (n->funccallF.args, 2));
        doSubtractC (p, lo);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) ")))", 3);
      }
    else
      M2RTS_HALT (0);
}

static void doNewC (mcPretty_pretty p, decl_node n)
{
  decl_node t;

  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args == NULL)
    M2RTS_HALT (0);
  else
    if ((expListLen (n->funccallF.args)) == 1)
      {
        keyc_useStorage ();
        outText (p, (char *) "Storage_ALLOCATE", 16);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "((void **)", 10);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "&", 1);
        doExprC (p, getExpList (n->funccallF.args, 1));
        outText (p, (char *) ",", 1);
        mcPretty_setNeedSpace (p);
        t = decl_skipType (decl_getType (getExpList (n->funccallF.args, 1)));
        if (decl_isPointer (t))
          {
            t = decl_getType (t);
            outText (p, (char *) "sizeof", 6);
            mcPretty_setNeedSpace (p);
            outText (p, (char *) "(", 1);
            doTypeNameC (p, t);
            mcPretty_noSpace (p);
            outText (p, (char *) "))", 2);
          }
        else
          mcMetaError_metaError1 ((char *) "expecting a pointer type variable as the argument to NEW, rather than {%1ad}", 76, (unsigned char *) &t, sizeof (t));
      }
}

static void doDisposeC (mcPretty_pretty p, decl_node n)
{
  decl_node t;

  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args == NULL)
    M2RTS_HALT (0);
  else
    if ((expListLen (n->funccallF.args)) == 1)
      {
        keyc_useStorage ();
        outText (p, (char *) "Storage_DEALLOCATE", 18);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "((void **)", 10);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "&", 1);
        doExprC (p, getExpList (n->funccallF.args, 1));
        outText (p, (char *) ",", 1);
        mcPretty_setNeedSpace (p);
        t = decl_skipType (decl_getType (getExpList (n->funccallF.args, 1)));
        if (decl_isPointer (t))
          {
            t = decl_getType (t);
            outText (p, (char *) "sizeof", 6);
            mcPretty_setNeedSpace (p);
            outText (p, (char *) "(", 1);
            doTypeNameC (p, t);
            mcPretty_noSpace (p);
            outText (p, (char *) "))", 2);
          }
        else
          mcMetaError_metaError1 ((char *) "expecting a pointer type variable as the argument to DISPOSE, rather than {%1ad}", 80, (unsigned char *) &t, sizeof (t));
      }
    else
      M2RTS_HALT (0);
}

static void doAbsC (mcPretty_pretty p, decl_node n)
{
  decl_node t;

  mcDebug_assert (isFuncCall (n));
  if ((n->funccallF.args != NULL) && ((expListLen (n->funccallF.args)) == 1))
    t = getExprType (n);
  else
    M2RTS_HALT (0);
  if (t == longintN)
    {
      keyc_useLabs ();
      outText (p, (char *) "labs", 4);
    }
  else if (t == integerN)
    {
      keyc_useAbs ();
      outText (p, (char *) "abs", 3);
    }
  else if (t == realN)
    {
      keyc_useFabs ();
      outText (p, (char *) "fabs", 4);
    }
  else if (t == longrealN)
    {
      keyc_useFabsl ();
      outText (p, (char *) "fabsl", 5);
    }
  else
    M2RTS_HALT (0);
  mcPretty_setNeedSpace (p);
  doFuncArgsC (p, n, (Indexing_Index) NULL, TRUE);
}

static void doValC (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args == NULL)
    M2RTS_HALT (0);
  else
    if ((expListLen (n->funccallF.args)) == 2)
      {
        outText (p, (char *) "(", 1);
        doTypeNameC (p, getExpList (n->funccallF.args, 1));
        outText (p, (char *) ")", 1);
        mcPretty_setNeedSpace (p);
        outText (p, (char *) "(", 1);
        doExprC (p, getExpList (n->funccallF.args, 2));
        outText (p, (char *) ")", 1);
      }
    else
      M2RTS_HALT (0);
}

static void doMinC (mcPretty_pretty p, decl_node n)
{
  decl_node t;
  decl_node a;

  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args == NULL)
    M2RTS_HALT (0);
  else
    if ((expListLen (n->funccallF.args)) == 1)
      {
        a = getExpList (n->funccallF.args, 1);
        t = getExprType (a);
        doExprC (p, getMin (t));
      }
    else
      M2RTS_HALT (0);
}

static void doMaxC (mcPretty_pretty p, decl_node n)
{
  decl_node t;
  decl_node a;

  mcDebug_assert (isFuncCall (n));
  if (n->funccallF.args == NULL)
    M2RTS_HALT (0);
  else
    if ((expListLen (n->funccallF.args)) == 1)
      {
        a = getExpList (n->funccallF.args, 1);
        t = getExprType (a);
        doExprC (p, getMax (t));
      }
    else
      M2RTS_HALT (0);
}

static unsigned int isIntrinsic (decl_node n)
{
  switch (n->funccallF.function->kind)
    {
      case halt:
      case max:
      case min:
      case cast:
      case val:
      case adr:
      case size:
      case tsize:
      case float_:
      case trunc:
      case ord:
      case chr:
      case abs_:
      case high:
      case inc:
      case dec:
      case incl:
      case excl:
      case new:
      case dispose:
      case throw:
        return TRUE;
        break;


      default:
        return (isFuncCall (n)) && (n->funccallF.function == haltN);
        break;
    }
}

static void doHalt (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (isFuncCall (n));
  if ((n->funccallF.args == NULL) || ((expListLen (n->funccallF.args)) == 0))
    {
      outText (p, (char *) "M2RTS_HALT", 10);
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "(0)", 3);
    }
  else if ((expListLen (n->funccallF.args)) == 1)
    {
      outText (p, (char *) "M2RTS_HALT", 10);
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "(", 1);
      doExprC (p, getExpList (n->funccallF.args, 1));
      outText (p, (char *) ")", 1);
    }
}

static void doIntrinsicC (mcPretty_pretty p, decl_node n)
{
  if (n->funccallF.function == haltN)
    doHalt (p, n);
  else
    switch (n->funccallF.function->kind)
      {
        case halt:
          doHalt (p, n);
          break;

        case val:
          doValC (p, n);
          break;

        case adr:
          doAdrC (p, n);
          break;

        case size:
        case tsize:
          outText (p, (char *) "sizeof", 6);
          mcPretty_setNeedSpace (p);
          doFuncArgsC (p, n, (Indexing_Index) NULL, TRUE);
          break;

        case float_:
          outText (p, (char *) "(double)", 8);
          mcPretty_setNeedSpace (p);
          doFuncArgsC (p, n, (Indexing_Index) NULL, TRUE);
          break;

        case trunc:
          outText (p, (char *) "(int)", 5);
          mcPretty_setNeedSpace (p);
          doFuncArgsC (p, n, (Indexing_Index) NULL, TRUE);
          break;

        case ord:
          outText (p, (char *) "(unsigned int)", 14);
          mcPretty_setNeedSpace (p);
          doFuncArgsC (p, n, (Indexing_Index) NULL, TRUE);
          break;

        case chr:
          outText (p, (char *) "(char)", 6);
          mcPretty_setNeedSpace (p);
          doFuncArgsC (p, n, (Indexing_Index) NULL, TRUE);
          break;

        case abs_:
          doAbsC (p, n);
          break;

        case high:
          doFuncHighC (p, getExpList (n->funccallF.args, 1));
          break;

        case inc:
          doIncDecC (p, n, (char *) "+=", 2);
          break;

        case dec:
          doIncDecC (p, n, (char *) "-=", 2);
          break;

        case incl:
          doInclC (p, n);
          break;

        case excl:
          doExclC (p, n);
          break;

        case new:
          doNewC (p, n);
          break;

        case dispose:
          doDisposeC (p, n);
          break;

        case min:
          doMinC (p, n);
          break;

        case max:
          doMaxC (p, n);
          break;

        case throw:
          doThrowC (p, n);
          break;


        default:
          CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
      }
}

static decl_node getFuncFromExpr (decl_node n)
{
  n = decl_skipType (decl_getType (n));
  while ((n != procN) && (! (decl_isProcType (n))))
    n = decl_skipType (decl_getType (n));
  return n;
}

static void doFuncExprC (mcPretty_pretty p, decl_node n)
{
  decl_node t;

  mcDebug_assert (isFuncCall (n));
  if (isIntrinsic (n))
    doIntrinsicC (p, n);
  else if (decl_isProcedure (n->funccallF.function))
    {
      doFQNameC (p, n->funccallF.function);
      mcPretty_setNeedSpace (p);
      doFuncArgsC (p, n, n->funccallF.function->procedureF.parameters, TRUE);
    }
  else
    {
      outText (p, (char *) "(*", 2);
      doExprC (p, n->funccallF.function);
      outText (p, (char *) ".proc)", 6);
      mcPretty_setNeedSpace (p);
      t = getFuncFromExpr (n->funccallF.function);
      if (t == procN)
        doProcTypeArgsC (p, n, (Indexing_Index) NULL, TRUE);
      else
        {
          mcDebug_assert (decl_isProcType (t));
          doProcTypeArgsC (p, n, t->proctypeF.parameters, TRUE);
        }
    }
}

static void doFuncCallC (mcPretty_pretty p, decl_node n)
{
  doFuncExprC (p, n);
  outText (p, (char *) ";\\n", 3);
}

static void doCaseStatementC (mcPretty_pretty p, decl_node n, unsigned int needBreak)
{
  p = mcPretty_pushPretty (p);
  mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
  doStatementSequenceC (p, n);
  if (needBreak)
    outText (p, (char *) "break;\\n", 8);
  p = mcPretty_popPretty (p);
}

static void doExceptionC (mcPretty_pretty p, char *a_, unsigned int _a_high, decl_node n)
{
  unsigned int w;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  w = decl_getDeclaredMod (n);
  outText (p, (char *) a, _a_high);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(\"", 2);
  outTextS (p, mcLexBuf_findFileNameFromToken (w, 0));
  outText (p, (char *) "\",", 2);
  mcPretty_setNeedSpace (p);
  outCard (p, mcLexBuf_tokenToLineNo (w, 0));
  outText (p, (char *) ",", 1);
  mcPretty_setNeedSpace (p);
  outCard (p, mcLexBuf_tokenToColumnNo (w, 0));
  outText (p, (char *) ");\\n", 4);
}

static void doRangeListC (mcPretty_pretty p, decl_node c)
{
  decl_node r;
  unsigned int i;
  unsigned int h;

  mcDebug_assert (decl_isCaseList (c));
  i = 1;
  h = Indexing_HighIndice (c->caselistF.rangePairs);
  while (i <= h)
    {
      r = Indexing_GetIndice (c->caselistF.rangePairs, i);
      mcDebug_assert ((r->rangeF.hi == NULL) || (r->rangeF.lo == r->rangeF.hi));
      outText (p, (char *) "case", 4);
      mcPretty_setNeedSpace (p);
      doExprC (p, r->rangeF.lo);
      outText (p, (char *) ":\\n", 3);
      i += 1;
    }
}

static void doRangeIfListC (mcPretty_pretty p, decl_node e, decl_node c)
{
  decl_node r;
  unsigned int i;
  unsigned int h;

  mcDebug_assert (decl_isCaseList (c));
  i = 1;
  h = Indexing_HighIndice (c->caselistF.rangePairs);
  while (i <= h)
    {
      r = Indexing_GetIndice (c->caselistF.rangePairs, i);
      if ((r->rangeF.lo != r->rangeF.hi) && (r->rangeF.hi != NULL))
        {
          outText (p, (char *) "((", 2);
          doExprC (p, e);
          outText (p, (char *) ")", 1);
          mcPretty_setNeedSpace (p);
          outText (p, (char *) ">=", 2);
          mcPretty_setNeedSpace (p);
          doExprC (p, r->rangeF.lo);
          outText (p, (char *) ")", 1);
          mcPretty_setNeedSpace (p);
          outText (p, (char *) "&&", 2);
          mcPretty_setNeedSpace (p);
          outText (p, (char *) "((", 2);
          doExprC (p, e);
          outText (p, (char *) ")", 1);
          mcPretty_setNeedSpace (p);
          outText (p, (char *) "<=", 2);
          mcPretty_setNeedSpace (p);
          doExprC (p, r->rangeF.hi);
          outText (p, (char *) ")", 1);
        }
      else
        {
          outText (p, (char *) "((", 2);
          doExprC (p, e);
          outText (p, (char *) ")", 1);
          mcPretty_setNeedSpace (p);
          outText (p, (char *) "==", 2);
          mcPretty_setNeedSpace (p);
          doExprC (p, r->rangeF.lo);
          outText (p, (char *) ")", 1);
        }
      if (i < h)
        {
          mcPretty_setNeedSpace (p);
          outText (p, (char *) "||", 2);
          mcPretty_setNeedSpace (p);
        }
      i += 1;
    }
}

static void doCaseLabels (mcPretty_pretty p, decl_node n, unsigned int needBreak)
{
  mcDebug_assert (decl_isCaseLabelList (n));
  doRangeListC (p, n->caselabellistF.caseList);
  p = mcPretty_pushPretty (p);
  mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
  doStatementSequenceC (p, n->caselabellistF.statements);
  if (needBreak)
    outText (p, (char *) "break;\\n\\n", 10);
  p = mcPretty_popPretty (p);
}

static void doCaseLabelListC (mcPretty_pretty p, decl_node n, unsigned int haveElse)
{
  unsigned int i;
  unsigned int h;
  decl_node c;

  mcDebug_assert (decl_isCase (n));
  i = 1;
  h = Indexing_HighIndice (n->caseF.caseLabelList);
  while (i <= h)
    {
      c = Indexing_GetIndice (n->caseF.caseLabelList, i);
      doCaseLabels (p, c, ((i < h) || haveElse) || caseException);
      i += 1;
    }
}

static void doCaseIfLabels (mcPretty_pretty p, decl_node e, decl_node n, unsigned int i, unsigned int h)
{
  mcDebug_assert (decl_isCaseLabelList (n));
  if (i > 1)
    {
      outText (p, (char *) "else", 4);
      mcPretty_setNeedSpace (p);
    }
  outText (p, (char *) "if", 2);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(", 1);
  doRangeIfListC (p, e, n->caselabellistF.caseList);
  outText (p, (char *) ")\\n", 3);
  if (h == 1)
    doCompoundStmt (p, n->caselabellistF.statements);
  else
    {
      outText (p, (char *) "{\\n", 3);
      doStatementSequenceC (p, n->caselabellistF.statements);
      outText (p, (char *) "}\\n", 3);
    }
}

static void doCaseIfLabelListC (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node c;

  mcDebug_assert (decl_isCase (n));
  i = 1;
  h = Indexing_HighIndice (n->caseF.caseLabelList);
  while (i <= h)
    {
      c = Indexing_GetIndice (n->caseF.caseLabelList, i);
      doCaseIfLabels (p, n->caseF.expression, c, i, h);
      i += 1;
    }
}

static void doCaseElseC (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (decl_isCase (n));
  if (n->caseF.else_ == NULL)
  {
    /* avoid dangling else.  */
    if (caseException)
      {
        outText (p, (char *) "\\ndefault:\\n", 12);
        p = mcPretty_pushPretty (p);
        mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
        doExceptionC (p, (char *) "CaseException", 13, n);
        p = mcPretty_popPretty (p);
      }
  }
  else
    {
      outText (p, (char *) "\\ndefault:\\n", 12);
      doCaseStatementC (p, n->caseF.else_, TRUE);
    }
}

static void doCaseIfElseC (mcPretty_pretty p, decl_node n)
{
  mcDebug_assert (decl_isCase (n));
  if (n->caseF.else_ == NULL)
  {
    /* avoid dangling else.  */
    if (TRUE)
      {
        outText (p, (char *) "\\n", 2);
        outText (p, (char *) "else {\\n", 8);
        p = mcPretty_pushPretty (p);
        mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
        doExceptionC (p, (char *) "CaseException", 13, n);
        p = mcPretty_popPretty (p);
        outText (p, (char *) "}\\n", 3);
      }
  }
  else
    {
      outText (p, (char *) "\\n", 2);
      outText (p, (char *) "else {\\n", 8);
      doCaseStatementC (p, n->caseF.else_, FALSE);
      outText (p, (char *) "}\\n", 3);
    }
}

static unsigned int canUseSwitchCaseLabels (decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node r;
  decl_node l;

  mcDebug_assert (decl_isCaseLabelList (n));
  l = n->caselabellistF.caseList;
  i = 1;
  h = Indexing_HighIndice (l->caselistF.rangePairs);
  while (i <= h)
    {
      r = Indexing_GetIndice (l->caselistF.rangePairs, i);
      if ((r->rangeF.hi != NULL) && (r->rangeF.lo != r->rangeF.hi))
        return FALSE;
      i += 1;
    }
  return TRUE;
}

static unsigned int canUseSwitch (decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node c;

  mcDebug_assert (decl_isCase (n));
  i = 1;
  h = Indexing_HighIndice (n->caseF.caseLabelList);
  while (i <= h)
    {
      c = Indexing_GetIndice (n->caseF.caseLabelList, i);
      if (! (canUseSwitchCaseLabels (c)))
        return FALSE;
      i += 1;
    }
  return TRUE;
}

static void doCaseC (mcPretty_pretty p, decl_node n)
{
  unsigned int i;

  mcDebug_assert (decl_isCase (n));
  if (canUseSwitch (n))
    {
      i = mcPretty_getindent (p);
      outText (p, (char *) "switch", 6);
      mcPretty_setNeedSpace (p);
      outText (p, (char *) "(", 1);
      doExprC (p, n->caseF.expression);
      p = mcPretty_pushPretty (p);
      outText (p, (char *) ")", 1);
      mcPretty_setindent (p, i+indentationC);
      outText (p, (char *) "\\n{\\n", 5);
      p = mcPretty_pushPretty (p);
      mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
      doCaseLabelListC (p, n, n->caseF.else_ != NULL);
      doCaseElseC (p, n);
      p = mcPretty_popPretty (p);
      outText (p, (char *) "}\\n", 3);
      p = mcPretty_popPretty (p);
    }
  else
    {
      doCaseIfLabelListC (p, n);
      doCaseIfElseC (p, n);
    }
}

static void doLoopC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (decl_isLoop (s));
  outText (p, (char *) "for (;;)\\n", 10);
  outText (p, (char *) "{\\n", 3);
  p = mcPretty_pushPretty (p);
  mcPretty_setindent (p, (mcPretty_getindent (p))+indentationC);
  doStatementSequenceC (p, s->loopF.statements);
  p = mcPretty_popPretty (p);
  outText (p, (char *) "}\\n", 3);
}

static void doExitC (mcPretty_pretty p, decl_node s)
{
  mcDebug_assert (decl_isExit (s));
  outText (p, (char *) "/* exit.  */\\n", 14);
}

static void doStatementsC (mcPretty_pretty p, decl_node s)
{
  if (s == NULL)
    ;  /* empty.  */
  else if (decl_isStatementSequence (s))
    doStatementSequenceC (p, s);
  else if (isComment (s))
    doCommentC (p, s);
  else if (decl_isExit (s))
    doExitC (p, s);
  else if (decl_isReturn (s))
    doReturnC (p, s);
  else if (isAssignment (s))
    doAssignmentC (p, s);
  else if (decl_isIf (s))
    doIfC (p, s);
  else if (decl_isFor (s))
    doForC (p, s);
  else if (decl_isRepeat (s))
    doRepeatC (p, s);
  else if (decl_isWhile (s))
    doWhileC (p, s);
  else if (isFuncCall (s))
    doFuncCallC (p, s);
  else if (decl_isCase (s))
    doCaseC (p, s);
  else if (decl_isLoop (s))
    doLoopC (p, s);
  else if (decl_isExit (s))
    doExitC (p, s);
  else
    M2RTS_HALT (0);
}

static void stop (void)
{
}

static void doLocalVarC (mcPretty_pretty p, scopeT s)
{
  includeVarProcedure (s);
  debugLists ();
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstC}, (nodeProcedure) {(nodeProcedure_t) doTypesC}, (nodeProcedure) {(nodeProcedure_t) doVarC}, (nodeProcedure) {(nodeProcedure_t) outputPartial}, (nodeProcedure) {(nodeProcedure_t) doNone}, (nodeProcedure) {(nodeProcedure_t) doCompletePartialC}, (nodeProcedure) {(nodeProcedure_t) doNone});
}

static void doLocalConstTypesC (mcPretty_pretty p, scopeT s)
{
  simplifyTypes (s);
  includeConstType (s);
  doP = p;
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstC}, (nodeProcedure) {(nodeProcedure_t) doTypesC}, (nodeProcedure) {(nodeProcedure_t) doVarC}, (nodeProcedure) {(nodeProcedure_t) outputPartial}, (nodeProcedure) {(nodeProcedure_t) doNone}, (nodeProcedure) {(nodeProcedure_t) doCompletePartialC}, (nodeProcedure) {(nodeProcedure_t) doNone});
}

static void addParamDone (decl_node n)
{
  if ((decl_isVar (n)) && n->varF.isParameter)
    {
      addDone (n);
      addDone (decl_getType (n));
    }
}

static void includeParameters (decl_node n)
{
  mcDebug_assert (decl_isProcedure (n));
  Indexing_ForeachIndiceInIndexDo (n->procedureF.decls.variables, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) addParamDone});
}

static void doProcedureC (decl_node n)
{
  unsigned int s;

  outText (doP, (char *) "\\n", 2);
  includeParameters (n);
  keyc_enterScope (n);
  doProcedureHeadingC (n);
  outText (doP, (char *) "\\n", 2);
  doP = outKc (doP, (char *) "{\\n", 3);
  s = mcPretty_getcurline (doP);
  doLocalConstTypesC (doP, n->procedureF.decls);
  doLocalVarC (doP, n->procedureF.decls);
  doUnboundedParamCopyC (doP, n);
  if (s != (mcPretty_getcurline (doP)))
    outText (doP, (char *) "\\n", 2);
  doStatementsC (doP, n->procedureF.beginStatements);
  doP = outKc (doP, (char *) "}\\n", 3);
  keyc_leaveScope (n);
}

static void outProceduresC (mcPretty_pretty p, scopeT s)
{
  doP = p;
  if (debugDecl)
    libc_printf ((char *) "seen %d procedures\\n", 20, Indexing_HighIndice (s.procedures));
  Indexing_ForeachIndiceInIndexDo (s.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doProcedureC});
}

static void output (decl_node n, nodeProcedure c, nodeProcedure t, nodeProcedure v)
{
  if (decl_isConst (n))
    (*c.proc) (n);
  else if (decl_isVar (n))
    (*v.proc) (n);
  else
    (*t.proc) (n);
}

static dependentState allDependants (decl_node n)
{
  alists_alist l;
  dependentState s;

  l = alists_initList ();
  s = walkDependants (l, n);
  alists_killList (&l);
  return s;
}

static dependentState walkDependants (alists_alist l, decl_node n)
{
  if ((n == NULL) || (alists_isItemInList (doneQ, (void *) n)))
    return completed;
  else if (alists_isItemInList (l, (void *) n))
    return recursive;
  else
    {
      alists_includeItemIntoList (l, (void *) n);
      return doDependants (l, n);
    }
}

static dependentState walkType (alists_alist l, decl_node n)
{
  decl_node t;

  t = decl_getType (n);
  if (alists_isItemInList (doneQ, (void *) t))
    return completed;
  else if (alists_isItemInList (partialQ, (void *) t))
    return blocked;
  else
    {
      queueBlocked (t);
      return blocked;
    }
}

static void db (char *a_, unsigned int _a_high, decl_node n)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  if (mcOptions_getDebugTopological ())
    {
      outText (doP, (char *) a, _a_high);
      if (n != NULL)
        outTextS (doP, gen (n));
    }
}

static void dbt (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  if (mcOptions_getDebugTopological ())
    outText (doP, (char *) a, _a_high);
}

static void dbs (dependentState s, decl_node n)
{
  if (mcOptions_getDebugTopological ())
    {
      switch (s)
        {
          case completed:
            outText (doP, (char *) "{completed ", 11);
            break;

          case blocked:
            outText (doP, (char *) "{blocked ", 9);
            break;

          case partial:
            outText (doP, (char *) "{partial ", 9);
            break;

          case recursive:
            outText (doP, (char *) "{recursive ", 11);
            break;


          default:
            CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
        }
      if (n != NULL)
        outTextS (doP, gen (n));
      outText (doP, (char *) "}\\n", 3);
    }
}

static void dbq (decl_node n)
{
  if (mcOptions_getDebugTopological ())
    if (alists_isItemInList (todoQ, (void *) n))
      {
        db ((char *) "{T", 2, n);
        outText (doP, (char *) "}", 1);
      }
    else if (alists_isItemInList (partialQ, (void *) n))
      {
        db ((char *) "{P", 2, n);
        outText (doP, (char *) "}", 1);
      }
    else if (alists_isItemInList (doneQ, (void *) n))
      {
        db ((char *) "{D", 2, n);
        outText (doP, (char *) "}", 1);
      }
}

static dependentState walkRecord (alists_alist l, decl_node n)
{
  dependentState s;
  unsigned int o;
  unsigned int i;
  unsigned int t;
  decl_node q;

  i = Indexing_LowIndice (n->recordF.listOfSons);
  t = Indexing_HighIndice (n->recordF.listOfSons);
  db ((char *) "\\nwalking ", 10, n);
  o = mcPretty_getindent (doP);
  mcPretty_setindent (doP, (mcPretty_getcurpos (doP))+3);
  dbq (n);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->recordF.listOfSons, i);
      db ((char *) "", 0, q);
      if ((decl_isRecordField (q)) && q->recordfieldF.tag)
        ;  /* empty.  */
      else
        {
          s = walkDependants (l, q);
          if (s != completed)
            {
              dbs (s, q);
              addTodo (n);
              dbq (n);
              db ((char *) "\\n", 2, (decl_node) NULL);
              mcPretty_setindent (doP, o);
              return s;
            }
        }
      i += 1;
    }
  db ((char *) "{completed", 10, n);
  dbt ((char *) "}\\n", 3);
  mcPretty_setindent (doP, o);
  return completed;
}

static dependentState walkVarient (alists_alist l, decl_node n)
{
  dependentState s;
  unsigned int i;
  unsigned int t;
  decl_node q;

  db ((char *) "\\nwalking", 9, n);
  s = walkDependants (l, n->varientF.tag);
  if (s != completed)
    {
      dbs (s, n->varientF.tag);
      dbq (n->varientF.tag);
      db ((char *) "\\n", 2, (decl_node) NULL);
      return s;
    }
  i = Indexing_LowIndice (n->varientF.listOfSons);
  t = Indexing_HighIndice (n->varientF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientF.listOfSons, i);
      db ((char *) "", 0, q);
      s = walkDependants (l, q);
      if (s != completed)
        {
          dbs (s, q);
          db ((char *) "\\n", 2, (decl_node) NULL);
          return s;
        }
      i += 1;
    }
  db ((char *) "{completed", 10, n);
  dbt ((char *) "}\\n", 3);
  return completed;
}

static void queueBlocked (decl_node n)
{
  if (! ((alists_isItemInList (doneQ, (void *) n)) || (alists_isItemInList (partialQ, (void *) n))))
    addTodo (n);
}

static dependentState walkVar (alists_alist l, decl_node n)
{
  decl_node t;

  t = decl_getType (n);
  if (alists_isItemInList (doneQ, (void *) t))
    return completed;
  else
    {
      queueBlocked (t);
      return blocked;
    }
}

static dependentState walkEnumeration (alists_alist l, decl_node n)
{
  dependentState s;
  unsigned int i;
  unsigned int t;
  decl_node q;

  i = Indexing_LowIndice (n->enumerationF.listOfSons);
  t = Indexing_HighIndice (n->enumerationF.listOfSons);
  s = completed;
  while (i <= t)
    {
      q = Indexing_GetIndice (n->enumerationF.listOfSons, i);
      s = walkDependants (l, q);
      if (s != completed)
        return s;
      i += 1;
    }
  return s;
}

static dependentState walkSubrange (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, n->subrangeF.low);
  if (s != completed)
    return s;
  s = walkDependants (l, n->subrangeF.high);
  if (s != completed)
    return s;
  s = walkDependants (l, n->subrangeF.type);
  if (s != completed)
    return s;
  return completed;
}

static dependentState walkSubscript (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, n->subscriptF.expr);
  if (s != completed)
    return s;
  s = walkDependants (l, n->subscriptF.type);
  if (s != completed)
    return s;
  return completed;
}

static dependentState walkPointer (alists_alist l, decl_node n)
{
  decl_node t;

  t = decl_getType (n);
  if ((alists_isItemInList (partialQ, (void *) t)) || (alists_isItemInList (doneQ, (void *) t)))
    return completed;
  return walkType (l, n);
}

static dependentState walkArray (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, n->arrayF.type);
  if (s != completed)
    return s;
  return walkDependants (l, n->arrayF.subr);
}

static dependentState walkConst (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, n->constF.type);
  if (s != completed)
    return s;
  s = walkDependants (l, n->constF.value);
  if (s != completed)
    return s;
  return completed;
}

static dependentState walkVarParam (alists_alist l, decl_node n)
{
  decl_node t;

  t = decl_getType (n);
  if (alists_isItemInList (partialQ, (void *) t))
    return completed;
  return walkDependants (l, t);
}

static dependentState walkParam (alists_alist l, decl_node n)
{
  decl_node t;

  t = decl_getType (n);
  if (alists_isItemInList (partialQ, (void *) t))
    return completed;
  return walkDependants (l, t);
}

static dependentState walkOptarg (alists_alist l, decl_node n)
{
  decl_node t;

  t = decl_getType (n);
  if (alists_isItemInList (partialQ, (void *) t))
    return completed;
  return walkDependants (l, t);
}

static dependentState walkRecordField (alists_alist l, decl_node n)
{
  decl_node t;
  dependentState s;

  mcDebug_assert (decl_isRecordField (n));
  t = decl_getType (n);
  if (alists_isItemInList (partialQ, (void *) t))
    {
      dbs ((dependentState) partial, n);
      return partial;
    }
  else if (alists_isItemInList (doneQ, (void *) t))
    {
      dbs ((dependentState) completed, n);
      return completed;
    }
  else
    {
      addTodo (t);
      dbs ((dependentState) blocked, n);
      dbq (n);
      dbq (t);
      return blocked;
    }
}

static dependentState walkVarientField (alists_alist l, decl_node n)
{
  dependentState s;
  unsigned int i;
  unsigned int t;
  decl_node q;

  i = Indexing_LowIndice (n->varientfieldF.listOfSons);
  t = Indexing_HighIndice (n->varientfieldF.listOfSons);
  s = completed;
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientfieldF.listOfSons, i);
      s = walkDependants (l, q);
      if (s != completed)
        {
          dbs (s, n);
          return s;
        }
      i += 1;
    }
  n->varientfieldF.simple = t <= 1;
  dbs (s, n);
  return s;
}

static dependentState walkEnumerationField (alists_alist l, decl_node n)
{
  return completed;
}

static dependentState walkSet (alists_alist l, decl_node n)
{
  return walkDependants (l, decl_getType (n));
}

static dependentState walkProcType (alists_alist l, decl_node n)
{
  dependentState s;
  decl_node t;

  t = decl_getType (n);
  if (alists_isItemInList (partialQ, (void *) t))
    ;  /* empty.  */
  else
    {
      s = walkDependants (l, t);
      if (s != completed)
        return s;
    }
  return walkParameters (l, n->proctypeF.parameters);
}

static dependentState walkProcedure (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, decl_getType (n));
  if (s != completed)
    return s;
  return walkParameters (l, n->procedureF.parameters);
}

static dependentState walkParameters (alists_alist l, Indexing_Index p)
{
  dependentState s;
  unsigned int i;
  unsigned int h;
  decl_node q;

  i = Indexing_LowIndice (p);
  h = Indexing_HighIndice (p);
  while (i <= h)
    {
      q = Indexing_GetIndice (p, i);
      s = walkDependants (l, q);
      if (s != completed)
        return s;
      i += 1;
    }
  return completed;
}

static dependentState walkFuncCall (alists_alist l, decl_node n)
{
  return completed;
}

static dependentState walkUnary (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, n->unaryF.arg);
  if (s != completed)
    return s;
  return walkDependants (l, n->unaryF.resultType);
}

static dependentState walkBinary (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, n->binaryF.left);
  if (s != completed)
    return s;
  s = walkDependants (l, n->binaryF.right);
  if (s != completed)
    return s;
  return walkDependants (l, n->binaryF.resultType);
}

static dependentState walkComponentRef (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, n->componentrefF.rec);
  if (s != completed)
    return s;
  s = walkDependants (l, n->componentrefF.field);
  if (s != completed)
    return s;
  return walkDependants (l, n->componentrefF.resultType);
}

static dependentState walkPointerRef (alists_alist l, decl_node n)
{
  dependentState s;

  s = walkDependants (l, n->pointerrefF.ptr);
  if (s != completed)
    return s;
  s = walkDependants (l, n->pointerrefF.field);
  if (s != completed)
    return s;
  return walkDependants (l, n->pointerrefF.resultType);
}

static dependentState doDependants (alists_alist l, decl_node n)
{
  switch (n->kind)
    {
      case throw:
      case varargs:
      case address:
      case loc:
      case byte:
      case word:
      case boolean:
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case ztype:
      case rtype:
      case proc:
        return completed;
        break;

      case type:
        return walkType (l, n);
        break;

      case record:
        return walkRecord (l, n);
        break;

      case varient:
        return walkVarient (l, n);
        break;

      case var:
        return walkVar (l, n);
        break;

      case enumeration:
        return walkEnumeration (l, n);
        break;

      case subrange:
        return walkSubrange (l, n);
        break;

      case pointer:
        return walkPointer (l, n);
        break;

      case array:
        return walkArray (l, n);
        break;

      case string:
        return completed;
        break;

      case const_:
        return walkConst (l, n);
        break;

      case literal:
        return completed;
        break;

      case varparam:
        return walkVarParam (l, n);
        break;

      case param:
        return walkParam (l, n);
        break;

      case optarg_:
        return walkOptarg (l, n);
        break;

      case recordfield:
        return walkRecordField (l, n);
        break;

      case varientfield:
        return walkVarientField (l, n);
        break;

      case enumerationfield:
        return walkEnumerationField (l, n);
        break;

      case set:
        return walkSet (l, n);
        break;

      case proctype:
        return walkProcType (l, n);
        break;

      case subscript:
        return walkSubscript (l, n);
        break;

      case procedure:
        return walkProcedure (l, n);
        break;

      case def:
      case imp:
      case module:
      case loop:
      case while_:
      case for_:
      case repeat:
      case if_:
      case elsif:
      case assignment:
        M2RTS_HALT (0);
        break;

      case componentref:
        return walkComponentRef (l, n);
        break;

      case pointerref:
        return walkPointerRef (l, n);
        break;

      case chr:
      case ord:
      case float_:
      case trunc:
      case high:
        return walkUnary (l, n);
        break;

      case cast:
      case val:
      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
        return walkBinary (l, n);
        break;

      case constexp:
      case neg:
      case adr:
      case size:
      case tsize:
      case deref:
        return walkUnary (l, n);
        break;

      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
        return walkBinary (l, n);
        break;

      case funccall:
        return walkFuncCall (l, n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static unsigned int tryComplete (decl_node n, nodeProcedure c, nodeProcedure t, nodeProcedure v)
{
  if (decl_isEnumeration (n))
    {
      output (n, c, t, v);
      return TRUE;
    }
  else if (((decl_isType (n)) && (decl_isTypeHidden (n))) && ((decl_getType (n)) == NULL))
    {
      outputHidden (n);
      return TRUE;
    }
  else if ((allDependants (n)) == completed)
    {
      output (n, c, t, v);
      return TRUE;
    }
  return FALSE;
}

static unsigned int tryCompleteFromPartial (decl_node n, nodeProcedure t)
{
  if ((((decl_isType (n)) && ((decl_getType (n)) != NULL)) && (decl_isPointer (decl_getType (n)))) && ((allDependants (decl_getType (n))) == completed))
    {
      outputHiddenComplete (n);
      return TRUE;
    }
  else if ((allDependants (n)) == completed)
    {
      (*t.proc) (n);
      return TRUE;
    }
  return FALSE;
}

static void visitUnary (alists_alist v, decl_node n, nodeProcedure p)
{
  visitNode (v, n->unaryF.arg, p);
  visitNode (v, n->unaryF.resultType, p);
}

static void visitBinary (alists_alist v, decl_node n, nodeProcedure p)
{
  visitNode (v, n->binaryF.left, p);
  visitNode (v, n->binaryF.right, p);
  visitNode (v, n->binaryF.resultType, p);
}

static void visitBoolean (alists_alist v, decl_node n, nodeProcedure p)
{
  visitNode (v, falseN, p);
  visitNode (v, trueN, p);
}

static void visitScope (alists_alist v, decl_node n, nodeProcedure p)
{
  if (mustVisitScope)
    visitNode (v, n, p);
}

static void visitType (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isType (n));
  if ((decl_getSymName (n)) == (nameKey_makeKey ((char *) "alist", 5)))
    stop ();
  visitNode (v, n->typeF.type, p);
  visitScope (v, n->typeF.scope, p);
}

static void visitIndex (alists_alist v, Indexing_Index i, nodeProcedure p)
{
  unsigned int j;
  unsigned int h;

  j = 1;
  h = Indexing_HighIndice (i);
  while (j <= h)
    {
      visitNode (v, (decl_node) Indexing_GetIndice (i, j), p);
      j += 1;
    }
}

static void visitRecord (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isRecord (n));
  visitScope (v, n->recordF.scope, p);
  visitIndex (v, n->recordF.listOfSons, p);
}

static void visitVarient (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isVarient (n));
  visitIndex (v, n->varientF.listOfSons, p);
  visitNode (v, n->varientF.varient, p);
  visitNode (v, n->varientF.tag, p);
  visitScope (v, n->varientF.scope, p);
}

static void visitVar (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isVar (n));
  visitNode (v, n->varF.type, p);
  visitNode (v, n->varF.decl, p);
  visitScope (v, n->varF.scope, p);
}

static void visitEnumeration (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isEnumeration (n));
  visitIndex (v, n->enumerationF.listOfSons, p);
  visitScope (v, n->enumerationF.scope, p);
}

static void visitSubrange (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isSubrange (n));
  visitNode (v, n->subrangeF.low, p);
  visitNode (v, n->subrangeF.high, p);
  visitNode (v, n->subrangeF.type, p);
  visitScope (v, n->subrangeF.scope, p);
}

static void visitPointer (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isPointer (n));
  visitNode (v, n->pointerF.type, p);
  visitScope (v, n->pointerF.scope, p);
}

static void visitArray (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isArray (n));
  visitNode (v, n->arrayF.subr, p);
  visitNode (v, n->arrayF.type, p);
  visitScope (v, n->arrayF.scope, p);
}

static void visitConst (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isConst (n));
  visitNode (v, n->constF.type, p);
  visitNode (v, n->constF.value, p);
  visitScope (v, n->constF.scope, p);
}

static void visitVarParam (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isVarParam (n));
  visitNode (v, n->varparamF.namelist, p);
  visitNode (v, n->varparamF.type, p);
  visitScope (v, n->varparamF.scope, p);
}

static void visitParam (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isParam (n));
  visitNode (v, n->paramF.namelist, p);
  visitNode (v, n->paramF.type, p);
  visitScope (v, n->paramF.scope, p);
}

static void visitOptarg (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isOptarg (n));
  visitNode (v, n->optargF.namelist, p);
  visitNode (v, n->optargF.type, p);
  visitNode (v, n->optargF.init, p);
  visitScope (v, n->optargF.scope, p);
}

static void visitRecordField (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isRecordField (n));
  visitNode (v, n->recordfieldF.type, p);
  visitNode (v, n->recordfieldF.parent, p);
  visitNode (v, n->recordfieldF.varient, p);
  visitScope (v, n->recordfieldF.scope, p);
}

static void visitVarientField (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isVarientField (n));
  visitNode (v, n->varientfieldF.parent, p);
  visitNode (v, n->varientfieldF.varient, p);
  visitIndex (v, n->varientfieldF.listOfSons, p);
  visitScope (v, n->varientfieldF.scope, p);
}

static void visitEnumerationField (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isEnumerationField (n));
  visitNode (v, n->enumerationfieldF.type, p);
  visitScope (v, n->enumerationfieldF.scope, p);
}

static void visitSet (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isSet (n));
  visitNode (v, n->setF.type, p);
  visitScope (v, n->setF.scope, p);
}

static void visitProcType (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isProcType (n));
  visitIndex (v, n->proctypeF.parameters, p);
  visitNode (v, n->proctypeF.optarg_, p);
  visitNode (v, n->proctypeF.returnType, p);
  visitScope (v, n->proctypeF.scope, p);
}

static void visitSubscript (alists_alist v, decl_node n, nodeProcedure p)
{
}

static void visitDecls (alists_alist v, scopeT s, nodeProcedure p)
{
  visitIndex (v, s.constants, p);
  visitIndex (v, s.types, p);
  visitIndex (v, s.procedures, p);
  visitIndex (v, s.variables, p);
}

static void visitProcedure (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isProcedure (n));
  visitDecls (v, n->procedureF.decls, p);
  visitScope (v, n->procedureF.scope, p);
  visitIndex (v, n->procedureF.parameters, p);
  visitNode (v, n->procedureF.optarg_, p);
  visitNode (v, n->procedureF.returnType, p);
  visitNode (v, n->procedureF.beginStatements, p);
}

static void visitDef (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isDef (n));
  visitDecls (v, n->defF.decls, p);
}

static void visitImp (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isImp (n));
  visitDecls (v, n->impF.decls, p);
  visitNode (v, n->impF.beginStatements, p);
  visitNode (v, n->impF.finallyStatements, p);
}

static void visitModule (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isModule (n));
  visitDecls (v, n->moduleF.decls, p);
  visitNode (v, n->moduleF.beginStatements, p);
  visitNode (v, n->moduleF.finallyStatements, p);
}

static void visitLoop (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isLoop (n));
  visitNode (v, n->loopF.statements, p);
}

static void visitWhile (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isWhile (n));
  visitNode (v, n->whileF.expr, p);
  visitNode (v, n->whileF.statements, p);
}

static void visitRepeat (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isRepeat (n));
  visitNode (v, n->repeatF.expr, p);
  visitNode (v, n->repeatF.statements, p);
}

static void visitCase (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isCase (n));
  visitNode (v, n->caseF.expression, p);
  visitIndex (v, n->caseF.caseLabelList, p);
  visitNode (v, n->caseF.else_, p);
}

static void visitCaseLabelList (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isCaseLabelList (n));
  visitNode (v, n->caselabellistF.caseList, p);
  visitNode (v, n->caselabellistF.statements, p);
}

static void visitCaseList (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isCaseList (n));
  visitIndex (v, n->caselistF.rangePairs, p);
}

static void visitRange (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isRange (n));
  visitNode (v, n->rangeF.lo, p);
  visitNode (v, n->rangeF.hi, p);
}

static void visitIf (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isIf (n));
  visitNode (v, n->ifF.expr, p);
  visitNode (v, n->ifF.elsif, p);
  visitNode (v, n->ifF.then, p);
  visitNode (v, n->ifF.else_, p);
}

static void visitElsif (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isElsif (n));
  visitNode (v, n->elsifF.expr, p);
  visitNode (v, n->elsifF.elsif, p);
  visitNode (v, n->elsifF.then, p);
  visitNode (v, n->elsifF.else_, p);
}

static void visitFor (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isFor (n));
  visitNode (v, n->forF.des, p);
  visitNode (v, n->forF.start, p);
  visitNode (v, n->forF.end, p);
  visitNode (v, n->forF.increment, p);
  visitNode (v, n->forF.statements, p);
}

static void visitAssignment (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (isAssignment (n));
  visitNode (v, n->assignmentF.des, p);
  visitNode (v, n->assignmentF.expr, p);
}

static void visitComponentRef (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (isComponentRef (n));
  visitNode (v, n->componentrefF.rec, p);
  visitNode (v, n->componentrefF.field, p);
  visitNode (v, n->componentrefF.resultType, p);
}

static void visitPointerRef (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isPointerRef (n));
  visitNode (v, n->pointerrefF.ptr, p);
  visitNode (v, n->pointerrefF.field, p);
  visitNode (v, n->pointerrefF.resultType, p);
}

static void visitArrayRef (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (isArrayRef (n));
  visitNode (v, n->arrayrefF.array, p);
  visitNode (v, n->arrayrefF.index, p);
  visitNode (v, n->arrayrefF.resultType, p);
}

static void visitFunccall (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (isFuncCall (n));
  visitNode (v, n->funccallF.function, p);
  visitNode (v, n->funccallF.args, p);
  visitNode (v, n->funccallF.type, p);
}

static void visitVarDecl (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (isVarDecl (n));
  visitNode (v, n->vardeclF.type, p);
  visitScope (v, n->vardeclF.scope, p);
}

static void visitExplist (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isExpList (n));
  visitIndex (v, n->explistF.exp, p);
}

static void visitExit (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isExit (n));
  visitNode (v, n->exitF.loop, p);
}

static void visitReturn (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isReturn (n));
  visitNode (v, n->returnF.exp, p);
}

static void visitStmtSeq (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isStatementSequence (n));
  visitIndex (v, n->stmtF.statements, p);
}

static void visitVarargs (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isVarargs (n));
  visitScope (v, n->varargsF.scope, p);
}

static void visitSetValue (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (decl_isSetValue (n));
  visitNode (v, n->setvalueF.type, p);
  visitIndex (v, n->setvalueF.values, p);
}

static void visitDependants (alists_alist v, decl_node n, nodeProcedure p)
{
  mcDebug_assert (n != NULL);
  mcDebug_assert (alists_isItemInList (v, (void *) n));
  switch (n->kind)
    {
      case explist:
        visitExplist (v, n, p);
        break;

      case funccall:
        visitFunccall (v, n, p);
        break;

      case exit_:
        visitExit (v, n, p);
        break;

      case return_:
        visitReturn (v, n, p);
        break;

      case stmtseq:
        visitStmtSeq (v, n, p);
        break;

      case halt:
        break;

      case new:
        break;

      case dispose:
        break;

      case inc:
        break;

      case dec:
        break;

      case incl:
        break;

      case excl:
        break;

      case boolean:
        visitBoolean (v, n, p);
        break;

      case nil:
      case false:
      case true:
        break;

      case varargs:
        visitVarargs (v, n, p);
        break;

      case address:
      case loc:
      case byte:
      case word:
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case ztype:
      case rtype:
      case proc:
        break;

      case type:
        visitType (v, n, p);
        break;

      case record:
        visitRecord (v, n, p);
        break;

      case varient:
        visitVarient (v, n, p);
        break;

      case var:
        visitVar (v, n, p);
        break;

      case enumeration:
        visitEnumeration (v, n, p);
        break;

      case subrange:
        visitSubrange (v, n, p);
        break;

      case pointer:
        visitPointer (v, n, p);
        break;

      case array:
        visitArray (v, n, p);
        break;

      case string:
        break;

      case const_:
        visitConst (v, n, p);
        break;

      case literal:
        break;

      case varparam:
        visitVarParam (v, n, p);
        break;

      case param:
        visitParam (v, n, p);
        break;

      case optarg_:
        visitOptarg (v, n, p);
        break;

      case recordfield:
        visitRecordField (v, n, p);
        break;

      case varientfield:
        visitVarientField (v, n, p);
        break;

      case enumerationfield:
        visitEnumerationField (v, n, p);
        break;

      case set:
        visitSet (v, n, p);
        break;

      case proctype:
        visitProcType (v, n, p);
        break;

      case subscript:
        visitSubscript (v, n, p);
        break;

      case procedure:
        visitProcedure (v, n, p);
        break;

      case def:
        visitDef (v, n, p);
        break;

      case imp:
        visitImp (v, n, p);
        break;

      case module:
        visitModule (v, n, p);
        break;

      case loop:
        visitLoop (v, n, p);
        break;

      case while_:
        visitWhile (v, n, p);
        break;

      case for_:
        visitFor (v, n, p);
        break;

      case repeat:
        visitRepeat (v, n, p);
        break;

      case case_:
        visitCase (v, n, p);
        break;

      case caselabellist:
        visitCaseLabelList (v, n, p);
        break;

      case caselist:
        visitCaseList (v, n, p);
        break;

      case range:
        visitRange (v, n, p);
        break;

      case if_:
        visitIf (v, n, p);
        break;

      case elsif:
        visitElsif (v, n, p);
        break;

      case assignment:
        visitAssignment (v, n, p);
        break;

      case componentref:
        visitComponentRef (v, n, p);
        break;

      case pointerref:
        visitPointerRef (v, n, p);
        break;

      case arrayref:
        visitArrayRef (v, n, p);
        break;

      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
      case and:
      case or:
      case in:
      case cast:
      case val:
      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
        visitBinary (v, n, p);
        break;

      case abs_:
      case chr:
      case high:
      case ord:
      case float_:
      case trunc:
      case not:
      case neg:
      case adr:
      case size:
      case tsize:
      case min:
      case max:
      case throw:
      case constexp:
      case deref:
        visitUnary (v, n, p);
        break;

      case identlist:
        break;

      case vardecl:
        visitVarDecl (v, n, p);
        break;

      case setvalue:
        visitSetValue (v, n, p);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static void visitNode (alists_alist v, decl_node n, nodeProcedure p)
{
  if ((n != NULL) && (! (alists_isItemInList (v, (void *) n))))
    {
      alists_includeItemIntoList (v, (void *) n);
      (*p.proc) (n);
      visitDependants (v, n, p);
    }
}

static DynamicStrings_String genKind (decl_node n)
{
  switch (n->kind)
    {
      case nil:
      case true:
      case false:
      case address:
      case loc:
      case byte:
      case word:
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case boolean:
      case proc:
      case ztype:
      case rtype:
        return NULL;
        break;

      case type:
        return DynamicStrings_InitString ((char *) "type", 4);
        break;

      case record:
        return DynamicStrings_InitString ((char *) "record", 6);
        break;

      case varient:
        return DynamicStrings_InitString ((char *) "varient", 7);
        break;

      case var:
        return DynamicStrings_InitString ((char *) "var", 3);
        break;

      case enumeration:
        return DynamicStrings_InitString ((char *) "enumeration", 11);
        break;

      case subrange:
        return DynamicStrings_InitString ((char *) "subrange", 8);
        break;

      case array:
        return DynamicStrings_InitString ((char *) "array", 5);
        break;

      case subscript:
        return DynamicStrings_InitString ((char *) "subscript", 9);
        break;

      case string:
        return DynamicStrings_InitString ((char *) "string", 6);
        break;

      case const_:
        return DynamicStrings_InitString ((char *) "const", 5);
        break;

      case literal:
        return DynamicStrings_InitString ((char *) "literal", 7);
        break;

      case varparam:
        return DynamicStrings_InitString ((char *) "varparam", 8);
        break;

      case param:
        return DynamicStrings_InitString ((char *) "param", 5);
        break;

      case varargs:
        return DynamicStrings_InitString ((char *) "varargs", 7);
        break;

      case pointer:
        return DynamicStrings_InitString ((char *) "pointer", 7);
        break;

      case recordfield:
        return DynamicStrings_InitString ((char *) "recordfield", 11);
        break;

      case varientfield:
        return DynamicStrings_InitString ((char *) "varientfield", 12);
        break;

      case enumerationfield:
        return DynamicStrings_InitString ((char *) "enumerationfield", 16);
        break;

      case set:
        return DynamicStrings_InitString ((char *) "set", 3);
        break;

      case proctype:
        return DynamicStrings_InitString ((char *) "proctype", 8);
        break;

      case procedure:
        return DynamicStrings_InitString ((char *) "procedure", 9);
        break;

      case def:
        return DynamicStrings_InitString ((char *) "def", 3);
        break;

      case imp:
        return DynamicStrings_InitString ((char *) "imp", 3);
        break;

      case module:
        return DynamicStrings_InitString ((char *) "module", 6);
        break;

      case loop:
        return DynamicStrings_InitString ((char *) "loop", 4);
        break;

      case while_:
        return DynamicStrings_InitString ((char *) "while", 5);
        break;

      case for_:
        return DynamicStrings_InitString ((char *) "for", 3);
        break;

      case repeat:
        return DynamicStrings_InitString ((char *) "repeat", 6);
        break;

      case assignment:
        return DynamicStrings_InitString ((char *) "assignment", 10);
        break;

      case call:
        return DynamicStrings_InitString ((char *) "call", 4);
        break;

      case if_:
        return DynamicStrings_InitString ((char *) "if", 2);
        break;

      case elsif:
        return DynamicStrings_InitString ((char *) "elsif", 5);
        break;

      case constexp:
        return DynamicStrings_InitString ((char *) "constexp", 8);
        break;

      case neg:
        return DynamicStrings_InitString ((char *) "neg", 3);
        break;

      case cast:
        return DynamicStrings_InitString ((char *) "cast", 4);
        break;

      case val:
        return DynamicStrings_InitString ((char *) "val", 3);
        break;

      case plus:
        return DynamicStrings_InitString ((char *) "plus", 4);
        break;

      case sub:
        return DynamicStrings_InitString ((char *) "sub", 3);
        break;

      case div_:
        return DynamicStrings_InitString ((char *) "div", 3);
        break;

      case mod:
        return DynamicStrings_InitString ((char *) "mod", 3);
        break;

      case mult:
        return DynamicStrings_InitString ((char *) "mult", 4);
        break;

      case divide:
        return DynamicStrings_InitString ((char *) "divide", 6);
        break;

      case adr:
        return DynamicStrings_InitString ((char *) "adr", 3);
        break;

      case size:
        return DynamicStrings_InitString ((char *) "size", 4);
        break;

      case tsize:
        return DynamicStrings_InitString ((char *) "tsize", 5);
        break;

      case chr:
        return DynamicStrings_InitString ((char *) "chr", 3);
        break;

      case ord:
        return DynamicStrings_InitString ((char *) "ord", 3);
        break;

      case float_:
        return DynamicStrings_InitString ((char *) "float", 5);
        break;

      case trunc:
        return DynamicStrings_InitString ((char *) "trunc", 5);
        break;

      case high:
        return DynamicStrings_InitString ((char *) "high", 4);
        break;

      case componentref:
        return DynamicStrings_InitString ((char *) "componentref", 12);
        break;

      case pointerref:
        return DynamicStrings_InitString ((char *) "pointerref", 10);
        break;

      case arrayref:
        return DynamicStrings_InitString ((char *) "arrayref", 8);
        break;

      case deref:
        return DynamicStrings_InitString ((char *) "deref", 5);
        break;

      case equal:
        return DynamicStrings_InitString ((char *) "equal", 5);
        break;

      case notequal:
        return DynamicStrings_InitString ((char *) "notequal", 8);
        break;

      case less:
        return DynamicStrings_InitString ((char *) "less", 4);
        break;

      case greater:
        return DynamicStrings_InitString ((char *) "greater", 7);
        break;

      case greequal:
        return DynamicStrings_InitString ((char *) "greequal", 8);
        break;

      case lessequal:
        return DynamicStrings_InitString ((char *) "lessequal", 9);
        break;

      case lsl:
        return DynamicStrings_InitString ((char *) "lsl", 3);
        break;

      case lsr:
        return DynamicStrings_InitString ((char *) "lsr", 3);
        break;

      case lor:
        return DynamicStrings_InitString ((char *) "lor", 3);
        break;

      case land:
        return DynamicStrings_InitString ((char *) "land", 4);
        break;

      case lnot:
        return DynamicStrings_InitString ((char *) "lnot", 4);
        break;

      case lxor:
        return DynamicStrings_InitString ((char *) "lxor", 4);
        break;

      case and:
        return DynamicStrings_InitString ((char *) "and", 3);
        break;

      case or:
        return DynamicStrings_InitString ((char *) "or", 2);
        break;

      case not:
        return DynamicStrings_InitString ((char *) "not", 3);
        break;

      case identlist:
        return DynamicStrings_InitString ((char *) "identlist", 9);
        break;

      case vardecl:
        return DynamicStrings_InitString ((char *) "vardecl", 7);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  M2RTS_HALT (0);
}

static DynamicStrings_String gen (decl_node n)
{
  DynamicStrings_String s;
  unsigned int d;

  d = (unsigned int ) ((long unsigned int ) (n));
  s = FormatStrings_Sprintf1 (DynamicStrings_InitString ((char *) "< %d ", 5), (unsigned char *) &d, sizeof (d));
  s = DynamicStrings_ConCat (s, genKind (n));
  s = DynamicStrings_ConCat (s, DynamicStrings_InitString ((char *) " ", 1));
  s = DynamicStrings_ConCat (s, getFQstring (n));
  s = DynamicStrings_ConCat (s, DynamicStrings_InitString ((char *) " >", 2));
  return s;
}

static void dumpQ (char *q_, unsigned int _q_high, alists_alist l)
{
  DynamicStrings_String m;
  decl_node n;
  unsigned int d;
  unsigned int h;
  unsigned int i;
  char q[_q_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (q, q_, _q_high+1);

  m = FormatStrings_Sprintf0 (DynamicStrings_InitString ((char *) "Queue ", 6));
  m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
  m = FormatStrings_Sprintf0 (DynamicStrings_InitString ((char *) q, _q_high));
  m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
  m = FormatStrings_Sprintf0 (DynamicStrings_InitString ((char *) "\\n", 2));
  m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
  i = 1;
  h = alists_noOfItemsInList (l);
  while (i <= h)
    {
      n = alists_getItemFromList (l, i);
      m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, gen (n)));
      i += 1;
    }
  m = FormatStrings_Sprintf0 (DynamicStrings_InitString ((char *) "\\n", 2));
  m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
}

static void dumpLists (void)
{
  DynamicStrings_String m;

  if (mcOptions_getDebugTopological ())
    {
      m = FormatStrings_Sprintf0 (DynamicStrings_InitString ((char *) "\\n", 2));
      m = DynamicStrings_KillString (SFIO_WriteS (FIO_StdOut, m));
      dumpQ ((char *) "todo", 4, todoQ);
      dumpQ ((char *) "partial", 7, partialQ);
      dumpQ ((char *) "done", 4, doneQ);
    }
}

static void outputHidden (decl_node n)
{
  outText (doP, (char *) "#if !defined (", 14);
  doFQNameC (doP, n);
  outText (doP, (char *) "_D)\\n", 5);
  outText (doP, (char *) "#  define ", 10);
  doFQNameC (doP, n);
  outText (doP, (char *) "_D\\n", 4);
  outText (doP, (char *) "   typedef void *", 17);
  doFQNameC (doP, n);
  outText (doP, (char *) ";\\n", 3);
  outText (doP, (char *) "#endif\\n\\n", 10);
}

static void outputHiddenComplete (decl_node n)
{
  decl_node t;

  mcDebug_assert (decl_isType (n));
  t = decl_getType (n);
  mcDebug_assert (decl_isPointer (t));
  outText (doP, (char *) "#define ", 8);
  doFQNameC (doP, n);
  outText (doP, (char *) "_D\\n", 4);
  outText (doP, (char *) "typedef ", 8);
  doTypeNameC (doP, decl_getType (t));
  mcPretty_setNeedSpace (doP);
  outText (doP, (char *) "*", 1);
  doFQNameC (doP, n);
  outText (doP, (char *) ";\\n", 3);
}

static unsigned int tryPartial (decl_node n, nodeProcedure pt)
{
  decl_node q;

  if ((n != NULL) && (decl_isType (n)))
    {
      q = decl_getType (n);
      while (decl_isPointer (q))
        q = decl_getType (q);
      if ((q != NULL) && (((decl_isArray (q)) || (decl_isRecord (q))) || (decl_isProcType (q))))
        {
          (*pt.proc) (n);
          addTodo (q);
          return TRUE;
        }
    }
  return FALSE;
}

static void outputPartial (decl_node n)
{
  DynamicStrings_String s;
  decl_node q;
  unsigned int i;

  q = decl_getType (n);
  i = 0;
  while (decl_isPointer (q))
    {
      q = decl_getType (q);
      i += 1;
    }
  outText (doP, (char *) "typedef struct", 14);
  mcPretty_setNeedSpace (doP);
  s = getFQstring (n);
  if (decl_isRecord (q))
    s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "_r", 2)));
  else if (decl_isArray (q))
    s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "_a", 2)));
  else if (decl_isProcType (q))
    s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "_p", 2)));
  outTextS (doP, s);
  mcPretty_setNeedSpace (doP);
  s = DynamicStrings_KillString (s);
  while (i > 0)
    {
      outText (doP, (char *) "*", 1);
      i -= 1;
    }
  doFQNameC (doP, n);
  outText (doP, (char *) ";\\n\\n", 5);
}

static void tryOutputTodo (nodeProcedure c, nodeProcedure t, nodeProcedure v, nodeProcedure pt)
{
  unsigned int i;
  unsigned int n;
  decl_node d;

  i = 1;
  n = alists_noOfItemsInList (todoQ);
  while (i <= n)
    {
      d = alists_getItemFromList (todoQ, i);
      if (tryComplete (d, c, t, v))
        {
          alists_removeItemFromList (todoQ, (void *) d);
          alists_includeItemIntoList (doneQ, (void *) d);
          i = 1;
        }
      else if (tryPartial (d, pt))
        {
          alists_removeItemFromList (todoQ, (void *) d);
          alists_includeItemIntoList (partialQ, (void *) d);
          i = 1;
        }
      else
        i += 1;
      n = alists_noOfItemsInList (todoQ);
    }
}

static void tryOutputPartial (nodeProcedure t)
{
  unsigned int i;
  unsigned int n;
  decl_node d;

  i = 1;
  n = alists_noOfItemsInList (partialQ);
  while (i <= n)
    {
      d = alists_getItemFromList (partialQ, i);
      if (tryCompleteFromPartial (d, t))
        {
          alists_removeItemFromList (partialQ, (void *) d);
          alists_includeItemIntoList (doneQ, (void *) d);
          i = 1;
          n -= 1;
        }
      else
        i += 1;
    }
}

static void debugList (char *a_, unsigned int _a_high, alists_alist l)
{
  unsigned int i;
  unsigned int h;
  decl_node n;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  h = alists_noOfItemsInList (l);
  if (h > 0)
    {
      outText (doP, (char *) a, _a_high);
      outText (doP, (char *) " still contains node(s)\\n", 25);
      i = 1;
      do {
        n = alists_getItemFromList (l, i);
        dbg (n);
        i += 1;
      } while (! (i > h));
    }
}

static void debugLists (void)
{
  if (mcOptions_getDebugTopological ())
    {
      debugList ((char *) "todo", 4, todoQ);
      debugList ((char *) "partial", 7, partialQ);
    }
}

static void addEnumConst (decl_node n)
{
  DynamicStrings_String s;

  if ((decl_isConst (n)) || (decl_isEnumeration (n)))
    addTodo (n);
}

static void populateTodo (nodeProcedure p)
{
  decl_node n;
  unsigned int i;
  unsigned int h;
  alists_alist l;

  h = alists_noOfItemsInList (todoQ);
  i = 1;
  while (i <= h)
    {
      n = alists_getItemFromList (todoQ, i);
      l = alists_initList ();
      visitNode (l, n, p);
      alists_killList (&l);
      h = alists_noOfItemsInList (todoQ);
      i += 1;
    }
}

static void topologicallyOut (nodeProcedure c, nodeProcedure t, nodeProcedure v, nodeProcedure tp, nodeProcedure pc, nodeProcedure pt, nodeProcedure pv)
{
  unsigned int tol;
  unsigned int pal;
  unsigned int to;
  unsigned int pa;

  populateTodo ((nodeProcedure) {(nodeProcedure_t) addEnumConst});
  tol = 0;
  pal = 0;
  to = alists_noOfItemsInList (todoQ);
  pa = alists_noOfItemsInList (partialQ);
  while ((tol != to) || (pal != pa))
    {
      dumpLists ();
      tryOutputTodo (c, t, v, tp);
      dumpLists ();
      tryOutputPartial (pt);
      tol = to;
      pal = pa;
      to = alists_noOfItemsInList (todoQ);
      pa = alists_noOfItemsInList (partialQ);
    }
  dumpLists ();
  debugLists ();
}

static void outImpInitC (mcPretty_pretty p, decl_node n)
{
  outText (p, (char *) "\\n", 2);
  outText (p, (char *) "void", 4);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "_M2_", 4);
  doFQNameC (p, n);
  outText (p, (char *) "_init", 5);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(int argc, char *argv[])\\n", 26);
  p = outKc (p, (char *) "{\\n", 3);
  doStatementsC (p, n->impF.beginStatements);
  p = outKc (p, (char *) "}\\n", 3);
  outText (p, (char *) "\\n", 2);
  outText (p, (char *) "void", 4);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "_M2_", 4);
  doFQNameC (p, n);
  outText (p, (char *) "_finish", 7);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(int argc, char *argv[])\\n", 26);
  p = outKc (p, (char *) "{\\n", 3);
  doStatementsC (p, n->impF.finallyStatements);
  p = outKc (p, (char *) "}\\n", 3);
}

static void runSimplifyTypes (decl_node n)
{
  if (decl_isImp (n))
    simplifyTypes (n->impF.decls);
  else if (decl_isModule (n))
    simplifyTypes (n->moduleF.decls);
  else if (decl_isDef (n))
    simplifyTypes (n->defF.decls);
}

static void outDefC (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  outputFile = mcStream_openFrag (1);
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSource (n)));
  mcPretty_print (p, (char *) "/* automatically created by mc from ", 36);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) ".  */\\n\\n", 9);
  s = DynamicStrings_KillString (s);
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  mcPretty_print (p, (char *) "\\n#if !defined (_", 17);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) "_H)\\n", 5);
  mcPretty_print (p, (char *) "#   define _", 12);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) "_H\\n\\n", 6);
  outputFile = mcStream_openFrag (3);
  doP = p;
  Indexing_ForeachIndiceInIndexDo (n->defF.importedModules, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doIncludeC});
  mcPretty_print (p, (char *) "\\n", 2);
  mcPretty_print (p, (char *) "#   if defined (_", 17);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) "_C)\\n", 5);
  mcPretty_print (p, (char *) "#      define EXTERN\\n", 22);
  mcPretty_print (p, (char *) "#   else\\n", 10);
  mcPretty_print (p, (char *) "#      if defined(__GNUG__)\\n", 29);
  mcPretty_print (p, (char *) "#         define EXTERN extern \"C\"\\n", 36);
  mcPretty_print (p, (char *) "#      else\\n", 13);
  mcPretty_print (p, (char *) "#         define EXTERN extern\\n", 32);
  mcPretty_print (p, (char *) "#      endif\\n", 14);
  mcPretty_print (p, (char *) "#   endif\\n\\n", 13);
  outDeclsDefC (p, n);
  runPrototypeDefC (n);
  mcPretty_print (p, (char *) "\\n", 2);
  mcPretty_print (p, (char *) "#   undef EXTERN\\n", 18);
  mcPretty_print (p, (char *) "#endif\\n", 8);
  outputFile = mcStream_openFrag (2);
  keyc_genDefs (p);
  s = DynamicStrings_KillString (s);
}

static void runPrototypeExported (decl_node n)
{
  if (decl_isExported (n))
    {
      keyc_enterScope (n);
      doProcedureHeadingC (n);
      mcPretty_print (doP, (char *) ";\\n", 3);
      keyc_leaveScope (n);
    }
}

static void runPrototypeDefC (decl_node n)
{
  if (decl_isDef (n))
    Indexing_ForeachIndiceInIndexDo (n->defF.decls.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) runPrototypeExported});
}

static void outImpC (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;
  decl_node defModule;

  outputFile = mcStream_openFrag (1);
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSource (n)));
  mcPretty_print (p, (char *) "/* automatically created by mc from ", 36);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) ".  */\\n\\n", 9);
  s = DynamicStrings_KillString (s);
  outputFile = mcStream_openFrag (3);
  if (mcOptions_getExtendedOpaque ())
    {
      doP = p;
      includeExternals (n);
      foreachModuleDo (n, (symbolKey_performOperation) {(symbolKey_performOperation_t) runSimplifyTypes});
      libc_printf ((char *) "/*  --extended-opaque seen therefore no #include will be used and everything will be declared in full.  */\\n", 108);
      decl_foreachDefModuleDo ((symbolKey_performOperation) {(symbolKey_performOperation_t) runIncludeDefConstType});
      includeDefVarProcedure (n);
      outDeclsImpC (p, n->impF.decls);
      decl_foreachDefModuleDo ((symbolKey_performOperation) {(symbolKey_performOperation_t) runPrototypeDefC});
    }
  else
    {
      s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
      mcPretty_print (p, (char *) "#define _", 9);
      mcPretty_prints (p, s);
      mcPretty_print (p, (char *) "_H\\n", 4);
      mcPretty_print (p, (char *) "#define _", 9);
      mcPretty_prints (p, s);
      mcPretty_print (p, (char *) "_C\\n\\n", 6);
      doP = p;
      Indexing_ForeachIndiceInIndexDo (n->impF.importedModules, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doIncludeC});
      mcPretty_print (p, (char *) "\\n", 2);
      includeDefConstType (n);
      includeDefVarProcedure (n);
      outDeclsImpC (p, n->impF.decls);
      defModule = decl_lookupDef (decl_getSymName (n));
      if (defModule != NULL)
        runPrototypeDefC (defModule);
    }
  Indexing_ForeachIndiceInIndexDo (n->impF.decls.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doPrototypeC});
  outProceduresC (p, n->impF.decls);
  outImpInitC (p, n);
  outputFile = mcStream_openFrag (2);
  keyc_genDefs (p);
  s = DynamicStrings_KillString (s);
}

static void outDeclsModuleC (mcPretty_pretty p, scopeT s)
{
  simplifyTypes (s);
  includeConstType (s);
  doP = p;
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstC}, (nodeProcedure) {(nodeProcedure_t) doTypesC}, (nodeProcedure) {(nodeProcedure_t) doVarC}, (nodeProcedure) {(nodeProcedure_t) outputPartial}, (nodeProcedure) {(nodeProcedure_t) doNone}, (nodeProcedure) {(nodeProcedure_t) doCompletePartialC}, (nodeProcedure) {(nodeProcedure_t) doNone});
  includeVarProcedure (s);
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstC}, (nodeProcedure) {(nodeProcedure_t) doTypesC}, (nodeProcedure) {(nodeProcedure_t) doVarC}, (nodeProcedure) {(nodeProcedure_t) outputPartial}, (nodeProcedure) {(nodeProcedure_t) doNone}, (nodeProcedure) {(nodeProcedure_t) doCompletePartialC}, (nodeProcedure) {(nodeProcedure_t) doNone});
  Indexing_ForeachIndiceInIndexDo (s.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doPrototypeC});
}

static void outModuleInitC (mcPretty_pretty p, decl_node n)
{
  outText (p, (char *) "\\n", 2);
  outText (p, (char *) "void", 4);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "_M2_", 4);
  doFQNameC (p, n);
  outText (p, (char *) "_init", 5);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(int argc, char *argv[])\\n", 26);
  p = outKc (p, (char *) "{\\n", 3);
  doStatementsC (p, n->moduleF.beginStatements);
  p = outKc (p, (char *) "}\\n", 3);
  outText (p, (char *) "\\n", 2);
  outText (p, (char *) "void", 4);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "_M2_", 4);
  doFQNameC (p, n);
  outText (p, (char *) "_finish", 7);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "(int argc, char *argv[])\\n", 26);
  p = outKc (p, (char *) "{\\n", 3);
  doStatementsC (p, n->moduleF.finallyStatements);
  p = outKc (p, (char *) "}\\n", 3);
}

static void outModuleC (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  outputFile = mcStream_openFrag (1);
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSource (n)));
  mcPretty_print (p, (char *) "/* automatically created by mc from ", 36);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) ".  */\\n\\n", 9);
  s = DynamicStrings_KillString (s);
  outputFile = mcStream_openFrag (3);
  if (mcOptions_getExtendedOpaque ())
    {
      doP = p;
      includeExternals (n);
      foreachModuleDo (n, (symbolKey_performOperation) {(symbolKey_performOperation_t) runSimplifyTypes});
      libc_printf ((char *) "/*  --extended-opaque seen therefore no #include will be used and everything will be declared in full.  */\\n", 108);
      decl_foreachDefModuleDo ((symbolKey_performOperation) {(symbolKey_performOperation_t) runIncludeDefConstType});
      outDeclsModuleC (p, n->impF.decls);
      decl_foreachDefModuleDo ((symbolKey_performOperation) {(symbolKey_performOperation_t) runPrototypeDefC});
    }
  else
    {
      doP = p;
      Indexing_ForeachIndiceInIndexDo (n->impF.importedModules, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doIncludeC});
      mcPretty_print (p, (char *) "\\n", 2);
      outDeclsModuleC (p, n->moduleF.decls);
    }
  Indexing_ForeachIndiceInIndexDo (n->moduleF.decls.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doPrototypeC});
  outProceduresC (p, n->moduleF.decls);
  outModuleInitC (p, n);
  outputFile = mcStream_openFrag (2);
  keyc_genDefs (p);
}

static void outC (mcPretty_pretty p, decl_node n)
{
  keyc_enterScope (n);
  if (decl_isDef (n))
    outDefC (p, n);
  else if (decl_isImp (n))
    outImpC (p, n);
  else if (decl_isModule (n))
    outModuleC (p, n);
  else
    M2RTS_HALT (0);
  keyc_leaveScope (n);
}

static void outCP (mcPretty_pretty p, decl_node n)
{
}

static void doIncludeM2 (decl_node n)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  mcPretty_print (doP, (char *) "IMPORT ", 7);
  mcPretty_prints (doP, s);
  mcPretty_print (doP, (char *) " ;\\n", 4);
  s = DynamicStrings_KillString (s);
  if (decl_isDef (n))
    symbolKey_foreachNodeDo (n->defF.decls.symbols, (symbolKey_performOperation) {(symbolKey_performOperation_t) addDone});
  else if (decl_isImp (n))
    symbolKey_foreachNodeDo (n->impF.decls.symbols, (symbolKey_performOperation) {(symbolKey_performOperation_t) addDone});
  else if (decl_isModule (n))
    symbolKey_foreachNodeDo (n->moduleF.decls.symbols, (symbolKey_performOperation) {(symbolKey_performOperation_t) addDone});
}

static void doConstM2 (decl_node n)
{
  mcPretty_print (doP, (char *) "CONST\\n", 7);
  doFQNameC (doP, n);
  mcPretty_setNeedSpace (doP);
  doExprC (doP, n->constF.value);
  mcPretty_print (doP, (char *) "\\n", 2);
}

static void doProcTypeM2 (mcPretty_pretty p, decl_node n)
{
  outText (p, (char *) "proc type to do..", 17);
}

static void doRecordFieldM2 (mcPretty_pretty p, decl_node f)
{
  doNameM2 (p, f);
  outText (p, (char *) ":", 1);
  mcPretty_setNeedSpace (p);
  doTypeM2 (p, decl_getType (f));
  mcPretty_setNeedSpace (p);
}

static void doVarientFieldM2 (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  mcDebug_assert (decl_isVarientField (n));
  doNameM2 (p, n);
  outText (p, (char *) ":", 1);
  mcPretty_setNeedSpace (p);
  i = Indexing_LowIndice (n->varientfieldF.listOfSons);
  t = Indexing_HighIndice (n->varientfieldF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientfieldF.listOfSons, i);
      if (decl_isRecordField (q))
        {
          doRecordFieldM2 (p, q);
          outText (p, (char *) ";\\n", 3);
        }
      else if (decl_isVarient (q))
        {
          doVarientM2 (p, q);
          outText (p, (char *) ";\\n", 3);
        }
      else
        M2RTS_HALT (0);
      i += 1;
    }
}

static void doVarientM2 (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  mcDebug_assert (decl_isVarient (n));
  outText (p, (char *) "CASE", 4);
  mcPretty_setNeedSpace (p);
  if (n->varientF.tag != NULL)
    if (decl_isRecordField (n->varientF.tag))
      doRecordFieldM2 (p, n->varientF.tag);
    else if (decl_isVarientField (n->varientF.tag))
      doVarientFieldM2 (p, n->varientF.tag);
    else
      M2RTS_HALT (0);
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "OF\\n", 4);
  i = Indexing_LowIndice (n->varientF.listOfSons);
  t = Indexing_HighIndice (n->varientF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientF.listOfSons, i);
      if (decl_isRecordField (q))
        if (! q->recordfieldF.tag)
          {
            doRecordFieldM2 (p, q);
            outText (p, (char *) ";\\n", 3);
          }
      else if (decl_isVarientField (q))
        doVarientFieldM2 (p, q);
      else
        M2RTS_HALT (0);
      i += 1;
    }
  outText (p, (char *) "END", 3);
  mcPretty_setNeedSpace (p);
}

static void doRecordM2 (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node f;

  mcDebug_assert (decl_isRecord (n));
  p = outKm2 (p, (char *) "RECORD", 6);
  i = Indexing_LowIndice (n->recordF.listOfSons);
  h = Indexing_HighIndice (n->recordF.listOfSons);
  outText (p, (char *) "\\n", 2);
  while (i <= h)
    {
      f = Indexing_GetIndice (n->recordF.listOfSons, i);
      if (decl_isRecordField (f))
        if (! f->recordfieldF.tag)
          {
            doRecordFieldM2 (p, f);
            outText (p, (char *) ";\\n", 3);
          }
      else if (decl_isVarient (f))
        {
          doVarientM2 (p, f);
          outText (p, (char *) ";\\n", 3);
        }
      else if (decl_isVarientField (f))
        doVarientFieldM2 (p, f);
      i += 1;
    }
  p = outKm2 (p, (char *) "END", 3);
  mcPretty_setNeedSpace (p);
}

static void doPointerM2 (mcPretty_pretty p, decl_node n)
{
  outText (p, (char *) "POINTER TO", 10);
  mcPretty_setNeedSpace (doP);
  doTypeM2 (p, decl_getType (n));
  mcPretty_setNeedSpace (p);
  outText (p, (char *) ";\\n", 3);
}

static void doTypeAliasM2 (mcPretty_pretty p, decl_node n)
{
  doTypeNameC (p, n);
  mcPretty_setNeedSpace (p);
  outText (doP, (char *) "=", 1);
  mcPretty_setNeedSpace (p);
  doTypeM2 (p, decl_getType (n));
  mcPretty_setNeedSpace (p);
  outText (p, (char *) "\\n", 2);
}

static void doEnumerationM2 (mcPretty_pretty p, decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node s;
  DynamicStrings_String t;

  outText (p, (char *) "(", 1);
  i = Indexing_LowIndice (n->enumerationF.listOfSons);
  h = Indexing_HighIndice (n->enumerationF.listOfSons);
  while (i <= h)
    {
      s = Indexing_GetIndice (n->enumerationF.listOfSons, i);
      doFQNameC (p, s);
      if (i < h)
        {
          outText (p, (char *) ",", 1);
          mcPretty_setNeedSpace (p);
        }
      i += 1;
    }
  outText (p, (char *) ")", 1);
}

static void doBaseM2 (mcPretty_pretty p, decl_node n)
{
  switch (n->kind)
    {
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case boolean:
      case proc:
        doNameM2 (p, n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  mcPretty_setNeedSpace (p);
}

static void doSystemM2 (mcPretty_pretty p, decl_node n)
{
  switch (n->kind)
    {
      case address:
      case loc:
      case byte:
      case word:
        doNameM2 (p, n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

static void doTypeM2 (mcPretty_pretty p, decl_node n)
{
  if (isBase (n))
    doBaseM2 (p, n);
  else if (isSystem (n))
    doSystemM2 (p, n);
  else if (decl_isType (n))
    doTypeAliasM2 (p, n);
  else if (decl_isProcType (n))
    doProcTypeM2 (p, n);
  else if (decl_isPointer (n))
    doPointerM2 (p, n);
  else if (decl_isEnumeration (n))
    doEnumerationM2 (p, n);
  else if (decl_isRecord (n))
    doRecordM2 (p, n);
}

static void doTypesM2 (decl_node n)
{
  decl_node m;

  outText (doP, (char *) "TYPE\\n", 6);
  doTypeM2 (doP, n);
}

static void doVarM2 (decl_node n)
{
  mcDebug_assert (decl_isVar (n));
  doNameC (doP, n);
  outText (doP, (char *) ":", 1);
  mcPretty_setNeedSpace (doP);
  doTypeM2 (doP, decl_getType (n));
  mcPretty_setNeedSpace (doP);
  outText (doP, (char *) ";\\n", 3);
}

static void doVarsM2 (decl_node n)
{
  decl_node m;

  outText (doP, (char *) "VAR\\n", 5);
  doVarM2 (n);
}

static void doTypeNameM2 (mcPretty_pretty p, decl_node n)
{
  doNameM2 (p, n);
}

static void doParamM2 (mcPretty_pretty p, decl_node n)
{
  decl_node ptype;
  nameKey_Name i;
  unsigned int c;
  unsigned int t;
  wlists_wlist l;

  mcDebug_assert (decl_isParam (n));
  ptype = decl_getType (n);
  if (n->paramF.namelist == NULL)
    doTypeNameM2 (p, ptype);
  else
    {
      mcDebug_assert (isIdentList (n->paramF.namelist));
      l = n->paramF.namelist->identlistF.names;
      if (l == NULL)
        doTypeNameM2 (p, ptype);
      else
        {
          t = wlists_noOfItemsInList (l);
          c = 1;
          while (c <= t)
            {
              i = wlists_getItemFromList (l, c);
              mcPretty_setNeedSpace (p);
              doNamesC (p, i);
              if (c < t)
                {
                  outText (p, (char *) ",", 1);
                  mcPretty_setNeedSpace (p);
                }
              c += 1;
            }
          outText (p, (char *) ":", 1);
          mcPretty_setNeedSpace (p);
          doTypeNameM2 (p, ptype);
        }
    }
}

static void doVarParamM2 (mcPretty_pretty p, decl_node n)
{
  decl_node ptype;
  nameKey_Name i;
  unsigned int c;
  unsigned int t;
  wlists_wlist l;

  mcDebug_assert (decl_isVarParam (n));
  outText (p, (char *) "VAR", 3);
  mcPretty_setNeedSpace (p);
  ptype = decl_getType (n);
  if (n->varparamF.namelist == NULL)
    doTypeNameM2 (p, ptype);
  else
    {
      mcDebug_assert (isIdentList (n->varparamF.namelist));
      l = n->varparamF.namelist->identlistF.names;
      if (l == NULL)
        doTypeNameM2 (p, ptype);
      else
        {
          t = wlists_noOfItemsInList (l);
          c = 1;
          while (c <= t)
            {
              i = wlists_getItemFromList (l, c);
              mcPretty_setNeedSpace (p);
              doNamesC (p, i);
              if (c < t)
                {
                  outText (p, (char *) ",", 1);
                  mcPretty_setNeedSpace (p);
                }
              c += 1;
            }
          outText (p, (char *) ":", 1);
          mcPretty_setNeedSpace (p);
          doTypeNameM2 (p, ptype);
        }
    }
}

static void doParameterM2 (mcPretty_pretty p, decl_node n)
{
  if (decl_isParam (n))
    doParamM2 (p, n);
  else if (decl_isVarParam (n))
    doVarParamM2 (p, n);
  else if (decl_isVarargs (n))
    mcPretty_print (p, (char *) "...", 3);
}

static void doPrototypeM2 (decl_node n)
{
  unsigned int i;
  unsigned int h;
  decl_node p;

  mcDebug_assert (decl_isProcedure (n));
  mcPretty_noSpace (doP);
  doNameM2 (doP, n);
  mcPretty_setNeedSpace (doP);
  outText (doP, (char *) "(", 1);
  i = Indexing_LowIndice (n->procedureF.parameters);
  h = Indexing_HighIndice (n->procedureF.parameters);
  while (i <= h)
    {
      p = Indexing_GetIndice (n->procedureF.parameters, i);
      doParameterM2 (doP, p);
      mcPretty_noSpace (doP);
      if (i < h)
        {
          mcPretty_print (doP, (char *) ";", 1);
          mcPretty_setNeedSpace (doP);
        }
      i += 1;
    }
  outText (doP, (char *) ")", 1);
  if (n->procedureF.returnType != NULL)
    {
      mcPretty_setNeedSpace (doP);
      outText (doP, (char *) ":", 1);
      doTypeM2 (doP, n->procedureF.returnType);
      mcPretty_setNeedSpace (doP);
    }
  outText (doP, (char *) ";\\n", 3);
}

static void outputPartialM2 (decl_node n)
{
  decl_node q;

  q = decl_getType (n);
  if (decl_isRecord (q))
    doTypeM2 (doP, n);
  else if (decl_isArray (q))
    doTypeM2 (doP, n);
  else if (decl_isProcType (q))
    doTypeM2 (doP, n);
}

static void outDeclsDefM2 (mcPretty_pretty p, scopeT s)
{
  simplifyTypes (s);
  includeConstType (s);
  doP = p;
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstM2}, (nodeProcedure) {(nodeProcedure_t) doTypesM2}, (nodeProcedure) {(nodeProcedure_t) doVarsM2}, (nodeProcedure) {(nodeProcedure_t) outputPartialM2}, (nodeProcedure) {(nodeProcedure_t) doNothing}, (nodeProcedure) {(nodeProcedure_t) doNothing}, (nodeProcedure) {(nodeProcedure_t) doNothing});
  includeVarProcedure (s);
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstM2}, (nodeProcedure) {(nodeProcedure_t) doTypesM2}, (nodeProcedure) {(nodeProcedure_t) doVarsM2}, (nodeProcedure) {(nodeProcedure_t) outputPartialM2}, (nodeProcedure) {(nodeProcedure_t) doNothing}, (nodeProcedure) {(nodeProcedure_t) doNothing}, (nodeProcedure) {(nodeProcedure_t) doNothing});
  Indexing_ForeachIndiceInIndexDo (s.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doPrototypeM2});
}

static void outDefM2 (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSource (n)));
  mcPretty_print (p, (char *) "(* automatically created by mc from ", 36);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) ".  *)\\n\\n", 9);
  s = DynamicStrings_KillString (s);
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  mcPretty_print (p, (char *) "DEFINITION MODULE ", 18);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) " ;\\n\\n", 6);
  doP = p;
  Indexing_ForeachIndiceInIndexDo (n->defF.importedModules, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doIncludeM2});
  mcPretty_print (p, (char *) "\\n", 2);
  outDeclsDefM2 (p, n->defF.decls);
  mcPretty_print (p, (char *) "\\n", 2);
  mcPretty_print (p, (char *) "END ", 4);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) ".\\n", 3);
  s = DynamicStrings_KillString (s);
}

static void outDeclsImpM2 (mcPretty_pretty p, scopeT s)
{
  simplifyTypes (s);
  includeConstType (s);
  doP = p;
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstM2}, (nodeProcedure) {(nodeProcedure_t) doTypesM2}, (nodeProcedure) {(nodeProcedure_t) doVarM2}, (nodeProcedure) {(nodeProcedure_t) outputPartialM2}, (nodeProcedure) {(nodeProcedure_t) doNothing}, (nodeProcedure) {(nodeProcedure_t) doNothing}, (nodeProcedure) {(nodeProcedure_t) doNothing});
  includeVarProcedure (s);
  topologicallyOut ((nodeProcedure) {(nodeProcedure_t) doConstM2}, (nodeProcedure) {(nodeProcedure_t) doTypesM2}, (nodeProcedure) {(nodeProcedure_t) doVarsM2}, (nodeProcedure) {(nodeProcedure_t) outputPartialM2}, (nodeProcedure) {(nodeProcedure_t) doNothing}, (nodeProcedure) {(nodeProcedure_t) doNothing}, (nodeProcedure) {(nodeProcedure_t) doNothing});
  outText (p, (char *) "\\n", 2);
  Indexing_ForeachIndiceInIndexDo (s.procedures, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doPrototypeC});
}

static void outImpM2 (mcPretty_pretty p, decl_node n)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSource (n)));
  mcPretty_print (p, (char *) "(* automatically created by mc from ", 36);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) ".  *)\\n\\n", 9);
  mcPretty_print (p, (char *) "IMPLEMENTATION MODULE ", 22);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) " ;\\n\\n", 6);
  doP = p;
  Indexing_ForeachIndiceInIndexDo (n->impF.importedModules, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) doIncludeM2});
  mcPretty_print (p, (char *) "\\n", 2);
  includeDefConstType (n);
  outDeclsImpM2 (p, n->impF.decls);
  mcPretty_print (p, (char *) "\\n", 2);
  mcPretty_print (p, (char *) "END ", 4);
  mcPretty_prints (p, s);
  mcPretty_print (p, (char *) ".\\n", 3);
  s = DynamicStrings_KillString (s);
}

static void outModuleM2 (mcPretty_pretty p, decl_node n)
{
}

static void outM2 (mcPretty_pretty p, decl_node n)
{
  if (decl_isDef (n))
    outDefM2 (p, n);
  else if (decl_isImp (n))
    outImpM2 (p, n);
  else if (decl_isModule (n))
    outModuleM2 (p, n);
  else
    M2RTS_HALT (0);
}

static void addDone (decl_node n)
{
  DynamicStrings_String s;

  alists_includeItemIntoList (doneQ, (void *) n);
}

static void addDoneDef (decl_node n)
{
  if ((decl_lookupImp (decl_getSymName (decl_getScope (n)))) == (decl_getMainModule ()))
    {
      mcMetaError_metaError1 ((char *) "cyclic dependancy found between another module using {%1ad} from the definition module of the implementation main being compiled, use the --extended-opaque option to compile", 173, (unsigned char *) &n, sizeof (n));
      mcError_flushErrors ();
      mcError_errorAbort0 ((char *) "terminating compilation", 23);
    }
  else
    addDone (n);
}

static decl_node dbgAdd (alists_alist l, decl_node n)
{
  if (n != NULL)
    alists_includeItemIntoList (l, (void *) n);
  return n;
}

static void dbgType (alists_alist l, decl_node n)
{
  decl_node t;

  t = dbgAdd (l, decl_getType (n));
  out1 ((char *) "<%s type", 8, n);
  if (t == NULL)
    out0 ((char *) ", type = NIL\\n", 14);
  else
    out1 ((char *) ", type = %s>\\n", 14, t);
}

static void dbgPointer (alists_alist l, decl_node n)
{
  decl_node t;

  t = dbgAdd (l, decl_getType (n));
  out1 ((char *) "<%s pointer", 11, n);
  out1 ((char *) " to %s>\\n", 9, t);
}

static void dbgRecord (alists_alist l, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  out1 ((char *) "<%s record:\\n", 13, n);
  i = Indexing_LowIndice (n->recordF.listOfSons);
  t = Indexing_HighIndice (n->recordF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->recordF.listOfSons, i);
      if (decl_isRecordField (q))
        out1 ((char *) " <recordfield %s", 16, q);
      else if (decl_isVarientField (q))
        out1 ((char *) " <varientfield %s", 17, q);
      else if (decl_isVarient (q))
        out1 ((char *) " <varient %s", 12, q);
      else
        M2RTS_HALT (0);
      q = dbgAdd (l, decl_getType (q));
      out1 ((char *) ": %s>\\n", 7, q);
      i += 1;
    }
  outText (doP, (char *) ">\\n", 3);
}

static void dbgVarient (alists_alist l, decl_node n)
{
  unsigned int i;
  unsigned int t;
  decl_node q;

  out1 ((char *) "<%s varient: ", 13, n);
  out1 ((char *) "tag %s", 6, n->varientF.tag);
  q = decl_getType (n->varientF.tag);
  if (q == NULL)
    outText (doP, (char *) "\\n", 2);
  else
    {
      out1 ((char *) ": %s\\n", 6, q);
      q = dbgAdd (l, q);
    }
  i = Indexing_LowIndice (n->varientF.listOfSons);
  t = Indexing_HighIndice (n->varientF.listOfSons);
  while (i <= t)
    {
      q = Indexing_GetIndice (n->varientF.listOfSons, i);
      if (decl_isRecordField (q))
        out1 ((char *) " <recordfield %s", 16, q);
      else if (decl_isVarientField (q))
        out1 ((char *) " <varientfield %s", 17, q);
      else if (decl_isVarient (q))
        out1 ((char *) " <varient %s", 12, q);
      else
        M2RTS_HALT (0);
      q = dbgAdd (l, decl_getType (q));
      out1 ((char *) ": %s>\\n", 7, q);
      i += 1;
    }
  outText (doP, (char *) ">\\n", 3);
}

static void dbgEnumeration (alists_alist l, decl_node n)
{
  decl_node e;
  unsigned int i;
  unsigned int h;

  outText (doP, (char *) "< enumeration ", 14);
  i = Indexing_LowIndice (n->enumerationF.listOfSons);
  h = Indexing_HighIndice (n->enumerationF.listOfSons);
  while (i <= h)
    {
      e = Indexing_GetIndice (n->enumerationF.listOfSons, i);
      out1 ((char *) "%s, ", 4, e);
      i += 1;
    }
  outText (doP, (char *) ">\\n", 3);
}

static void dbgVar (alists_alist l, decl_node n)
{
  decl_node t;

  t = dbgAdd (l, decl_getType (n));
  out1 ((char *) "<%s var", 7, n);
  out1 ((char *) ", type = %s>\\n", 14, t);
}

static void dbgSubrange (alists_alist l, decl_node n)
{
  if (n->subrangeF.low == NULL)
    out1 ((char *) "%s", 2, n->subrangeF.type);
  else
    {
      out1 ((char *) "[%s", 3, n->subrangeF.low);
      out1 ((char *) "..%s]", 5, n->subrangeF.high);
    }
}

static void dbgArray (alists_alist l, decl_node n)
{
  decl_node t;

  t = dbgAdd (l, decl_getType (n));
  out1 ((char *) "<%s array ", 10, n);
  if (n->arrayF.subr != NULL)
    dbgSubrange (l, n->arrayF.subr);
  out1 ((char *) " of %s>\\n", 9, t);
}

static void doDbg (alists_alist l, decl_node n)
{
  if (n == NULL)
    ;  /* empty.  */
  else if (decl_isSubrange (n))
    dbgSubrange (l, n);
  else if (decl_isType (n))
    dbgType (l, n);
  else if (decl_isRecord (n))
    dbgRecord (l, n);
  else if (decl_isVarient (n))
    dbgVarient (l, n);
  else if (decl_isEnumeration (n))
    dbgEnumeration (l, n);
  else if (decl_isPointer (n))
    dbgPointer (l, n);
  else if (decl_isArray (n))
    dbgArray (l, n);
  else if (decl_isVar (n))
    dbgVar (l, n);
}

static void dbg (decl_node n)
{
  alists_alist l;
  mcPretty_pretty o;
  FIO_File f;
  DynamicStrings_String s;
  unsigned int i;

  o = doP;
  f = outputFile;
  outputFile = FIO_StdOut;
  doP = mcPretty_initPretty ((mcPretty_writeProc) {(mcPretty_writeProc_t) write}, (mcPretty_writeLnProc) {(mcPretty_writeLnProc_t) writeln});
  l = alists_initList ();
  alists_includeItemIntoList (l, (void *) n);
  i = 1;
  out1 ((char *) "dbg (%s)\\n", 10, n);
  do {
    n = alists_getItemFromList (l, i);
    doDbg (l, n);
    i += 1;
  } while (! (i > (alists_noOfItemsInList (l))));
  doP = o;
  outputFile = f;
}

static unsigned int isAssignment (decl_node n)
{
  return n->kind == assignment;
}

static unsigned int isComment (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == comment;
}

static decl_node dupExplist (decl_node n)
{
  decl_node m;
  unsigned int i;

  mcDebug_assert (decl_isExpList (n));
  m = decl_makeExpList ();
  i = Indexing_LowIndice (n->explistF.exp);
  while (i <= (Indexing_HighIndice (n->explistF.exp)))
    {
      decl_putExpList (m, decl_dupExpr ((decl_node) Indexing_GetIndice (n->explistF.exp, i)));
      i += 1;
    }
  return m;
}

static decl_node dupArrayref (decl_node n)
{
  mcDebug_assert (isArrayRef (n));
  return decl_makeArrayRef (decl_dupExpr (n->arrayrefF.array), decl_dupExpr (n->arrayrefF.index));
}

static decl_node dupPointerref (decl_node n)
{
  mcDebug_assert (decl_isPointerRef (n));
  return decl_makePointerRef (decl_dupExpr (n->pointerrefF.ptr), decl_dupExpr (n->pointerrefF.field));
}

static decl_node dupComponentref (decl_node n)
{
  mcDebug_assert (isComponentRef (n));
  return doMakeComponentRef (decl_dupExpr (n->componentrefF.rec), decl_dupExpr (n->componentrefF.field));
}

static decl_node dupBinary (decl_node n)
{
  return makeBinary (n->kind, decl_dupExpr (n->binaryF.left), decl_dupExpr (n->binaryF.right), n->binaryF.resultType);
}

static decl_node dupUnary (decl_node n)
{
  return makeUnary (n->kind, decl_dupExpr (n->unaryF.arg), n->unaryF.resultType);
}

static decl_node dupFunccall (decl_node n)
{
  decl_node m;

  mcDebug_assert (isFuncCall (n));
  m = decl_makeFuncCall (decl_dupExpr (n->funccallF.function), decl_dupExpr (n->funccallF.args));
  m->funccallF.type = n->funccallF.type;
  return m;
}

static decl_node dupSetValue (decl_node n)
{
  decl_node m;
  unsigned int i;

  m = newNode ((nodeT) setvalue);
  m->setvalueF.type = n->setvalueF.type;
  i = Indexing_LowIndice (n->setvalueF.values);
  while (i <= (Indexing_HighIndice (n->setvalueF.values)))
    {
      m = decl_putSetValue (m, decl_dupExpr ((decl_node) Indexing_GetIndice (n->setvalueF.values, i)));
      i += 1;
    }
  return m;
}

static void makeSystem (void)
{
  systemN = decl_lookupDef (nameKey_makeKey ((char *) "SYSTEM", 6));
  addressN = makeBase ((nodeT) address);
  locN = makeBase ((nodeT) loc);
  byteN = makeBase ((nodeT) byte);
  wordN = makeBase ((nodeT) word);
  adrN = makeBase ((nodeT) adr);
  tsizeN = makeBase ((nodeT) tsize);
  throwN = makeBase ((nodeT) throw);
  decl_enterScope (systemN);
  addressN = addToScope (addressN);
  locN = addToScope (locN);
  byteN = addToScope (byteN);
  wordN = addToScope (wordN);
  adrN = addToScope (adrN);
  tsizeN = addToScope (tsizeN);
  throwN = addToScope (throwN);
  mcDebug_assert (sizeN != NULL);
  sizeN = addToScope (sizeN);
  decl_leaveScope ();
  addDone (addressN);
  addDone (locN);
  addDone (byteN);
  addDone (wordN);
}

static void makeM2rts (void)
{
  m2rtsN = decl_lookupDef (nameKey_makeKey ((char *) "M2RTS", 5));
}

static decl_node makeBitnum (void)
{
  decl_node b;

  b = newNode ((nodeT) subrange);
  b->subrangeF.type = NULL;
  b->subrangeF.scope = NULL;
  b->subrangeF.low = lookupConst (b, nameKey_makeKey ((char *) "0", 1));
  b->subrangeF.high = lookupConst (b, nameKey_makeKey ((char *) "31", 2));
  return b;
}

static void makeBaseSymbols (void)
{
  baseSymbols = symbolKey_initTree ();
  booleanN = makeBase ((nodeT) boolean);
  charN = makeBase ((nodeT) char_);
  procN = makeBase ((nodeT) proc);
  cardinalN = makeBase ((nodeT) cardinal);
  longcardN = makeBase ((nodeT) longcard);
  shortcardN = makeBase ((nodeT) shortcard);
  integerN = makeBase ((nodeT) integer);
  longintN = makeBase ((nodeT) longint);
  shortintN = makeBase ((nodeT) shortint);
  bitsetN = makeBase ((nodeT) bitset);
  bitnumN = makeBitnum ();
  ztypeN = makeBase ((nodeT) ztype);
  rtypeN = makeBase ((nodeT) rtype);
  realN = makeBase ((nodeT) real);
  longrealN = makeBase ((nodeT) longreal);
  shortrealN = makeBase ((nodeT) shortreal);
  nilN = makeBase ((nodeT) nil);
  trueN = makeBase ((nodeT) true);
  falseN = makeBase ((nodeT) false);
  sizeN = makeBase ((nodeT) size);
  minN = makeBase ((nodeT) min);
  maxN = makeBase ((nodeT) max);
  floatN = makeBase ((nodeT) float_);
  truncN = makeBase ((nodeT) trunc);
  ordN = makeBase ((nodeT) ord);
  valN = makeBase ((nodeT) val);
  chrN = makeBase ((nodeT) chr);
  absN = makeBase ((nodeT) abs_);
  newN = makeBase ((nodeT) new);
  disposeN = makeBase ((nodeT) dispose);
  incN = makeBase ((nodeT) inc);
  decN = makeBase ((nodeT) dec);
  inclN = makeBase ((nodeT) incl);
  exclN = makeBase ((nodeT) excl);
  highN = makeBase ((nodeT) high);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "BOOLEAN", 7), (void *) booleanN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "PROC", 4), (void *) procN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "CHAR", 4), (void *) charN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "CARDINAL", 8), (void *) cardinalN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "SHORTCARD", 9), (void *) shortcardN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "LONGCARD", 8), (void *) longcardN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "INTEGER", 7), (void *) integerN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "LONGINT", 7), (void *) longintN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "SHORTINT", 8), (void *) shortintN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "BITSET", 6), (void *) bitsetN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "REAL", 4), (void *) realN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "SHORTREAL", 9), (void *) shortrealN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "LONGREAL", 8), (void *) longrealN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "NIL", 3), (void *) nilN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "TRUE", 4), (void *) trueN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "FALSE", 5), (void *) falseN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "SIZE", 4), (void *) sizeN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "MIN", 3), (void *) minN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "MAX", 3), (void *) maxN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "FLOAT", 5), (void *) floatN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "TRUNC", 5), (void *) truncN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "ORD", 3), (void *) ordN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "VAL", 3), (void *) valN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "CHR", 3), (void *) chrN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "ABS", 3), (void *) absN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "NEW", 3), (void *) newN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "DISPOSE", 7), (void *) disposeN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "INC", 3), (void *) incN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "DEC", 3), (void *) decN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "INCL", 4), (void *) inclN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "EXCL", 4), (void *) exclN);
  symbolKey_putSymKey (baseSymbols, nameKey_makeKey ((char *) "HIGH", 4), (void *) highN);
  addDone (booleanN);
  addDone (charN);
  addDone (cardinalN);
  addDone (longcardN);
  addDone (shortcardN);
  addDone (integerN);
  addDone (longintN);
  addDone (shortintN);
  addDone (bitsetN);
  addDone (ztypeN);
  addDone (rtypeN);
  addDone (realN);
  addDone (longrealN);
  addDone (shortrealN);
  addDone (procN);
  addDone (nilN);
  addDone (trueN);
  addDone (falseN);
}

static void makeBuiltins (void)
{
  bitsperunitN = decl_makeLiteralInt (nameKey_makeKey ((char *) "8", 1));
  bitsperwordN = decl_makeLiteralInt (nameKey_makeKey ((char *) "32", 2));
  bitspercharN = decl_makeLiteralInt (nameKey_makeKey ((char *) "8", 1));
  unitsperwordN = decl_makeLiteralInt (nameKey_makeKey ((char *) "4", 1));
  addDone (bitsperunitN);
  addDone (bitsperwordN);
  addDone (bitspercharN);
  addDone (unitsperwordN);
}

static void init (void)
{
  lang = ansiC;
  outputFile = FIO_StdOut;
  doP = mcPretty_initPretty ((mcPretty_writeProc) {(mcPretty_writeProc_t) write}, (mcPretty_writeLnProc) {(mcPretty_writeLnProc_t) writeln});
  todoQ = alists_initList ();
  partialQ = alists_initList ();
  doneQ = alists_initList ();
  modUniverse = symbolKey_initTree ();
  defUniverse = symbolKey_initTree ();
  modUniverseI = Indexing_InitIndex (1);
  defUniverseI = Indexing_InitIndex (1);
  scopeStack = Indexing_InitIndex (1);
  makeBaseSymbols ();
  makeSystem ();
  makeBuiltins ();
  makeM2rts ();
  outputState = punct;
  tempCount = 0;
  mustVisitScope = FALSE;
}

unsigned int decl_getDeclaredMod (decl_node n)
{
  return n->at.modDeclared;
}

unsigned int decl_getDeclaredDef (decl_node n)
{
  return n->at.defDeclared;
}

unsigned int decl_getFirstUsed (decl_node n)
{
  return n->at.firstUsed;
}

unsigned int decl_isDef (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == def;
}

unsigned int decl_isImp (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == imp;
}

unsigned int decl_isImpOrModule (decl_node n)
{
  return (decl_isImp (n)) || (decl_isModule (n));
}

unsigned int decl_isVisited (decl_node n)
{
  switch (n->kind)
    {
      case def:
        return n->defF.visited;
        break;

      case imp:
        return n->impF.visited;
        break;

      case module:
        return n->moduleF.visited;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

void decl_unsetVisited (decl_node n)
{
  switch (n->kind)
    {
      case def:
        n->defF.visited = FALSE;
        break;

      case imp:
        n->impF.visited = FALSE;
        break;

      case module:
        n->moduleF.visited = FALSE;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

void decl_setVisited (decl_node n)
{
  switch (n->kind)
    {
      case def:
        n->defF.visited = TRUE;
        break;

      case imp:
        n->impF.visited = TRUE;
        break;

      case module:
        n->moduleF.visited = TRUE;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

void decl_setEnumsComplete (decl_node n)
{
  switch (n->kind)
    {
      case def:
        n->defF.enumsComplete = TRUE;
        break;

      case imp:
        n->impF.enumsComplete = TRUE;
        break;

      case module:
        n->moduleF.enumsComplete = TRUE;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

unsigned int decl_getEnumsComplete (decl_node n)
{
  switch (n->kind)
    {
      case def:
        return n->defF.enumsComplete;
        break;

      case imp:
        return n->impF.enumsComplete;
        break;

      case module:
        return n->moduleF.enumsComplete;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

void decl_resetEnumPos (decl_node n)
{
  mcDebug_assert (((decl_isDef (n)) || (decl_isImp (n))) || (decl_isModule (n)));
  if (decl_isDef (n))
    n->defF.enumFixup.count = 0;
  else if (decl_isImp (n))
    n->impF.enumFixup.count = 0;
  else if (decl_isModule (n))
    n->moduleF.enumFixup.count = 0;
}

decl_node decl_getNextEnum (void)
{
  decl_node n;

  n = NULL;
  mcDebug_assert (((decl_isDef (currentModule)) || (decl_isImp (currentModule))) || (decl_isModule (currentModule)));
  if (decl_isDef (currentModule))
    n = getNextFixup (&currentModule->defF.enumFixup);
  else if (decl_isImp (currentModule))
    n = getNextFixup (&currentModule->impF.enumFixup);
  else if (decl_isModule (currentModule))
    n = getNextFixup (&currentModule->moduleF.enumFixup);
  mcDebug_assert (n != NULL);
  mcDebug_assert ((decl_isEnumeration (n)) || (decl_isEnumerationField (n)));
  return n;
}

unsigned int decl_isModule (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == module;
}

unsigned int decl_isMainModule (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n == mainModule;
}

void decl_setMainModule (decl_node n)
{
  mcDebug_assert (n != NULL);
  mainModule = n;
}

void decl_setCurrentModule (decl_node n)
{
  mcDebug_assert (n != NULL);
  currentModule = n;
}

decl_node decl_lookupDef (nameKey_Name n)
{
  decl_node d;

  d = symbolKey_getSymKey (defUniverse, n);
  if (d == NULL)
    {
      d = makeDef (n);
      symbolKey_putSymKey (defUniverse, n, (void *) d);
      Indexing_IncludeIndiceIntoIndex (defUniverseI, (void *) d);
    }
  return d;
}

decl_node decl_lookupImp (nameKey_Name n)
{
  decl_node m;

  m = symbolKey_getSymKey (modUniverse, n);
  if (m == NULL)
    {
      m = makeImp (n);
      symbolKey_putSymKey (modUniverse, n, (void *) m);
      Indexing_IncludeIndiceIntoIndex (modUniverseI, (void *) m);
    }
  mcDebug_assert (! (decl_isModule (m)));
  return m;
}

decl_node decl_lookupModule (nameKey_Name n)
{
  decl_node m;

  m = symbolKey_getSymKey (modUniverse, n);
  if (m == NULL)
    {
      m = makeModule (n);
      symbolKey_putSymKey (modUniverse, n, (void *) m);
      Indexing_IncludeIndiceIntoIndex (modUniverseI, (void *) m);
    }
  mcDebug_assert (! (decl_isImp (m)));
  return m;
}

void decl_putDefForC (decl_node n)
{
  mcDebug_assert (decl_isDef (n));
}

decl_node decl_lookupInScope (decl_node scope, nameKey_Name n)
{
  switch (scope->kind)
    {
      case def:
        return symbolKey_getSymKey (scope->defF.decls.symbols, n);
        break;

      case module:
        return symbolKey_getSymKey (scope->moduleF.decls.symbols, n);
        break;

      case imp:
        return symbolKey_getSymKey (scope->impF.decls.symbols, n);
        break;

      case procedure:
        return symbolKey_getSymKey (scope->procedureF.decls.symbols, n);
        break;

      case record:
        return symbolKey_getSymKey (scope->recordF.localSymbols, n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

unsigned int decl_isConst (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == const_;
}

unsigned int decl_isType (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == type;
}

void decl_putType (decl_node des, decl_node exp)
{
  mcDebug_assert (des != NULL);
  mcDebug_assert (decl_isType (des));
  des->typeF.type = exp;
}

decl_node decl_getType (decl_node n)
{
  switch (n->kind)
    {
      case new:
      case dispose:
      case inc:
      case dec:
      case incl:
      case excl:
        return NULL;
        break;

      case nil:
        return addressN;
        break;

      case true:
      case false:
        return booleanN;
        break;

      case address:
        return n;
        break;

      case loc:
        return n;
        break;

      case byte:
        return n;
        break;

      case word:
        return n;
        break;

      case boolean:
        return n;
        break;

      case proc:
        return n;
        break;

      case char_:
        return n;
        break;

      case cardinal:
        return n;
        break;

      case longcard:
        return n;
        break;

      case shortcard:
        return n;
        break;

      case integer:
        return n;
        break;

      case longint:
        return n;
        break;

      case shortint:
        return n;
        break;

      case real:
        return n;
        break;

      case longreal:
        return n;
        break;

      case shortreal:
        return n;
        break;

      case bitset:
        return n;
        break;

      case ztype:
        return n;
        break;

      case rtype:
        return n;
        break;

      case type:
        return n->typeF.type;
        break;

      case record:
        return n;
        break;

      case varient:
        return n;
        break;

      case var:
        return n->varF.type;
        break;

      case enumeration:
        return n;
        break;

      case subrange:
        return n->subrangeF.type;
        break;

      case array:
        return n->arrayF.type;
        break;

      case string:
        return charN;
        break;

      case const_:
        return n->constF.type;
        break;

      case literal:
        return n->literalF.type;
        break;

      case varparam:
        return n->varparamF.type;
        break;

      case param:
        return n->paramF.type;
        break;

      case optarg_:
        return n->optargF.type;
        break;

      case pointer:
        return n->pointerF.type;
        break;

      case recordfield:
        return n->recordfieldF.type;
        break;

      case varientfield:
        return n;
        break;

      case enumerationfield:
        return n->enumerationfieldF.type;
        break;

      case set:
        return n->setF.type;
        break;

      case proctype:
        return n->proctypeF.returnType;
        break;

      case subscript:
        return n->subscriptF.type;
        break;

      case procedure:
        return n->procedureF.returnType;
        break;

      case throw:
        return NULL;
        break;

      case def:
      case imp:
      case module:
      case loop:
      case while_:
      case for_:
      case repeat:
      case if_:
      case elsif:
      case assignment:
        M2RTS_HALT (0);
        break;

      case cast:
      case val:
      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
        return n->binaryF.resultType;
        break;

      case in:
        return booleanN;
        break;

      case abs_:
      case constexp:
      case deref:
      case neg:
      case adr:
      case size:
      case tsize:
        return n->unaryF.resultType;
        break;

      case and:
      case or:
      case not:
      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
        return booleanN;
        break;

      case trunc:
        return integerN;
        break;

      case float_:
        return realN;
        break;

      case high:
        return cardinalN;
        break;

      case ord:
        return cardinalN;
        break;

      case chr:
        return charN;
        break;

      case arrayref:
        return n->arrayrefF.resultType;
        break;

      case componentref:
        return n->componentrefF.resultType;
        break;

      case pointerref:
        return n->pointerrefF.resultType;
        break;

      case funccall:
        return n->funccallF.type;
        break;

      case setvalue:
        return n->setvalueF.type;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  M2RTS_HALT (0);
}

decl_node decl_skipType (decl_node n)
{
  while ((n != NULL) && (decl_isType (n)))
    {
      if ((decl_getType (n)) == NULL)
        return n;
      n = decl_getType (n);
    }
  return n;
}

void decl_putTypeHidden (decl_node des)
{
  decl_node s;

  mcDebug_assert (des != NULL);
  mcDebug_assert (decl_isType (des));
  des->typeF.isHidden = TRUE;
  s = decl_getScope (des);
  mcDebug_assert (decl_isDef (s));
  s->defF.hasHidden = TRUE;
}

unsigned int decl_isTypeHidden (decl_node n)
{
  mcDebug_assert (n != NULL);
  mcDebug_assert (decl_isType (n));
  return n->typeF.isHidden;
}

unsigned int decl_hasHidden (decl_node n)
{
  mcDebug_assert (decl_isDef (n));
  return n->defF.hasHidden;
}

unsigned int decl_isVar (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == var;
}

unsigned int decl_isTemporary (decl_node n)
{
  return FALSE;
}

unsigned int decl_isExported (decl_node n)
{
  decl_node s;

  s = decl_getScope (n);
  if (s != NULL)
    switch (s->kind)
      {
        case def:
          return Indexing_IsIndiceInIndex (s->defF.exported, (void *) n);
          break;


        default:
          return FALSE;
          break;
      }
  return FALSE;
}

decl_node decl_getDeclScope (void)
{
  unsigned int i;

  i = Indexing_HighIndice (scopeStack);
  return Indexing_GetIndice (scopeStack, i);
}

decl_node decl_getScope (decl_node n)
{
  switch (n->kind)
    {
      case stmtseq:
      case exit_:
      case return_:
      case comment:
      case identlist:
      case setvalue:
      case halt:
      case new:
      case dispose:
      case inc:
      case dec:
      case incl:
      case excl:
      case nil:
      case true:
      case false:
        return NULL;
        break;

      case address:
      case loc:
      case byte:
      case word:
        return systemN;
        break;

      case boolean:
      case proc:
      case char_:
      case cardinal:
      case longcard:
      case shortcard:
      case integer:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case ztype:
      case rtype:
        return NULL;
        break;

      case type:
        return n->typeF.scope;
        break;

      case record:
        return n->recordF.scope;
        break;

      case varient:
        return n->varientF.scope;
        break;

      case var:
        return n->varF.scope;
        break;

      case enumeration:
        return n->enumerationF.scope;
        break;

      case subrange:
        return n->subrangeF.scope;
        break;

      case array:
        return n->arrayF.scope;
        break;

      case string:
        return NULL;
        break;

      case const_:
        return n->constF.scope;
        break;

      case literal:
        return NULL;
        break;

      case varparam:
        return n->varparamF.scope;
        break;

      case param:
        return n->paramF.scope;
        break;

      case optarg_:
        return n->optargF.scope;
        break;

      case pointer:
        return n->pointerF.scope;
        break;

      case recordfield:
        return n->recordfieldF.scope;
        break;

      case varientfield:
        return n->varientfieldF.scope;
        break;

      case enumerationfield:
        return n->enumerationfieldF.scope;
        break;

      case set:
        return n->setF.scope;
        break;

      case proctype:
        return n->proctypeF.scope;
        break;

      case subscript:
        return NULL;
        break;

      case procedure:
        return n->procedureF.scope;
        break;

      case def:
      case imp:
      case module:
      case case_:
      case loop:
      case while_:
      case for_:
      case repeat:
      case if_:
      case elsif:
      case assignment:
        return NULL;
        break;

      case componentref:
      case pointerref:
      case arrayref:
      case chr:
      case ord:
      case float_:
      case trunc:
      case high:
      case cast:
      case val:
      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
      case in:
        return NULL;
        break;

      case neg:
        return NULL;
        break;

      case lsl:
      case lsr:
      case lor:
      case land:
      case lnot:
      case lxor:
      case and:
      case or:
      case not:
      case constexp:
      case deref:
      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
        return NULL;
        break;

      case adr:
      case size:
      case tsize:
      case throw:
        return systemN;
        break;

      case min:
      case max:
        return NULL;
        break;

      case vardecl:
        return n->vardeclF.scope;
        break;

      case funccall:
        return NULL;
        break;

      case explist:
        return NULL;
        break;

      case caselabellist:
        return NULL;
        break;

      case caselist:
        return NULL;
        break;

      case range:
        return NULL;
        break;

      case varargs:
        return n->varargsF.scope;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

unsigned int decl_isLiteral (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == literal;
}

unsigned int decl_isConstSet (decl_node n)
{
  mcDebug_assert (n != NULL);
  if ((decl_isLiteral (n)) || (decl_isConst (n)))
    return decl_isSet (decl_skipType (decl_getType (n)));
  return FALSE;
}

unsigned int decl_isEnumerationField (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == enumerationfield;
}

unsigned int decl_isEnumeration (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == enumeration;
}

unsigned int decl_isUnbounded (decl_node n)
{
  mcDebug_assert (n != NULL);
  return (n->kind == array) && n->arrayF.isUnbounded;
}

unsigned int decl_isParameter (decl_node n)
{
  mcDebug_assert (n != NULL);
  return (n->kind == param) || (n->kind == varparam);
}

unsigned int decl_isVarParam (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == varparam;
}

unsigned int decl_isParam (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == param;
}

unsigned int decl_isNonVarParam (decl_node n)
{
  return decl_isParam (n);
}

decl_node decl_addOptParameter (decl_node proc, nameKey_Name id, decl_node type, decl_node init)
{
  decl_node p;
  decl_node l;

  mcDebug_assert (decl_isProcedure (proc));
  l = decl_makeIdentList ();
  mcDebug_assert (decl_putIdent (l, id));
  checkMakeVariables (proc, l, type, FALSE);
  if (! proc->procedureF.checking)
    {
      p = makeOptParameter (l, type, init);
      decl_addParameter (proc, p);
    }
  return p;
}

unsigned int decl_isOptarg (decl_node n)
{
  return n->kind == optarg_;
}

unsigned int decl_isRecord (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == record;
}

unsigned int decl_isRecordField (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == recordfield;
}

unsigned int decl_isVarientField (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == varientfield;
}

unsigned int decl_isArray (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == array;
}

unsigned int decl_isProcType (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == proctype;
}

unsigned int decl_isPointer (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == pointer;
}

unsigned int decl_isProcedure (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == procedure;
}

unsigned int decl_isVarient (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == varient;
}

unsigned int decl_isSet (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == set;
}

unsigned int decl_isSubrange (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == subrange;
}

unsigned int decl_isZtype (decl_node n)
{
  return n == ztypeN;
}

unsigned int decl_isRtype (decl_node n)
{
  return n == rtypeN;
}

decl_node decl_makeConst (nameKey_Name n)
{
  decl_node d;

  d = newNode ((nodeT) const_);
  d->constF.name = n;
  d->constF.type = NULL;
  d->constF.scope = decl_getDeclScope ();
  d->constF.value = NULL;
  return addToScope (d);
}

void decl_putConst (decl_node n, decl_node v)
{
  mcDebug_assert (decl_isConst (n));
  n->constF.value = v;
}

decl_node decl_makeType (nameKey_Name n)
{
  decl_node d;

  d = newNode ((nodeT) type);
  d->typeF.name = n;
  d->typeF.type = NULL;
  d->typeF.scope = decl_getDeclScope ();
  d->typeF.isHidden = FALSE;
  d->typeF.isInternal = FALSE;
  return addToScope (d);
}

decl_node decl_makeTypeImp (nameKey_Name n)
{
  decl_node d;

  d = decl_lookupSym (n);
  if (d != NULL)
    {
      d->typeF.isHidden = FALSE;
      return addToScope (d);
    }
  else
    {
      d = newNode ((nodeT) type);
      d->typeF.name = n;
      d->typeF.type = NULL;
      d->typeF.scope = decl_getDeclScope ();
      d->typeF.isHidden = FALSE;
      return addToScope (d);
    }
}

decl_node decl_makeVar (nameKey_Name n)
{
  decl_node d;

  d = newNode ((nodeT) var);
  d->varF.name = n;
  d->varF.type = NULL;
  d->varF.decl = NULL;
  d->varF.scope = decl_getDeclScope ();
  d->varF.isInitialised = FALSE;
  d->varF.isParameter = FALSE;
  d->varF.isVarParameter = FALSE;
  return addToScope (d);
}

void decl_putVar (decl_node var, decl_node type, decl_node decl)
{
  mcDebug_assert (var != NULL);
  mcDebug_assert (decl_isVar (var));
  var->varF.type = type;
  var->varF.decl = decl;
}

decl_node decl_makeVarDecl (decl_node i, decl_node type)
{
  decl_node d;
  decl_node v;
  unsigned int j;
  unsigned int n;

  d = newNode ((nodeT) vardecl);
  d->vardeclF.names = i->identlistF.names;
  d->vardeclF.type = type;
  d->vardeclF.scope = decl_getDeclScope ();
  n = wlists_noOfItemsInList (d->vardeclF.names);
  j = 1;
  while (j <= n)
    {
      v = decl_lookupSym ((nameKey_Name) wlists_getItemFromList (d->vardeclF.names, j));
      mcDebug_assert (decl_isVar (v));
      decl_putVar (v, type, d);
      j += 1;
    }
  return d;
}

decl_node decl_makeEnum (void)
{
  if ((currentModule != NULL) && (decl_getEnumsComplete (currentModule)))
    return decl_getNextEnum ();
  else
    return doMakeEnum ();
}

decl_node decl_makeEnumField (decl_node e, nameKey_Name n)
{
  if ((currentModule != NULL) && (decl_getEnumsComplete (currentModule)))
    return decl_getNextEnum ();
  else
    return doMakeEnumField (e, n);
}

decl_node decl_makeSubrange (decl_node low, decl_node high)
{
  decl_node n;

  n = newNode ((nodeT) subrange);
  n->subrangeF.low = low;
  n->subrangeF.high = high;
  n->subrangeF.type = NULL;
  n->subrangeF.scope = decl_getDeclScope ();
  return n;
}

void decl_putSubrangeType (decl_node sub, decl_node type)
{
  mcDebug_assert (decl_isSubrange (sub));
  sub->subrangeF.type = type;
}

decl_node decl_makePointer (decl_node type)
{
  decl_node n;

  n = newNode ((nodeT) pointer);
  n->pointerF.type = type;
  n->pointerF.scope = decl_getDeclScope ();
  return n;
}

decl_node decl_makeSet (decl_node type)
{
  decl_node n;

  n = newNode ((nodeT) set);
  n->setF.type = type;
  n->setF.scope = decl_getDeclScope ();
  return n;
}

decl_node decl_makeArray (decl_node subr, decl_node type)
{
  decl_node n;
  decl_node s;

  s = decl_skipType (subr);
  mcDebug_assert ((decl_isSubrange (s)) || (isOrdinal (s)));
  n = newNode ((nodeT) array);
  n->arrayF.subr = subr;
  n->arrayF.type = type;
  n->arrayF.scope = decl_getDeclScope ();
  n->arrayF.isUnbounded = FALSE;
  return n;
}

void decl_putUnbounded (decl_node n)
{
  mcDebug_assert (n->kind == array);
  n->arrayF.isUnbounded = TRUE;
}

decl_node decl_makeRecord (void)
{
  decl_node n;

  n = newNode ((nodeT) record);
  n->recordF.localSymbols = symbolKey_initTree ();
  n->recordF.listOfSons = Indexing_InitIndex (1);
  n->recordF.scope = decl_getDeclScope ();
  return n;
}

decl_node decl_makeVarient (decl_node r)
{
  decl_node n;

  n = newNode ((nodeT) varient);
  n->varientF.listOfSons = Indexing_InitIndex (1);
  if (decl_isRecord (r))
    n->varientF.varient = NULL;
  else
    n->varientF.varient = r;
  n->varientF.tag = NULL;
  n->varientF.scope = decl_getDeclScope ();
  switch (r->kind)
    {
      case record:
        Indexing_IncludeIndiceIntoIndex (r->recordF.listOfSons, (void *) n);
        break;

      case varientfield:
        Indexing_IncludeIndiceIntoIndex (r->varientfieldF.listOfSons, (void *) n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  return n;
}

decl_node decl_addFieldsToRecord (decl_node r, decl_node v, decl_node i, decl_node t)
{
  decl_node p;
  decl_node fj;
  unsigned int j;
  unsigned int n;
  nameKey_Name fn;

  if (decl_isRecord (r))
    {
      p = r;
      v = NULL;
    }
  else
    {
      p = getRecord (getParent (r));
      mcDebug_assert (decl_isVarientField (r));
      mcDebug_assert (decl_isVarient (v));
      putFieldVarient (r, v);
    }
  n = wlists_noOfItemsInList (i->identlistF.names);
  j = 1;
  while (j <= n)
    {
      fn = wlists_getItemFromList (i->identlistF.names, j);
      fj = symbolKey_getSymKey (p->recordF.localSymbols, (nameKey_Name) n);
      if (fj == NULL)
        fj = putFieldRecord (r, fn, t, v);
      else
        mcMetaError_metaErrors2 ((char *) "record field {%1ad} has already been declared inside a {%2Dd} {%2a}", 67, (char *) "attempting to declare a duplicate record field", 46, (unsigned char *) &fj, sizeof (fj), (unsigned char *) &p, sizeof (p));
      j += 1;
    }
  return r;
}

void decl_buildVarientSelector (decl_node r, decl_node v, nameKey_Name tag, decl_node type)
{
  decl_node f;

  mcDebug_assert ((decl_isRecord (r)) || (decl_isVarientField (r)));
  if ((decl_isRecord (r)) || (decl_isVarientField (r)))
    if ((type == NULL) && (tag == nameKey_NulName))
      mcMetaError_metaError1 ((char *) "expecting a tag field in the declaration of a varient record {%1Ua}", 67, (unsigned char *) &r, sizeof (r));
    else if (type == NULL)
      {
        f = decl_lookupSym (tag);
        putVarientTag (v, f);
      }
    else
      {
        f = putFieldRecord (r, tag, type, v);
        mcDebug_assert (decl_isRecordField (f));
        f->recordfieldF.tag = TRUE;
        putVarientTag (v, f);
      }
}

decl_node decl_buildVarientFieldRecord (decl_node v, decl_node p)
{
  decl_node f;

  mcDebug_assert (decl_isVarient (v));
  f = makeVarientField (v, p);
  mcDebug_assert (decl_isVarientField (f));
  putFieldVarient (f, v);
  return f;
}

nameKey_Name decl_getSymName (decl_node n)
{
  switch (n->kind)
    {
      case new:
        return nameKey_makeKey ((char *) "NEW", 3);
        break;

      case dispose:
        return nameKey_makeKey ((char *) "DISPOSE", 7);
        break;

      case inc:
        return nameKey_makeKey ((char *) "INC", 3);
        break;

      case dec:
        return nameKey_makeKey ((char *) "DEC", 3);
        break;

      case incl:
        return nameKey_makeKey ((char *) "INCL", 4);
        break;

      case excl:
        return nameKey_makeKey ((char *) "EXCL", 4);
        break;

      case nil:
        return nameKey_makeKey ((char *) "NIL", 3);
        break;

      case true:
        return nameKey_makeKey ((char *) "TRUE", 4);
        break;

      case false:
        return nameKey_makeKey ((char *) "FALSE", 5);
        break;

      case address:
        return nameKey_makeKey ((char *) "ADDRESS", 7);
        break;

      case loc:
        return nameKey_makeKey ((char *) "LOC", 3);
        break;

      case byte:
        return nameKey_makeKey ((char *) "BYTE", 4);
        break;

      case word:
        return nameKey_makeKey ((char *) "WORD", 4);
        break;

      case boolean:
        return nameKey_makeKey ((char *) "BOOLEAN", 7);
        break;

      case proc:
        return nameKey_makeKey ((char *) "PROC", 4);
        break;

      case char_:
        return nameKey_makeKey ((char *) "CHAR", 4);
        break;

      case cardinal:
        return nameKey_makeKey ((char *) "CARDINAL", 8);
        break;

      case longcard:
        return nameKey_makeKey ((char *) "LONGCARD", 8);
        break;

      case shortcard:
        return nameKey_makeKey ((char *) "SHORTCARD", 9);
        break;

      case integer:
        return nameKey_makeKey ((char *) "INTEGER", 7);
        break;

      case longint:
        return nameKey_makeKey ((char *) "LONGINT", 7);
        break;

      case shortint:
        return nameKey_makeKey ((char *) "SHORTINT", 8);
        break;

      case real:
        return nameKey_makeKey ((char *) "REAL", 4);
        break;

      case longreal:
        return nameKey_makeKey ((char *) "LONGREAL", 8);
        break;

      case shortreal:
        return nameKey_makeKey ((char *) "SHORTREAL", 9);
        break;

      case bitset:
        return nameKey_makeKey ((char *) "BITSET", 6);
        break;

      case ztype:
        return nameKey_makeKey ((char *) "_ZTYPE", 6);
        break;

      case rtype:
        return nameKey_makeKey ((char *) "_RTYPE", 6);
        break;

      case type:
        return n->typeF.name;
        break;

      case record:
        return nameKey_NulName;
        break;

      case varient:
        return nameKey_NulName;
        break;

      case var:
        return n->varF.name;
        break;

      case enumeration:
        return nameKey_NulName;
        break;

      case subrange:
        return nameKey_NulName;
        break;

      case pointer:
        return nameKey_NulName;
        break;

      case array:
        return nameKey_NulName;
        break;

      case string:
        return n->stringF.name;
        break;

      case const_:
        return n->constF.name;
        break;

      case literal:
        return n->literalF.name;
        break;

      case varparam:
        return nameKey_NulName;
        break;

      case param:
        return nameKey_NulName;
        break;

      case optarg_:
        return nameKey_NulName;
        break;

      case recordfield:
        return n->recordfieldF.name;
        break;

      case varientfield:
        return n->varientfieldF.name;
        break;

      case enumerationfield:
        return n->enumerationfieldF.name;
        break;

      case set:
        return nameKey_NulName;
        break;

      case proctype:
        return nameKey_NulName;
        break;

      case subscript:
        return nameKey_NulName;
        break;

      case procedure:
        return n->procedureF.name;
        break;

      case def:
        return n->defF.name;
        break;

      case imp:
        return n->impF.name;
        break;

      case module:
        return n->moduleF.name;
        break;

      case loop:
      case while_:
      case for_:
      case repeat:
      case if_:
      case elsif:
      case assignment:
        return nameKey_NulName;
        break;

      case constexp:
      case deref:
      case arrayref:
      case componentref:
      case cast:
      case val:
      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
      case in:
      case neg:
      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
        return nameKey_NulName;
        break;

      case adr:
        return nameKey_makeKey ((char *) "ADR", 3);
        break;

      case size:
        return nameKey_makeKey ((char *) "SIZE", 4);
        break;

      case tsize:
        return nameKey_makeKey ((char *) "TSIZE", 5);
        break;

      case chr:
        return nameKey_makeKey ((char *) "CHR", 3);
        break;

      case abs_:
        return nameKey_makeKey ((char *) "ABS", 3);
        break;

      case ord:
        return nameKey_makeKey ((char *) "ORD", 3);
        break;

      case float_:
        return nameKey_makeKey ((char *) "FLOAT", 5);
        break;

      case trunc:
        return nameKey_makeKey ((char *) "TRUNC", 5);
        break;

      case high:
        return nameKey_makeKey ((char *) "HIGH", 4);
        break;

      case throw:
        return nameKey_makeKey ((char *) "THROW", 5);
        break;

      case max:
        return nameKey_makeKey ((char *) "MAX", 3);
        break;

      case min:
        return nameKey_makeKey ((char *) "MIN", 3);
        break;

      case funccall:
        return nameKey_NulName;
        break;

      case identlist:
        return nameKey_NulName;
        break;


      default:
        M2RTS_HALT (0);
        break;
    }
}

decl_node decl_import (decl_node m, decl_node n)
{
  nameKey_Name name;
  decl_node r;

  mcDebug_assert (((decl_isDef (m)) || (decl_isModule (m))) || (decl_isImp (m)));
  name = decl_getSymName (n);
  r = decl_lookupInScope (m, name);
  if (r == NULL)
    {
      switch (m->kind)
        {
          case def:
            symbolKey_putSymKey (m->defF.decls.symbols, name, (void *) n);
            break;

          case imp:
            symbolKey_putSymKey (m->impF.decls.symbols, name, (void *) n);
            break;

          case module:
            symbolKey_putSymKey (m->moduleF.decls.symbols, name, (void *) n);
            break;


          default:
            CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
        }
      importEnumFields (m, n);
      return n;
    }
  return r;
}

decl_node decl_lookupExported (decl_node n, nameKey_Name i)
{
  decl_node r;

  mcDebug_assert (decl_isDef (n));
  r = symbolKey_getSymKey (n->defF.decls.symbols, i);
  if ((r != NULL) && (decl_isExported (r)))
    return r;
  return NULL;
}

decl_node decl_lookupSym (nameKey_Name n)
{
  decl_node s;
  decl_node m;
  unsigned int l;
  unsigned int h;

  l = Indexing_LowIndice (scopeStack);
  h = Indexing_HighIndice (scopeStack);
  while (h >= l)
    {
      s = Indexing_GetIndice (scopeStack, h);
      m = decl_lookupInScope (s, n);
      if (debugScopes && (m == NULL))
        out3 ((char *) " [%d] search for symbol name %s in scope %s\\n", 45, h, n, s);
      if (m != NULL)
        {
          if (debugScopes)
            out3 ((char *) " [%d] search for symbol name %s in scope %s (found)\\n", 53, h, n, s);
          return m;
        }
      h -= 1;
    }
  return lookupBase (n);
}

void decl_addImportedModule (decl_node m, decl_node i, unsigned int scoped)
{
  mcDebug_assert ((decl_isDef (i)) || (decl_isModule (i)));
  if (decl_isDef (m))
    Indexing_IncludeIndiceIntoIndex (m->defF.importedModules, (void *) i);
  else if (decl_isImp (m))
    Indexing_IncludeIndiceIntoIndex (m->impF.importedModules, (void *) i);
  else if (decl_isModule (m))
    Indexing_IncludeIndiceIntoIndex (m->moduleF.importedModules, (void *) i);
  else
    M2RTS_HALT (0);
  if (scoped)
    addModuleToScope (m, i);
}

void decl_setSource (decl_node n, nameKey_Name s)
{
  switch (n->kind)
    {
      case def:
        n->defF.source = s;
        break;

      case module:
        n->moduleF.source = s;
        break;

      case imp:
        n->impF.source = s;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

nameKey_Name decl_getSource (decl_node n)
{
  switch (n->kind)
    {
      case def:
        return n->defF.source;
        break;

      case module:
        return n->moduleF.source;
        break;

      case imp:
        return n->impF.source;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

decl_node decl_getMainModule (void)
{
  return mainModule;
}

decl_node decl_getCurrentModule (void)
{
  return currentModule;
}

void decl_foreachDefModuleDo (symbolKey_performOperation p)
{
  Indexing_ForeachIndiceInIndexDo (defUniverseI, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) p.proc});
}

void decl_foreachModModuleDo (symbolKey_performOperation p)
{
  Indexing_ForeachIndiceInIndexDo (modUniverseI, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) p.proc});
}

void decl_enterScope (decl_node n)
{
  if (Indexing_IsIndiceInIndex (scopeStack, (void *) n))
    M2RTS_HALT (0);
  else
    Indexing_IncludeIndiceIntoIndex (scopeStack, (void *) n);
  if (debugScopes)
    {
      libc_printf ((char *) "enter scope\\n", 13);
      dumpScopes ();
    }
}

void decl_leaveScope (void)
{
  unsigned int i;
  decl_node n;

  i = Indexing_HighIndice (scopeStack);
  n = Indexing_GetIndice (scopeStack, i);
  Indexing_RemoveIndiceFromIndex (scopeStack, (void *) n);
  if (debugScopes)
    {
      libc_printf ((char *) "leave scope\\n", 13);
      dumpScopes ();
    }
}

decl_node decl_makeProcedure (nameKey_Name n)
{
  decl_node d;

  d = decl_lookupSym (n);
  if (d == NULL)
    {
      d = newNode ((nodeT) procedure);
      d->procedureF.name = n;
      initDecls (&d->procedureF.decls);
      d->procedureF.scope = decl_getDeclScope ();
      d->procedureF.parameters = Indexing_InitIndex (1);
      d->procedureF.built = FALSE;
      d->procedureF.returnopt = FALSE;
      d->procedureF.optarg_ = NULL;
      d->procedureF.vararg = FALSE;
      d->procedureF.checking = FALSE;
      d->procedureF.paramcount = 0;
      d->procedureF.returnType = NULL;
      d->procedureF.beginStatements = NULL;
    }
  return addProcedureToScope (d, n);
}

decl_node decl_makeProcType (void)
{
  decl_node d;

  d = newNode ((nodeT) proctype);
  d->proctypeF.scope = decl_getDeclScope ();
  d->proctypeF.parameters = Indexing_InitIndex (1);
  d->proctypeF.returnopt = FALSE;
  d->proctypeF.optarg_ = NULL;
  d->proctypeF.vararg = FALSE;
  d->proctypeF.returnType = NULL;
  return d;
}

void decl_putProcTypeOptReturn (decl_node proc)
{
  proc->proctypeF.returnopt = TRUE;
}

void decl_putReturnType (decl_node p, decl_node type)
{
  mcDebug_assert (p != NULL);
  mcDebug_assert ((decl_isProcedure (p)) || (decl_isProcType (p)));
  if (p->kind == procedure)
    p->procedureF.returnType = type;
  else
    p->proctypeF.returnType = type;
}

decl_node decl_makeVarParameter (decl_node l, decl_node type)
{
  decl_node d;

  mcDebug_assert ((l == NULL) || (isIdentList (l)));
  d = newNode ((nodeT) varparam);
  d->varparamF.namelist = l;
  d->varparamF.type = type;
  d->varparamF.scope = NULL;
  d->varparamF.isUnbounded = FALSE;
  return d;
}

decl_node decl_makeNonVarParameter (decl_node l, decl_node type)
{
  decl_node d;

  mcDebug_assert ((l == NULL) || (isIdentList (l)));
  d = newNode ((nodeT) param);
  d->paramF.namelist = l;
  d->paramF.type = type;
  d->paramF.scope = NULL;
  d->paramF.isUnbounded = FALSE;
  return d;
}

void decl_paramEnter (decl_node n)
{
  mcDebug_assert (decl_isProcedure (n));
  n->procedureF.paramcount = 0;
}

void decl_paramLeave (decl_node n)
{
  mcDebug_assert (decl_isProcedure (n));
  n->procedureF.checking = TRUE;
  if ((decl_isImp (currentModule)) || (decl_isModule (currentModule)))
    n->procedureF.built = TRUE;
}

decl_node decl_makeIdentList (void)
{
  decl_node n;

  n = newNode ((nodeT) identlist);
  n->identlistF.names = wlists_initList ();
  return n;
}

unsigned int decl_putIdent (decl_node n, nameKey_Name i)
{
  mcDebug_assert (isIdentList (n));
  if (wlists_isItemInList (n->identlistF.names, (unsigned int ) i))
    return FALSE;
  else
    {
      wlists_putItemIntoList (n->identlistF.names, (unsigned int ) i);
      return TRUE;
    }
}

void decl_addVarParameters (decl_node n, decl_node i, decl_node type)
{
  decl_node p;

  mcDebug_assert (isIdentList (i));
  mcDebug_assert (decl_isProcedure (n));
  checkMakeVariables (n, i, type, TRUE);
  if (n->procedureF.checking)
    checkParameters (n, i, type, TRUE);
  else
    {
      p = decl_makeVarParameter (i, type);
      Indexing_IncludeIndiceIntoIndex (n->procedureF.parameters, (void *) p);
    }
}

void decl_addNonVarParameters (decl_node n, decl_node i, decl_node type)
{
  decl_node p;

  mcDebug_assert (isIdentList (i));
  mcDebug_assert (decl_isProcedure (n));
  checkMakeVariables (n, i, type, FALSE);
  if (n->procedureF.checking)
    checkParameters (n, i, type, FALSE);
  else
    {
      p = decl_makeNonVarParameter (i, type);
      Indexing_IncludeIndiceIntoIndex (n->procedureF.parameters, (void *) p);
    }
}

decl_node decl_makeVarargs (void)
{
  decl_node d;

  d = newNode ((nodeT) varargs);
  d->varargsF.scope = NULL;
  return d;
}

unsigned int decl_isVarargs (decl_node n)
{
  return n->kind == varargs;
}

void decl_addParameter (decl_node proc, decl_node param)
{
  mcDebug_assert ((((decl_isVarargs (param)) || (decl_isParam (param))) || (decl_isVarParam (param))) || (decl_isOptarg (param)));
  switch (proc->kind)
    {
      case procedure:
        Indexing_IncludeIndiceIntoIndex (proc->procedureF.parameters, (void *) param);
        if (decl_isVarargs (param))
          proc->procedureF.vararg = TRUE;
        if (decl_isOptarg (param))
          proc->procedureF.optarg_ = param;
        break;

      case proctype:
        Indexing_IncludeIndiceIntoIndex (proc->proctypeF.parameters, (void *) param);
        if (decl_isVarargs (param))
          proc->proctypeF.vararg = TRUE;
        if (decl_isOptarg (param))
          proc->proctypeF.optarg_ = param;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

decl_node decl_makeBinaryTok (mcReserved_toktype op, decl_node l, decl_node r)
{
  if (op == mcReserved_equaltok)
    return makeBinary ((nodeT) equal, l, r, booleanN);
  else if ((op == mcReserved_hashtok) || (op == mcReserved_lessgreatertok))
    return makeBinary ((nodeT) notequal, l, r, booleanN);
  else if (op == mcReserved_lesstok)
    return makeBinary ((nodeT) less, l, r, booleanN);
  else if (op == mcReserved_greatertok)
    return makeBinary ((nodeT) greater, l, r, booleanN);
  else if (op == mcReserved_greaterequaltok)
    return makeBinary ((nodeT) greequal, l, r, booleanN);
  else if (op == mcReserved_lessequaltok)
    return makeBinary ((nodeT) lessequal, l, r, booleanN);
  else if (op == mcReserved_andtok)
    return makeBinary ((nodeT) and, l, r, booleanN);
  else if (op == mcReserved_ortok)
    return makeBinary ((nodeT) or, l, r, booleanN);
  else if (op == mcReserved_plustok)
    return makeBinary ((nodeT) plus, l, r, (decl_node) NULL);
  else if (op == mcReserved_minustok)
    return makeBinary ((nodeT) sub, l, r, (decl_node) NULL);
  else if (op == mcReserved_divtok)
    return makeBinary ((nodeT) div_, l, r, (decl_node) NULL);
  else if (op == mcReserved_timestok)
    return makeBinary ((nodeT) mult, l, r, (decl_node) NULL);
  else if (op == mcReserved_modtok)
    return makeBinary ((nodeT) mod, l, r, (decl_node) NULL);
  else if (op == mcReserved_intok)
    return makeBinary ((nodeT) in, l, r, (decl_node) NULL);
  else if (op == mcReserved_dividetok)
    return makeBinary ((nodeT) divide, l, r, (decl_node) NULL);
  else
    M2RTS_HALT (0);
}

decl_node decl_makeUnaryTok (mcReserved_toktype op, decl_node e)
{
  if (op == mcReserved_nottok)
    return makeUnary ((nodeT) not, e, booleanN);
  else if (op == mcReserved_plustok)
    return makeUnary ((nodeT) plus, e, (decl_node) NULL);
  else if (op == mcReserved_minustok)
    return makeUnary ((nodeT) neg, e, (decl_node) NULL);
  else
    M2RTS_HALT (0);
}

decl_node decl_makeComponentRef (decl_node rec, decl_node field)
{
  decl_node n;
  decl_node a;

  if (isDeref (rec))
    {
      a = rec->unaryF.arg;
      rec->kind = pointerref;
      rec->pointerrefF.ptr = a;
      rec->pointerrefF.field = field;
      rec->pointerrefF.resultType = decl_getType (field);
      return rec;
    }
  else
    return doMakeComponentRef (rec, field);
}

decl_node decl_makePointerRef (decl_node ptr, decl_node field)
{
  decl_node n;

  n = newNode ((nodeT) pointerref);
  n->pointerrefF.ptr = ptr;
  n->pointerrefF.field = field;
  n->pointerrefF.resultType = decl_getType (field);
  return n;
}

unsigned int decl_isPointerRef (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == pointerref;
}

decl_node decl_makeDeRef (decl_node n)
{
  decl_node t;

  t = decl_skipType (decl_getType (n));
  mcDebug_assert (decl_isPointer (t));
  return makeUnary ((nodeT) deref, n, decl_getType (t));
}

decl_node decl_makeArrayRef (decl_node array, decl_node index)
{
  decl_node n;

  n = newNode ((nodeT) arrayref);
  n->arrayrefF.array = array;
  n->arrayrefF.index = index;
  n->arrayrefF.resultType = decl_getType (decl_getType (array));
  return n;
}

decl_node decl_getLastOp (decl_node n)
{
  return doGetLastOp (n, n);
}

decl_node decl_getCardinal (void)
{
  return cardinalN;
}

decl_node decl_makeLiteralInt (nameKey_Name n)
{
  decl_node m;
  DynamicStrings_String s;

  m = newNode ((nodeT) literal);
  s = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  m->literalF.name = n;
  if ((DynamicStrings_char (s, -1)) == 'C')
    m->literalF.type = charN;
  else
    m->literalF.type = ztypeN;
  s = DynamicStrings_KillString (s);
  return m;
}

decl_node decl_makeLiteralReal (nameKey_Name n)
{
  decl_node m;

  m = newNode ((nodeT) literal);
  m->literalF.name = n;
  m->literalF.type = rtypeN;
  return m;
}

decl_node decl_makeString (nameKey_Name n)
{
  decl_node m;

  m = newNode ((nodeT) string);
  m->stringF.name = n;
  m->stringF.length = nameKey_lengthKey (n);
  m->stringF.isCharCompatible = m->stringF.length <= 3;
  m->stringF.cstring = toCstring (n);
  m->stringF.clength = lenCstring (m->stringF.cstring);
  return m;
}

decl_node decl_makeSetValue (void)
{
  decl_node n;

  n = newNode ((nodeT) setvalue);
  n->setvalueF.type = bitsetN;
  n->setvalueF.values = Indexing_InitIndex (1);
  return n;
}

unsigned int decl_isSetValue (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == setvalue;
}

decl_node decl_putSetValue (decl_node n, decl_node t)
{
  mcDebug_assert (decl_isSetValue (n));
  n->setvalueF.type = t;
  return n;
}

decl_node decl_includeSetValue (decl_node n, decl_node l, decl_node h)
{
  mcDebug_assert (decl_isSetValue (n));
  Indexing_IncludeIndiceIntoIndex (n->setvalueF.values, (void *) l);
  return n;
}

decl_node decl_getBuiltinConst (nameKey_Name n)
{
  if (n == (nameKey_makeKey ((char *) "BITS_PER_UNIT", 13)))
    return bitsperunitN;
  else if (n == (nameKey_makeKey ((char *) "BITS_PER_WORD", 13)))
    return bitsperwordN;
  else if (n == (nameKey_makeKey ((char *) "BITS_PER_CHAR", 13)))
    return bitspercharN;
  else if (n == (nameKey_makeKey ((char *) "UNITS_PER_WORD", 14)))
    return unitsperwordN;
  else
    return NULL;
}

decl_node decl_makeExpList (void)
{
  decl_node n;

  n = newNode ((nodeT) explist);
  n->explistF.exp = Indexing_InitIndex (1);
  return n;
}

unsigned int decl_isExpList (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == explist;
}

void decl_putExpList (decl_node n, decl_node e)
{
  mcDebug_assert (n != NULL);
  mcDebug_assert (decl_isExpList (n));
  Indexing_PutIndice (n->explistF.exp, (Indexing_HighIndice (n->explistF.exp))+1, (void *) e);
}

decl_node decl_makeConstExp (void)
{
  if ((currentModule != NULL) && (getConstExpComplete (currentModule)))
    return decl_getNextConstExp ();
  else
    return doMakeConstExp ();
}

decl_node decl_getNextConstExp (void)
{
  decl_node n;

  mcDebug_assert (((decl_isDef (currentModule)) || (decl_isImp (currentModule))) || (decl_isModule (currentModule)));
  if (decl_isDef (currentModule))
    return getNextFixup (&currentModule->defF.constFixup);
  else if (decl_isImp (currentModule))
    return getNextFixup (&currentModule->impF.constFixup);
  else if (decl_isModule (currentModule))
    return getNextFixup (&currentModule->moduleF.constFixup);
  return n;
}

void decl_setConstExpComplete (decl_node n)
{
  switch (n->kind)
    {
      case def:
        n->defF.constsComplete = TRUE;
        break;

      case imp:
        n->impF.constsComplete = TRUE;
        break;

      case module:
        n->moduleF.constsComplete = TRUE;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

decl_node decl_fixupConstExp (decl_node c, decl_node e)
{
  mcDebug_assert (isConstExp (c));
  c->unaryF.arg = e;
  return c;
}

void decl_resetConstExpPos (decl_node n)
{
  mcDebug_assert (((decl_isDef (n)) || (decl_isImp (n))) || (decl_isModule (n)));
  if (decl_isDef (n))
    n->defF.constFixup.count = 0;
  else if (decl_isImp (n))
    n->impF.constFixup.count = 0;
  else if (decl_isModule (n))
    n->moduleF.constFixup.count = 0;
}

decl_node decl_makeFuncCall (decl_node c, decl_node n)
{
  decl_node f;

  mcDebug_assert ((n == NULL) || (decl_isExpList (n)));
  if (isAnyType (c))
    return makeCast (c, n);
  else
    {
      f = newNode ((nodeT) funccall);
      f->funccallF.function = c;
      f->funccallF.args = n;
      f->funccallF.type = NULL;
      return f;
    }
}

decl_node decl_makeStatementSequence (void)
{
  decl_node n;

  n = newNode ((nodeT) stmtseq);
  n->stmtF.statements = Indexing_InitIndex (1);
  return n;
}

unsigned int decl_isStatementSequence (decl_node n)
{
  return n->kind == stmtseq;
}

void decl_addStatement (decl_node s, decl_node n)
{
  if (n != NULL)
    {
      mcDebug_assert (decl_isStatementSequence (s));
      Indexing_PutIndice (s->stmtF.statements, (Indexing_HighIndice (s->stmtF.statements))+1, (void *) n);
    }
}

decl_node decl_makeReturn (void)
{
  decl_node n;

  n = newNode ((nodeT) return_);
  n->returnF.exp = NULL;
  return n;
}

unsigned int decl_isReturn (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == return_;
}

void decl_putReturn (decl_node n, decl_node e)
{
  mcDebug_assert (decl_isReturn (n));
  n->returnF.exp = e;
}

decl_node decl_makeWhile (void)
{
  decl_node n;

  n = newNode ((nodeT) while_);
  n->whileF.expr = NULL;
  n->whileF.statements = NULL;
  return n;
}

void decl_putWhile (decl_node n, decl_node e, decl_node s)
{
  mcDebug_assert (decl_isWhile (n));
  n->whileF.expr = e;
  n->whileF.statements = s;
}

unsigned int decl_isWhile (decl_node n)
{
  return n->kind == while_;
}

decl_node decl_makeAssignment (decl_node d, decl_node e)
{
  decl_node n;

  n = newNode ((nodeT) assignment);
  n->assignmentF.des = d;
  n->assignmentF.expr = e;
  return n;
}

void decl_putBegin (decl_node b, decl_node s)
{
  mcDebug_assert (((decl_isImp (b)) || (decl_isProcedure (b))) || (decl_isModule (b)));
  switch (b->kind)
    {
      case imp:
        b->impF.beginStatements = s;
        break;

      case module:
        b->moduleF.beginStatements = s;
        break;

      case procedure:
        b->procedureF.beginStatements = s;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

void decl_putFinally (decl_node b, decl_node s)
{
  mcDebug_assert (((decl_isImp (b)) || (decl_isProcedure (b))) || (decl_isModule (b)));
  switch (b->kind)
    {
      case imp:
        b->impF.finallyStatements = s;
        break;

      case module:
        b->moduleF.finallyStatements = s;
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

decl_node decl_makeExit (decl_node l, unsigned int n)
{
  decl_node e;

  mcDebug_assert (decl_isLoop (l));
  e = newNode ((nodeT) exit_);
  e->exitF.loop = l;
  l->loopF.labelno = n;
  return e;
}

unsigned int decl_isExit (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == exit_;
}

decl_node decl_makeLoop (void)
{
  decl_node l;

  l = newNode ((nodeT) loop);
  l->loopF.statements = NULL;
  l->loopF.labelno = 0;
  return l;
}

unsigned int decl_isLoop (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == loop;
}

void decl_putLoop (decl_node l, decl_node s)
{
  mcDebug_assert (decl_isLoop (l));
  l->loopF.statements = s;
}

decl_node decl_makeComment (char *a_, unsigned int _a_high)
{
  decl_node n;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  n = newNode ((nodeT) comment);
  n->commentF.content = DynamicStrings_InitString ((char *) a, _a_high);
  return n;
}

decl_node decl_makeIf (decl_node e, decl_node s)
{
  decl_node n;

  n = newNode ((nodeT) if_);
  n->ifF.expr = e;
  n->ifF.then = s;
  n->ifF.else_ = NULL;
  n->ifF.elsif = NULL;
  return n;
}

unsigned int decl_isIf (decl_node n)
{
  return n->kind == if_;
}

decl_node decl_makeElsif (decl_node i, decl_node e, decl_node s)
{
  decl_node n;

  n = newNode ((nodeT) elsif);
  n->elsifF.expr = e;
  n->elsifF.then = s;
  n->elsifF.elsif = NULL;
  n->elsifF.else_ = NULL;
  mcDebug_assert ((decl_isIf (i)) || (decl_isElsif (i)));
  if (decl_isIf (i))
    {
      i->ifF.elsif = n;
      mcDebug_assert (i->ifF.else_ == NULL);
    }
  else
    {
      i->elsifF.elsif = n;
      mcDebug_assert (i->elsifF.else_ == NULL);
    }
  return n;
}

unsigned int decl_isElsif (decl_node n)
{
  return n->kind == elsif;
}

void decl_putElse (decl_node i, decl_node s)
{
  mcDebug_assert ((decl_isIf (i)) || (decl_isElsif (i)));
  if (decl_isIf (i))
    {
      mcDebug_assert (i->ifF.elsif == NULL);
      mcDebug_assert (i->ifF.else_ == NULL);
      i->ifF.else_ = s;
    }
  else
    {
      mcDebug_assert (i->elsifF.elsif == NULL);
      mcDebug_assert (i->elsifF.else_ == NULL);
      i->elsifF.else_ = s;
    }
}

decl_node decl_makeFor (void)
{
  decl_node n;

  n = newNode ((nodeT) for_);
  n->forF.des = NULL;
  n->forF.start = NULL;
  n->forF.end = NULL;
  n->forF.increment = NULL;
  n->forF.statements = NULL;
  return n;
}

unsigned int decl_isFor (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == for_;
}

void decl_putFor (decl_node f, decl_node i, decl_node s, decl_node e, decl_node b, decl_node sq)
{
  mcDebug_assert (decl_isFor (f));
  f->forF.des = i;
  f->forF.start = s;
  f->forF.end = e;
  f->forF.increment = b;
  f->forF.statements = sq;
}

decl_node decl_makeRepeat (void)
{
  decl_node n;

  n = newNode ((nodeT) repeat);
  n->repeatF.expr = NULL;
  n->repeatF.statements = NULL;
  return n;
}

unsigned int decl_isRepeat (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == repeat;
}

void decl_putRepeat (decl_node n, decl_node s, decl_node e)
{
  n->repeatF.expr = e;
  n->repeatF.statements = s;
}

decl_node decl_makeCase (void)
{
  decl_node n;

  n = newNode ((nodeT) case_);
  n->caseF.expression = NULL;
  n->caseF.caseLabelList = Indexing_InitIndex (1);
  n->caseF.else_ = NULL;
  return n;
}

unsigned int decl_isCase (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == case_;
}

decl_node decl_putCaseExpression (decl_node n, decl_node e)
{
  mcDebug_assert (decl_isCase (n));
  n->caseF.expression = e;
  return n;
}

decl_node decl_putCaseElse (decl_node n, decl_node e)
{
  mcDebug_assert (decl_isCase (n));
  n->caseF.else_ = e;
  return n;
}

decl_node decl_putCaseStatement (decl_node n, decl_node l, decl_node s)
{
  mcDebug_assert (decl_isCase (n));
  mcDebug_assert (decl_isCaseList (l));
  Indexing_IncludeIndiceIntoIndex (n->caseF.caseLabelList, (void *) decl_makeCaseLabelList (l, s));
  return n;
}

decl_node decl_makeCaseLabelList (decl_node l, decl_node s)
{
  decl_node n;

  n = newNode ((nodeT) caselabellist);
  n->caselabellistF.caseList = l;
  n->caselabellistF.statements = s;
  return n;
}

unsigned int decl_isCaseLabelList (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == caselabellist;
}

decl_node decl_makeCaseList (void)
{
  decl_node n;

  n = newNode ((nodeT) caselist);
  n->caselistF.rangePairs = Indexing_InitIndex (1);
  return n;
}

unsigned int decl_isCaseList (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == caselist;
}

decl_node decl_putCaseRange (decl_node n, decl_node lo, decl_node hi)
{
  mcDebug_assert (decl_isCaseList (n));
  Indexing_IncludeIndiceIntoIndex (n->caselistF.rangePairs, (void *) decl_makeRange (lo, hi));
  return n;
}

decl_node decl_makeRange (decl_node lo, decl_node hi)
{
  decl_node n;

  n = newNode ((nodeT) range);
  n->rangeF.lo = lo;
  n->rangeF.hi = hi;
  return n;
}

unsigned int decl_isRange (decl_node n)
{
  mcDebug_assert (n != NULL);
  return n->kind == range;
}

decl_node decl_dupExpr (decl_node n)
{
  mcDebug_assert (n != NULL);
  switch (n->kind)
    {
      case explist:
        return dupExplist (n);
        break;

      case exit_:
      case return_:
      case stmtseq:
      case comment:
        M2RTS_HALT (0);
        break;

      case nil:
      case true:
      case false:
      case address:
      case loc:
      case byte:
      case word:
        break;

      case boolean:
      case proc:
      case char_:
      case integer:
      case cardinal:
      case longcard:
      case shortcard:
      case longint:
      case shortint:
      case real:
      case longreal:
      case shortreal:
      case bitset:
      case ztype:
      case rtype:
        return n;
        break;

      case type:
      case record:
      case varient:
      case var:
      case enumeration:
      case subrange:
      case subscript:
      case array:
      case string:
      case const_:
      case literal:
      case varparam:
      case param:
      case varargs:
      case optarg_:
      case pointer:
      case recordfield:
      case varientfield:
      case enumerationfield:
      case set:
      case proctype:
        return n;
        break;

      case procedure:
      case def:
      case imp:
      case module:
        return n;
        break;

      case loop:
      case while_:
      case for_:
      case repeat:
      case case_:
      case caselabellist:
      case caselist:
      case range:
      case if_:
      case elsif:
      case assignment:
        return n;
        break;

      case arrayref:
        return dupArrayref (n);
        break;

      case pointerref:
        return dupPointerref (n);
        break;

      case componentref:
        return dupComponentref (n);
        break;

      case and:
      case or:
      case equal:
      case notequal:
      case less:
      case greater:
      case greequal:
      case lessequal:
      case cast:
      case val:
      case plus:
      case sub:
      case div_:
      case mod:
      case mult:
      case divide:
      case in:
        return dupBinary (n);
        break;

      case constexp:
      case deref:
      case abs_:
      case chr:
      case high:
      case float_:
      case trunc:
      case ord:
      case throw:
      case not:
      case neg:
      case adr:
      case size:
      case tsize:
      case min:
      case max:
        return dupUnary (n);
        break;

      case identlist:
        return n;
        break;

      case vardecl:
        return n;
        break;

      case funccall:
        return dupFunccall (n);
        break;

      case setvalue:
        return dupSetValue (n);
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
}

void decl_setLangC (void)
{
  lang = ansiC;
}

void decl_setLangCP (void)
{
  lang = ansiCP;
}

void decl_setLangM2 (void)
{
  lang = pim4;
}

void decl_out (void)
{
  mcPretty_pretty p;

  openOutput ();
  p = mcPretty_initPretty ((mcPretty_writeProc) {(mcPretty_writeProc_t) write}, (mcPretty_writeLnProc) {(mcPretty_writeLnProc_t) writeln});
  switch (lang)
    {
      case ansiC:
        outC (p, decl_getMainModule ());
        break;

      case ansiCP:
        outCP (p, decl_getMainModule ());
        break;

      case pim4:
        outM2 (p, decl_getMainModule ());
        break;


      default:
        CaseException ("../../gcc-5.2.0/gcc/gm2/mc/decl.def", 20, 0);
    }
  closeOutput ();
}

void _M2_decl_init (int argc, char *argv[])
{
  init ();
}

void _M2_decl_finish (int argc, char *argv[])
{
}
