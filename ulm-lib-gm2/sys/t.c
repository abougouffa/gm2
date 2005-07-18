extern _M2_M2RTS_init(int argc, char *argv[]);
extern _M2_ASCII_init(int argc, char *argv[]);
extern _M2_Conversions_init(int argc, char *argv[]);
extern _M2_Strings_init(int argc, char *argv[]);
extern _M2_Errno_init(int argc, char *argv[]);
extern _M2_Sys_init(int argc, char *argv[]);
extern _M2_SysWrite_init(int argc, char *argv[]);
extern _M2_test_init(int argc, char *argv[]);
extern M2RTS_Terminate(void);
int 
main (argc, argv)
int   argc ;
char  *argv[];
{
    _M2_M2RTS_init(argc, argv);
    _M2_ASCII_init(argc, argv);
    _M2_Conversions_init(argc, argv);
    _M2_Strings_init(argc, argv);
    _M2_Errno_init(argc, argv);
    _M2_Sys_init(argc, argv);
    _M2_SysWrite_init(argc, argv);
    _M2_test_init(argc, argv);
  M2RTS_Terminate();
   M2RTS_Terminate();
   exit(0);
   return(0);
}
