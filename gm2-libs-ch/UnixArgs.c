/*
   implementation obviously in C !!
*/

int	UnixArgs_ArgC ;
char	**UnixArgs_ArgV ;

void _M2_UnixArgs_init(argc, argv)
int   argc ;
char  *argv[];
{
	UnixArgs_ArgC = argc ;
	UnixArgs_ArgV = argv ;
}
