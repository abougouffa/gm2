#include <p2c/p2c.h>

/*
   strtime - returns the address of a string which describes the
             local time.
*/

char *wrapc_strtime (void)
{
#if defined(HAVE_CTIME)
  void *clock = time((void *)0) ;
  char *string = ctime(&clock);

  string[24] = (char) 0;

  return string;
#else
  return "";
#endif
}


int wrapc_filesize (int f)
{
  struct stat s;

  if (fstat(f, (struct stat *) &s) == 0)
    return s.st_size;
  else
    return -1;
}


/*
 *   filemtime - returns the mtime of a file, f.
 */

int wrapc_filemtime (int f)
{
  struct stat s;

  if (fstat(f, (struct stat *) &s) == 0)
    return s.st_mtime;
  else
    return -1;
}


/*
   getrand - returns a random number between 0..n-1
*/

int wrapc_getrand (int n)
{
  return rand() % n;
}

#if defined(HAVE_PWD_H)
#include <pwd.h>

char *wrapc_getusername (void)
{
  return getpwuid(getuid()) -> pw_gecos;
}

/*
   getnameuidgid - fills in the, uid, and, gid, which represents
                   user, name.
*/

void wrapc_getnameuidgid (char *name, int *uid, int *gid)
{
  struct passwd *p=getpwnam(name);

  if (p == NULL) {
    *uid = -1;
    *gid = -1;
  } else {
    *uid = p->pw_uid;
    *gid = p->pw_gid;
  }
}
#else
char *wrapc_getusername (void)
{
  return "unknown";
}

void wrapc_getnameuidgid (char *name, int *uid, int *gid)
{
  *uid = -1;
  *gid = -1;
}
#endif


/*
   init - init function for the module
*/

void _M2_wrapc_init() {}