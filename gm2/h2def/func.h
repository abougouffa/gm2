extern char *func (int n);

extern int pthread_create (float(*__start_routine) (void *));  /* works */

typedef void *(*mydefinedproc) (void *); /* works */

extern  int (*arrayfunc[5])(int, char); /* works */




