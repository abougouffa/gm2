extern int T29;

static void *Queue;
static unsigned int Out;

#if 0
extern void PutItemIntoList (void *, unsigned int);
#endif
extern unsigned int GetItemFromList (void *, unsigned int);
extern void InitList (void **);

#if 0
/*
   PutIntoFifoQueue - places a CARDINAL number, c, into a fifo queue.
*/

void PutIntoFifoQueue (unsigned int c)
{
  PutItemIntoList(Queue, c);
}
#endif

/*
   GetFromFifoQueue - retrieves a CARDINAL number, c, from a fifo queue.
*/

void GetFromFifoQueue (unsigned int *c)
{
  unsigned int T29 = Out;
  T29++;
  Out = T29;
  *c = GetItemFromList(Queue, Out);
}


/*
   InitFifoQueue - initialize the fifo queue.
*/

static void InitFifoQueue (void)
{
  InitList(&Queue);
  Out = 0;
}

void _M2_FifoQueue_init ()
{
  InitFifoQueue();
}
