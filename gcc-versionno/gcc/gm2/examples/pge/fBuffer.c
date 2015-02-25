/*
 *  fBuffer - wrap the frame buffer contents into a binary string.
 */

extern void deviceIf_getFrameBuffer (void **start, int *length, int *used);


void fBuffer_get (void **start, unsigned int *used)
{
  int length;
  deviceIf_getFrameBuffer (start, &length, used);
}
