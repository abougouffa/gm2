/*
 *
 */

%module fBuffer

%cstring_output_allocate_size(char **start, int *used);

%{
extern "C" void fBuffer_get (char **start, int *used);
%}

extern "C" void fBuffer_get (char **start, int *used);
