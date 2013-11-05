
%module MyStrLib

%apply (char *STRING, int LENGTH) { (char *data, int size) };

%{
extern int    MyStrLib_StrLen (char *STRING, int LENGTH);
%}

extern int    MyStrLib_StrLen (char *STRING, int LENGTH);


