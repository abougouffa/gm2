
%module StrLib

%apply (char *STRING, int LENGTH) { (char *data, int size) };

%{
extern int    StrLib_StrLen (char *STRING, int LENGTH);
%}

extern int    StrLib_StrLen (char *STRING, int LENGTH);


