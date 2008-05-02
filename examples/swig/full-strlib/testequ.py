import StrLib

print 'we are going to call StrLib.StrLib_StrLess("abcd", "pqr") and we expect 1 in return'
print StrLib.StrLib_StrLess("abcd", "pqr")
if StrLib.StrLib_StrLess("abcd", "pqr")==1:
    print "passed"
else:
    print "failed"

print 'we are going to call StrLib.StrLib_StrLess("pqr", "abcd") and we expect 0 in return'
print StrLib.StrLib_StrLess("pqr", "abcd")
if StrLib.StrLib_StrLess("pqr", "abcd")==0:
    print "passed"
else:
    print "failed"

print 'we are going to call StrLib.StrLib_IsSubString("abcdefghijk", "fghi") and we expect 1 in return'
print StrLib.StrLib_IsSubString("abcdefghijk", "fghi")
if StrLib.StrLib_IsSubString("abcdefghijk", "fghi")==1:
    print "passed"
else:
    print "failed"
