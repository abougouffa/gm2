cd ../../../
# break outdeclarator
# break out_function
# run -c p2crc -Igm2-libs-boot -s gm2-libs-boot/FormatStrings.def gm2-libs-boot/FormatStrings.md -o -
# run -c p2crc -Igm2-libs-boot -s gm2-libs-boot/Strings.def gm2-libs-boot/Strings.md -o -
# break makesettype
# break stop3
# break p_setfactor
# run -c p2crc -Igm2-libs-boot -s gm2-compiler-boot/P1SyntaxCheck.def gm2-compiler-boot/P1SyntaxCheck.md -o -
# run -c p2crc -Igm2-libs-boot -s gm2-compiler-boot/P2Build.def gm2-auto/P2Build.md -o -
# break fopen
# break lex.c:3036
# run -c p2crc -Igm2-libs-boot -s gm2-compiler-boot/SymbolKey.def gm2-compiler-boot/SymbolKey.mod -o -
# break stop3
# run -c p2crc gm2-libs-boot/IO.def -o gm2-libs-boot/IO.h
# run -c p2crc -Igm2-libs-boot gm2-compiler-boot/SymbolKey.def -o gm2-compiler-boot/SymbolKey.h
# break p_import
# break stop2
# break p_import
# break stop
# run -c p2crc -Igm2-libs-boot -Igm2-libiberty -M0 -q -s gm2-compiler/M2Preprocess.def gm2-compiler/M2Preprocess.mod -o -
run -c p2crc -Igm2-libs-boot -Igm2-libiberty -M0 -q gm2-libiberty/choosetemp.def -h gm2-libiberty/choosetemp.h -o -
