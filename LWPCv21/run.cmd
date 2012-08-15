cd library
ar r lwpc.a *.o
mv lwpc.a ..
cd ..
rm test1.mds
gfortran -w -fno-automatic lwpm.for lwpc.a -o LWPC
./LWPC test1