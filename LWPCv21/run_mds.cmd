cd library
ar r lwpc.a *.o
mv lwpc.a ..
cd ..
gfortran -w -fno-automatic lwpm.for lwpc.a -o LWPC
cp save/xmtr.lis data/
rm test1.mds
./LWPC test1
vi test1.log
