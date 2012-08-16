cd library
gfortran -c -w -fdollar-ok -fno-automatic -fno-range-check *.for
ar r lwpc.a *.o
mv lwpc.a ..
cd ..
cp save/xmtr.lis data/
rm *.mds
rm *.lwf
gfortran -w -fno-automatic lwpm.for lwpc.a -o LWPC
chmod +x LWPC
