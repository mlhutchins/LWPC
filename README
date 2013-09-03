The Long Wavelength Propagation Capability (LWPC) code was original programmed for Windows using the Watcom Fortran compilers. As these were not readily available for Unix based system the code was ported over using gfortran (http://gcc.gnu.org/fortran/) as the base compiler. The earliest version of the compiler used was 4.1.2 (need 4.3+) on a Linux machine as well as 4.4.3 on OS 10.6.

Prereqs:
gfortran, gcc, libgfortran.so.1

Fedora install:
sudo yum install gcc gcc-gfortran compat-libgfortran-41

Two simple steps are needed to get LWPC running:

1. Update the lwpcDAT.loc file to point towards the data folder.
2. Run BuildLWPC.cmd

To run LWPC type ./LWPC [input file name] with the input file following the conventions in the User_manual.pdf. Some features such as plotting may not work as they were not tested when LWPC was ported over from Windows.

Known Issues:

Run-time Errors:
"At line 20 of file lwpc_dat_loc.for (unit = 2, file = 'lwpcDAT.loc')
Fortran runtime error: End of file"

Solution: Add an extra line to the end of the lwpcDAT.loc.

"At line 95 of file decode_list_flt.for
Fortran runtime error: Bad value during floating point read"

Solution: Replace data/xmtr.lis file with original

There is a known issue involved the [input file name].mds file. If this file is present before running the program will hang, so delete it before each use. Similarly replace the /data/xmtr.lis file with the original (/save/xmtr.lis) if changing transmitter location but not transmitter name.

If there are errors in any of the data files (coast$d.dat) run BuildData.cmd

An example input file and output file are test1.inp and test1.log.

Tip for troubleshooting code:
The program seems to have various memory errors that can propagate through undetected. The best thing to do is to add variables to common blocks or sometimes just adding in a line will change things. Also print statements that output the variable under inspection can and will change that variable in the code.