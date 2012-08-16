REM Build GRFDriver resource file for screen graphics programs.
REM
REM Set compilation variables
set watcomDir=c:\win32app\watcom
set include=%watcomDir%\h;%watcomDir%\h\nt;%LWPCDir%Include;
REM
wcc386 /fpi87 /sg /ot /zq GRFDriver.c
wrc -r  GRFDriver.rc

