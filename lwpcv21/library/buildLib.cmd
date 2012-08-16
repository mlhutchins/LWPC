REM Build LWPC library by invoking the make file found in the
REM library subdirectory.
REM
REM Set compilation variables
set watcomDir=c:\win32app\watcom\
set finclude=%LWPCDir%Include;
set include=%watcomDir%h;%watcomDir%h\nt;%LWPCDir%Include;
set lib=%watcomDir%lib386;%watcomDir%lib386\nt;%LWPCDir%;
set wcc386=/fpi87 /sg /ot /zq /6
set wfc386=/fpi87 /sg /ot /quiet /6 /save
REM
wmake
