REM Set compilation variables:
set watcomDir=c:\win32app\watcom\
set finclude=%LWPCDir%Include;
set include=%watcomDir%h;%watcomDir%h\nt;%LWPCDir%Include;
set lib=%watcomDir%lib386;%watcomDir%lib386\nt;%LWPCDir%;
set wfl386=/fpi87 /quiet /save /sg /6 /trace

REM Build the LWPC.
REM
REM First, set the LWPC directory path
call setLWPC
REM
REM Next, build the data files:
cd   Data
call BuildLWPCData
cd..
REM
REM Next, build the library:
cd   Library
call BuildLib
cd..
REM
REM Next, prepare the graphics driver:
cd   GRFDriver
call GRFDriver
cd..
REM
REM Finally, build all the programs:
REM
REM lwpm
wfl386 lwpm.for    lwpc.lib /fm=lwpm    /fe=lwpm
REM
REM scan
wfl386 scan.for    lwpc.lib /fm=scan    /fe=scan
REM
REM lwfPlot
REM wfl386 GRFDriver\GRFDriver.obj lwfPlot.for  lwpc.lib flib7.lib /fm=lwfPlot  /fe=lwfPlot
REM wrc    GRFDriver\GRFDriver.res lwfPlot.exe
REM
REM grdPlot
REM wfl386 GRFDriver\GRFDriver.obj grdPlot.for  lwpc.lib flib7.lib /fm=grdPlot  /fe=grdPlot
REM wrc    GRFDriver\GRFDriver.res grdPlot.exe
REM
REM prvwPlot
REM wfl386 GRFDriver\GRFDriver.obj prvwPlot.for lwpc.lib flib7.lib /fm=prvwPlot /fe=prvwPlot
REM wrc    GRFDriver\GRFDriver.res prvwPlot.exe
REM
REM Done
