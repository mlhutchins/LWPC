REM Clean up LWPCv21 directory
REM
cd   Baseline
del/q/s *.*
cd..
cd   Data
del/q/s *.*
cd..
cd   GRFDriver
del/q/s *.*
cd..
cd   Include
del/q/s *.*
cd..
cd   Library
del/q/s *.*
cd..
cd   Output
del/q/s *.*
cd..
cd   Profile
del/q/s *.*
cd..
del/q/s setLWPC.cmd
del/q/s BuildLWPC.cmd
del/q/s runSamples.cmd
del/q/s lwpcDAT.loc
del/q/s bearings.*
del/q/s chiexp.*
del/q/s gcpath.*
del/q/s grdPlot.*
del/q/s htable.*
del/q/s jammer.*
del/q/s lwflds.*
del/q/s lwfPlot.*
del/q/s lwpm.*
del/q/s prvwPlot.*
del/q/s rexp.*
del/q/s rtable.*
del/q/s scan.*
del/q/s *.his
del/q/s lwpc.lib
REM Done
