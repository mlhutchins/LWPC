Rem: BEARINGS.CMD exercises the BEARINGS/+BEARINGS option
Rem: with a specific date and time chosen to put the
Rem: day-night terminator on the path

Rem: Delete previous files
del  Output\bearings.*

Rem: Generate data
\LWPCv21\lwpm.exe bearings
