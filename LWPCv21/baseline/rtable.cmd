Rem: RTABLE.CMD exercises the range dependent tabular
Rem: ionosphere option

Rem: Delete previous files
del  Output\rtbl.*

Rem: Generate data
\LWPCv21\lwpm.exe rtable
