Rem: REXP.CMD exercises the range dependent exponential
Rem: ionosphere option. The variation of the exponential
Rem: profile matches that used in BEARINGS.CMD with a
Rem: day-night terminator on the path at a bearing of 24
Rem: degrees.

Rem: Delete previous files
del  Output\rexp.*

Rem: Generate data
\LWPCv21\lwpm.exe rexp
