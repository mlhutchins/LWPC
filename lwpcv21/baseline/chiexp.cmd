Rem: CHIEXP.CMD exercises the chi dependent exponential
Rem: ionosphere option. The variation of the exponential
Rem: profile reduces the length of the terminator from
Rem: 90-99 to 96-99.

Rem: Delete previous files
del  Output\chiexp.*

Rem: Generate data
\LWPCv21\lwpm.exe chiexp
