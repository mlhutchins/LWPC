Rem: JAMMER.CMD creates a jammer data set for OMEGA North Dakota
Rem: using OMEGA Norway.

Rem: Delete previous files
del  Output\jammer.*

Rem: Generate data
\LWPCv21\lwpm.exe jammer
