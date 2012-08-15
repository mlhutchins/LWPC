Rem: LWPM.CMD exercises the OP-AREA option

Rem: This run will take some time but it is necessary to set
Rem: up the test of PLOT_GRD. Use SCAN and PLOT_LWF to verify
Rem: the contents of the mode sum file named LWPM.LWF and the
Rem: grid file named LWPM_MEDITERRANEAN.GRD before using
Rem: GRD_PLOT.

Rem: Delete previous files
del  Output\lwpm.*

Rem: Generate data
\LWPCv21\lwpm.exe lwpm
