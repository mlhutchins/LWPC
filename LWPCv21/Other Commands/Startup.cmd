REM Unpack the LWPC distribution files.
REM
unzip -o lwpcv21
unzip -o lwpcData
unzip -o baseline
REM
Rem Copy the LWPC data pointer file to C:
copy lwpcDAT.loc C:\
REM Done
