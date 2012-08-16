REM Build LWPC data files
REM
wfl386 unf_coas
call   unf_coas
del    unf_coas.exe
del    unf_coas.obj
wfl386 unf_cond
call   unf_cond
del    unf_cond.exe
del    unf_cond.obj
wfl386 unf_itsn
call   unf_itsn
del    unf_itsn.exe
del    unf_itsn.obj
wfl386 unf_ntia
call   unf_ntia
del    unf_ntia.exe
del    unf_ntia.obj

