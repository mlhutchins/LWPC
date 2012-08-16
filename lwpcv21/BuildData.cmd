cd data
gfortran -fdollar-ok unf_coas.for -o unf_coas
gfortran -fdollar-ok unf_cond.for -o unf_cond
gfortran -fdollar-ok unf_itsn.for -o unf_itsn
gfortran -fdollar-ok unf_ntia.for -o unf_ntia
./unf_cond
./unf_coas
./unf_itsn
./unf_ntia
rm unf_cond
rm unf_coas
rm unf_itsn
rm unf_ntia
cd ..