#pragma aux grd_data;

struct grd_data {
   char archive    [  9];
   char mds_status [121];
   char lwf_status [121];
   char grd_status [121];
   char program_id [  9];
   char case_id    [ 81];
   char profile_id [ 41];
   char xmtr_id    [ 21];
   char path_id    [ 21];
   char area_id    [ 21];

   float  tx_freq, tx_lat, tx_lon;
   float  oplat1, oplon1, oplat2, oplon2;
   int    nrpath;
   float  bearing[MAX_PATH], rhomax[MAX_PATH], rxlat[MAX_PATH], rxlon[MAX_PATH];
   float  xlat1, xlon1, xlat2, xlon2;
   int    nrlon, nrlat, nrprm, nrcmp, nrlwf;
   float  parameter_list[MAX_PARAMS];

   float  amp_grd[MAX_XLATS][MAX_XLONS];
   float  sig_grd[MAX_XLATS][MAX_XLONS];

} grd_data;



