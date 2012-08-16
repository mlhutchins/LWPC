/* The following structure defines a FORTRAN common block called
lwpc_cfg.  These are also defined in the FORTRAN common block file
lwpc_cfg.cmn */

#pragma aux lwpc_cfg "^";

extern struct lwpc_cfg
{
     char lwpcDAT_loc[245];
     char lwpcMDS_loc[245];
     char lwpcLWF_loc[245];
     char lwpcGRD_loc[245];
     char lwpcPRF_loc[245];
     char lwpcNDX_loc[245];
     char lwpcLOG_loc[245];
     char lwpcINP_loc[245];
     char lwpcXtr_loc[245];
     char lwpcLWT_loc[245];
     char lwpcNOI_loc[245];
} lwpc_cfg;

