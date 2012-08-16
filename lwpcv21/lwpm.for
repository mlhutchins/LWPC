      PROGRAM LWPM

c Long Wave Propagation Model

c Generates mode parameters along propagation paths. Uses the mode data
c to calculate signal strength along the same paths. If required, also
c generates a GRD file for specified operating areas.

c CONTROL STRINGS:

c FILE-MDS    directory-location-of-MDS-data-files
c FILE-LWF    directory-location-of-LWF-data-files
c FILE-GRD    directory-location-of-GRD-data-files
c FILE-PRF    directory-location-of-PRF-data-files
c FILE-NDX    directory-location-of-NDX-data-files
c CASE-ID     CASE-ID
c TX-NTR      TX-DATA-FILE
c JX-NJR      JX-DATA-FILE
c TX-DATA     TX-ID freq tx-lat tx-lon power inclination heading tx-alt
c TX-DATA     TX-ID TX-SPECIFICATION-FILE
c JX-DATA     JX-ID freq jx-lat jx-lon power inclination heading jx-alt
c JX-DATA     JX-ID JX-SPECIFICATION-FILE
c RX-DATA     VERTICAL   rx-alt
c RX-DATA     HORIZONTAL rx-alt
c IONOSPHERE  HOMOGENEOUS TABLE FILE-NAME
c IONOSPHERE  HOMOGENEOUS EXPONENTIAL beta hprime
c IONOSPHERE  LWPM DAY
c IONOSPHERE  LWPM NIGHT
c IONOSPHERE  LWPM MONTH/day/year hour:minute
c IONOSPHERE  RANGE TABLE       FILE-NAME
c IONOSPHERE  RANGE EXPONENTIAL FILE-NAME
c IONOSPHERE  CHI   TABLE       FILE-NAME
c IONOSPHERE  CHI   EXPONENTIAL FILE-NAME
c IONOSPHERE  GRID  TABLE       FILE-NAME
c BEARINGS    bearing-1 bearing-2 ...
c +BEARINGS   bearing-n bearing-n ...
c RECEIVERS   rx-lat-1 rx-lon-1 rx-lat-2 rx-lon-2 ...
c +RECEIVERS  rx-lat-n rx-lon-n rx-lat-m rx-lon-m ...
c RANGE-MAX   maximum-range-for-BEARINGS-and-RECEIVERS
c OP-AREA     AREA-ID op-lat1 op-lon1 op-lat2 op-lon2
c OP-AREA     AREA-ID OP-AREA-SPECIFICATION-FILE
c A-NOISE     N_MODEL MONTH/day/year hour:minute band-width
c LWF-VS-DIST lwf-dist-max lwf-dist-inc
c PRINT-LWF   print-lwf
c PRINT-MDS   print-mds
c PRINT-SWG   print-swg
c GCPATH
c LWFLDS
c OPA-GRID
c PRESEG
c START
c QUIT
c MC-OPTIONS  MC-STEP mc-test wf-iterate
c PRINT-MC    print_mc
c PRINT-WF    print_wf

c***********************************************************************

c CASE-ID           Provides for user identification of the data

c    INPUT PARAMETERS:

c    CASE-ID        identification string [80 characters]


c TX-NTR            Provides a root file name from which output files
c                   are constructed.

c    INPUT PARAMETERS:

c    NTR            transmitter number provided for convenience in
c                   identifying transmitters for complicated cases.
c                   If it is not required, then the control string
c                   may be shortened to "TX".

c    TX-DATA-FILE   character string which will be used to define file
c                   names [120 characters]. It may contain a directory
c                   name. This and other programs in the LWPC generate
c                   files with different extensions using this name as
c                   the root. For example, the output from the mode
c                   sum program is of the form: TX-DATA-FILE.LWF


c JX-NJR            Provides a root file name from which output files
c                   are constructed. Equivalent in effect to "TX-NTR".

c    INPUT PARAMETERS:

c    NJR            jammer number provided for convenience in
c                   identifying jammers for complicated cases.
c                   If it is not required, then the control string
c                   may be shortened to "JX".

c    JX-DATA-FILE   character string which will be used to define file
c                   names [120 characters]. It may contain a directory
c                   name. This and other programs in the LWPC generate
c                   files with different extensions using this name as
c                   the root. For example, the output from the mode
c                   sum program is of the form: JX-DATA-FILE.LWF


c TX-DATA           Defines the parameters of the transmitter.

c    INPUT PARAMETERS:

c    TX-ID          character string which identifies the transmitter
c                   [20 characters]
c    FREQ           frequency
c    TX-LAT         latitude
c    TX-LON         longitude
c    POWER          power
c    INCLINATION    inclination of the antenna
c    HEADING        heading of the antenna
c    TX-ALT         altitude of the antenna

c    TX-SPECIFICATION-FILE
c                   name of a file which contains records containing
c                   the above list of parameters for the transmitter;
c                   the file is searched for a match with TX-ID and
c                   the corresponding parameters are returned.


c JX-DATA           Defines the parameters of the jammer. Equivalent in
c                   effect to "TX-DATA".

c    INPUT PARAMETERS:

c    JX-ID          character string which identifies the jammer
c                   [20 characters]
c    FREQ           frequency
c    JX-LAT         latitude
c    JX-LON         longitude
c    POWER          power
c    INCLINATION    inclination of the antenna
c    HEADING        heading of the antenna
c    JX-ALT         altitude of the antenna

c    JX-SPECIFICATION-FILE
c                   name of a file which contains records containing
c                   the above list of parameters for the jammer.
c                   the file is searched for a match with JX-ID and
c                   the corresponding parameters are returned.


c RX-DATA           Defines the parameters of the received fields.

c    INPUT PARAMETERS:

c    VERTICAL       indicates that only the vertical fields (Ez) are
c                   to be computed; this string may be shortened.

c    HORIZONTAL     indicates that both the vertical and horizontal
c                   fields (Ez and Ey) are to be computed; this string
c                   may be shortened.

c    RX-ALT         altitude of the antenna


c IONOSPHERE        Defines the ionospheric profile(s).

c    INPUT PARAMETERS:

c    HOMOGENEOUS TABLE FILE-NAME
c                   indicates that a homogeneous ionosphere is to be
c                   used over the whole path; the profile parameters
c                   are defined by a table found in the file named
c                   FILE-NAME.PRF

c    HOMOGENEOUS EXPONENTIAL beta hprime
c                   indicates that a homogeneous exponential
c                   ionosphere is to be used over the whole path;
c                   the profile parameters are a slope BETA and a
c                   reference height HPRIME.

c    LWPM DAY       indicates that all day-time conditions are to be
c                   used over the whole path; the parameters of the
c                   ionosphere are to be set by the program.

c    LWPM NIGHT     indicates that all night-time conditions are to be
c                   used over the whole path; the parameters of the
c                   ionosphere are to be set by the program.

c    LWPM MONTH/day/year hour:minute
c                   indicates that a specific date and time is to be
c                   used; the parameters of the ionospheric profiles
c                   are to be set by the program; the parameters of
c                   the date and time is as follows:
c                      MONTH  name of the month
c                      day    day of the month
c                      year   year
c                      hour   hour of the day
c                      minute minute of the hour
c                   The day may be omitted by using two "/"; the year
c                   may be omitted by dropping "/year"; the day and
c                   year may be omitted by dropping "/day/year"; the
c                   year may be entered modulo (100). The minutes may
c                   be omitted by dropping ":minute".

c    RANGE TABLE FILE-NAME
c                   indicates that the ionosphere varies along the
c                   path; the profile parameters are defined using
c                   files with the root name defined by FILE-NAME;
c                   the index file, named FILE-NAME.NDX, contains
c                   a list of ranges and associated profile indices;
c                   the ionosphere for each index "nnn" is defined in
c                   a table found in the file FILE-NAMEnnn.PRF

c    RANGE EXPONENTIAL FILE-NAME
c                   indicates that the ionosphere varies along the
c                   path; the profile parameters are defined by a
c                   file, named FILE-NAME.NDX, which contains a list
c                   of ranges and associated BETAs and HPRIMEs.

c    CHI TABLE FILE-NAME
c                   indicates that the ionosphere varies with solar
c                   zenith angle (chi); the profile parameters are
c                   defined using files with the root name defined by
c                   FILE-NAME; the index file, named FILE-NAME.NDX,
c                   contains a list of ranges and associated profile
c                   indices; the convention to be used is actually to
c                   use the complement of the zenith angle such that
c                   0<CHI<90 is midnight to noon and 90<CHI<180 is
c                   noon to midnight; the ionosphere for each index
c                   "nnn" is defined in a table found in the file
c                   FILE-NAMEnnn.PRF

c    CHI EXPONENTIAL FILE-NAME
c                   indicates that the ionosphere varies with solar
c                   zenith angle (chi); the convention to be used is
c                   actually to use the complement of the zenith angle
c                   such that 0<CHI<90 is midnight to noon and
c                   90<CHI<180 is noon to midnight; the profile
c                   parameters are defined by a file, named
c                   FILE-NAME.NDX, which contains a list of solar
c                   zenith angles (chi) and associated BETAs and
c                   HPRIMEs.

c    GRID TABLE FILE-NAME
c                   indicates that the ionosphere varies with
c                   position; the profile parameters are defined using
c                   files with the root name defined by FILE-NAME;
c                   the index file, named FILE-NAME.NDX, contains a
c                   grid of positions and associated profile indices;
c                   the ionosphere for each index "nnn" is defined in
c                   a table found in the file FILE-NAMEnnn.PRF


c BEARINGS          Defines a list of bearing angles used to define
c                   paths

c    INPUT PARAMETERS:

c    bearing        geographic bearing angle of a path [120 values];
c                   a list is continued by the control string
c                   "+BEARINGS".


c RECEIVERS         Defines a list of receiver positions used to define
c                   paths

c    INPUT PARAMETERS:

c    rx-lat rx-lon  geographic coordinates of a receiver [60 values];
c                   a list is continued by the control string
c                   "+RECEIVERS".


c RANGE-MAX         Defines the maximum range of paths when they are
c                   defined using "BEARINGS" and "RECEIVERS".

c    INPUT PARAMETERS:

c    maximum-range  maximum range


c OP-AREA           Defines an operating area used to define paths

c    INPUT PARAMETERS:

c    AREA-ID        character string which identifies the op area
c                   [20 characters]
c    op-lat1        bottom latitude  of the op area
c    op-lon1        left   longitude of the op area
c    op-lat2        top    latitude  of the op area
c    op-lon2        right  longitude of the op area

c    AREA-SPECIFICATION-FILE
c                   name of a file which contains records containing
c                   the above list of parameters for the op area.
c                   the file is searched for a match with AREA-ID and
c                   the corresponding parameters are returned.


c A-NOISE           Defines parameters for computation of atmospheric
c                   noise

c    INPUT PARAMETERS:

c    N-MODEL        noise model name [4 characters: ITSN, NTIA]
c    MONTH          name of the month
c    day            day of the month
c    year           year
c    NOTE:          The day may be omitted by using two "/"; the year
c                   may be omitted by dropping "/year"; the day and
c                   year may be omitted by dropping "/day/year"; the
c                   year may be entered modulo (100).

c    hour           hour of the day
c    minute         minute of the hour
c    NOTE:          The minutes may be omitted by dropping ":minute".

c    band-width     band width in Hz


c LWF-VS-DIST       Defines the range of distance over which the mode
c                   sum is computed

c    INPUT PARAMETERS:

c    lwf-dist-max   distance at which to end   the mode sum
c    lwf-dist-inc   distance increment


c PRINT-LWF         Indicates levels of print out for the mode summation

c    INPUT PARAMETERS:

c    print-lwf      =0: minimum print from LWF file
c                    1: adds fields vs. distance


c PRINT-MDS         Indicates levels of print out for the mode
c                   parameters

c    INPUT PARAMETERS:

c    print-mds      =0: minimum print from MDS file
c                    1: adds attenuation rate, etc.
c                    2: adds conversion coefficients
c                    3: adds slab integrals


c PRINT-SWG         Indicates levels of print out for the calculations
c                   along the propagation paths

c    INPUT PARAMETERS:

c    print-swg      =0: none
c                    1: path parameters and mode lists:   LWP_DRIVER
c                    2: mode parameter outputs:           SW_WVGD
c                    3: extrapolation parameter outputs:  SW_EXTRAP


c GCPATH            Indicates that the path segmentation is to be
c                   printed; no mode parameters are be generated.


c LWFLDS            Re-compute the mode summation; presumes that the
c                   altitudes or orientation of the transmitter/jammer
c                   or receiver has been changed.


c OPA-GRID          Re-compute the grid file; presumes a new mode sum or
c                   a different operating area.


c PRESEG            Use precomputed segmentation data; the list of
c                   segments follows the control string; see LWP_INPUT
c                   for details.


c START             Indicates that all inputs are complete and
c                   processing of the paths can begin.


c QUIT              Terminates the run.


c MC-OPTIONS        Controls which mode conversion calculation is used.

c    INPUT PARAMETERS:

c    MC-STEP        =FULL-WAVE indicates that the full wave
c                   calculations are to be performed; this means
c                   integration of the wave fields through the
c                   ionosphere; if the flag ITERATE is true, then
c                   the eigen angles are iterated until the mode
c                   equation is satisfied.

c                   =MIXED indicates that a mix of full wave and
c                   approximate calculations is to be performed; the
c                   full wave calculations are done when the h' on
c                   either side of a slab boundary is greater than
c                   the value defined by MC-TEST.

c                   =APPROXIMATE indicates that only the approximate
c                   form of the calculations is to be used.

c    mc-test        the value of h' used to determine when full wave
c                   calculations are to be performed.

c    wf-iterate     =TRUE tells the WaveFields routine to iterate the
c                   input eigen angles to ensure that the mode
c                   equation is satisfied.


c PRINT-MC          Indicates levels of print from ModeConversion

c    INPUT PARAMETERS:

c    print-mc       =0: minimum print
c                    1: adds conversion coefficients
c                    2: adds integrals


c PRINT-WF          Indicates levels of print from WaveFields

c    INPUT PARAMETERS:

c    print-wf       =0: minimum print
c                    1: adds iterations
c                    2: adds fields vs. height

c***********************************************************************

c The flow of this program is determined by the use of control strings
c and their associated data. Control strings begin in column 1; control
c strings beginning with a blank in this column are treated as comments;
c a control string may be shortened so long as it is unique. More than
c one space may separate the control string from its associated data and
c any number of spaces may appear between data items. In the list of
c control strings, quantities which appear in upper case are character
c data.

c***********************************************************************

c Common parameters, units of measure, conventions:

c Quantity:       Units:
c    frequency      kHz
c    power          kW
c    altitude       km
c    distance       km
c    latitude       degrees North
c    longitude      degrees West
c    inclination    degrees from vertical
c    heading        degrees East of North (geographic)
c    bearing        degrees East of North (geographic)

c Conventions:
c    Southern latitudes are input as negative values
c    Eastern longitudes are input as negative values
c    Dates are input as MONTH/day/year
c    Times are input as hour:minute
c    Antennae are modeled as short dipoles

c***********************************************************************

c LU:    Ext:   File:
c    2              xmtr, map-area, op-area
c    3     INP      Input file
c    4     LOG      Log file

c    11    NDX      Ionospheric profile index file
c    12    PRF      Ionospheric profile
c    13    MDS      Mode parameters
c    14    LWF      Mode sum vs. distance
c    15    GRD      Grid data

c    16             ITSN noise coefficients
c    17             NTIA noise coefficients

c    20             Temporary storage of wf data: direct azimuth
c    21             Temporary storage of wf data: adjoint
c    22             Temporary storage of wf data: previous

c    95             Corrected geomagnetic coordinates
c    96             Land mass map
c    97             Coastal outline map
c    98             Ground conductivity map

c    99             Scratch

c***********************************************************************

c Preferred plotter set up:
c   PEN:   COLOR:   PLOT_GRD:      COND_MAP:    PREVIEW:
c    1     black    labels                      border
c    2     green    solid          1e-2/3e-2
c    3     blue     long dash      1e-3/3e-3
c    4     purple   medium dash      3e-5       op_area
c    5     red      short dash       1e-5       paths
c    6     black    dots             3e-4
c    7     yellow   land overlay     1e-4       land
c    8     broad    coast                       coast

c***********************************************************************

c Change history:
c     23 Oct 90     Deleted run-time definition of strings in argument
c                   lists of subroutines (using // operator).

c     05 Feb 92     Initialize collision frequency to the "standard"
c                   values used by Wait's exponential profiles.

c     10 Feb 92     Modified /LWPC_IN/ to store HOFWR,TOPHT,BOTHT and
c                   PINDEX.

c     22 May 92     T.A.Hepner
c                   Changed file access mode from "READONLY" to
c                   "MODE='READ' " to work under MS FORTRAN 5.x.

c                   Added a new common block called LWPC_CFG, which is
c                   used to store configuration information, such as
c                   the location of the lwpc_dat data files.

c                   Added the following control strings which are used
c                   to specify the location of the various data files
c                   used or created by LWPM:
c                      FILE-DAT, FILE-MDS, FILE-LWF, FILE-GRD,
c                      FILE-PRF, FILE-NDX

c     01 Nov 92     Added final configuration of the ModeConversion
c                   option.

c     20 Jan 93     Added frequency dependent limits for RANGER and
c                   RANGEI.

c     10 Nov 93     Added solar zenith angle dependent ionospheres.

c     11 Nov 93     Disabled PREVIEW option for compatibility with
c                   current OS/2 graphics. This option now available
c                   in the program named PrvwPlot. The control strings
c                   are still recognized, just not implemented.

c                   Added command line input to replace redirection.

c     04 Jan 94     In call to LWP_EOF_MDS, replaced argument EMPTY
c                   with NUMBER_OF_PATHS; removed status file.

c     06 Jan 94     Added solar zenith angle to GCPATH output.

c     17 Jan 94     Dropped FAT compatibility in GRD file names.

c     21 Jan 94     Changed file naming convention for noise grids to
c                   be the signal grid name with extension equal to the
c                   first 3 characters of the noise model name; dropped
c                   lwpcGRD_loc from call to OPA_NOISE_GRID.

c     29 Jan 94     Added ASCII/BINARY option for grid files.

c     06 Apr 94     Added 3 standard deviations:
c                   (1: day; 2: night; and 3: transition).

c     30 Jun 94     Dropped END_FILE_MDS from call to LWP_DRIVER;
c                   dropped PRFL_ID, CXCLT and SXCLT from call to
c                   OPA_NOISE_GRID.

c     21 Sep 95     Fixed up handling of output:
c                   Added extra variable to allow multiple cases in a
c                   single run;
c                   Check LWF file to ensure that it is
c                   compatible with generation of a grid file;
c                   If the input contains LWFIELDS or OPA-GRID,
c                   check the MDS file, if it doesn't exist or is
c                   incomplete, then create or complete it;
c                   Miscellaneous cosmetic changes.

c     26 Sep 95     Changed all PRINT statements to WRITE(6) ones.

c     25 Oct 95     Changed to use lwpc_LUN.cmn and lwpc_LUN.ini to set
c                   logical units.

c     18 Dec 95     Added PRINT-MC and PRINT-WF to argument list.

c     30 Jan 96     Added V sub d grid array.

c     12 Dec 96     Modified to use new generic flush unit routine.

c     01 Mar 97     Added option for presegmentation.

c     15 Jan 98     Dropped PLT_UNIT; use PLT_DEVICE to store file name
c                   when output to a file.

c     05 Feb 98     Removed control string FILE-DAT.

c     07 Sep 98     Changed default values: FTOL=.1, MAXITR=5

c*******************!***************************************************
      parameter    (mxpath=120,mxprm1=21,
     &              mxeigen=50,mxprm2=5,mxsgmnt=201,
     &              mxpts=1002,mxlat=73,mxlon=145,
     &              mxlwf=21)

c     LWPC parameters
      include      'lwpc_cfg.cmn'
      include      'lwpc_lun.cmn'

      include      'lwpc_lun.ini'

      character*  4 map_type(2),map_prjctn,n_model
      character*  8 archive,prgm_id,prgmidx,
     &              mds_format,lwf_format,grd_format
      character* 12 plt_orientation
      character* 20 xmtr_id,path_id,area_id,map_id,
     &              xmtridx,pathidx
      character* 40 prfl_id
      character* 80 case_id
      character* 96 plt_device
      character*120 file_id,
     &              root_file,
     &              file_name,prfl_file

      logical       more_data,exists,
     &              do_mds,do_lwf,do_opa,
     &              begin_file_mds,end_file_mds,
     &              begin_file_lwf,
     &              gcpath,lwflds,opa_grid,preseg,
     &              dist_avg

      integer       str_length,
     &              bflag,pflag,pindex,year,day,UT,
     &              autos,rprnt,xprnt,rpoly,
     &              print_swg,print_mds,print_lwf,print_mc,print_wf,
     &              print_grd,
     &              mc_step,arb_azim,lu_wfd,lu_wfa,lu_wfp,
     &              c_year,c_month,c_day,c_hrs,c_mins,c_secs,c_hsecs

      real          lat,lon,lub,incl,
     &              map_lat(2),map_lon(2),map_size(2),
     &              lwf_dst_min,lwf_dst_max,lwf_dst_inc

      real     *  8 max_wf_ht,mc_test,
     &              dtheta2,tol2,thtinc2,alpha2,h2,prec2

      complex       eigen,tp,capt,fofr,param2

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearing,range,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex
     &      /lwpc_mf/
     &              ranger(2),rangei(2),atnmax,lub,h,eigen(mxeigen),
     &              nreigen
     &      /lwpc_pr/
     &              hten(51),algen(51,3),nrhten,ihten,
     &              htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &              charge(3),ratiom(3),nrspec,lu_prf,
     &              select_hgts(2),hgts(3)

     &      /mc_inpt/
     &              prfl_file,max_wf_ht,mc_test,
     &              lu_wfd,lu_wfa,lu_wfp,mc_step,arb_azim

     &      /mf_area/
     &              gmax,bxrtio,tmesh,autos
     &      /mf_flag/
     &              iexact,iovflo,kexact,lowg,nodivd,noevct,nofinl,
     &              nointg,nomesh,notlin
     &      /mf_hgts/
     &              rtol,xintrp,htntrp,d
     &      /mf_prnt/
     &              lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

     &      /mf_sw_1/
     &              dst(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt
     &      /mf_sw_4/
     &              chi(mxsgmnt)

     &      /mf_sw_3/
     &              op_lat(2),op_lon(2),brsltn(2),
     &              path_bearing(mxpath),path_range(mxpath),
     &              rxlat(mxpath),rxlon(mxpath),rxrho(mxpath),nrpath

     &      /sw_path/
     &              drmin,drmax,mdir,lost,lx,print_swg
     &      /sw_wgin/
     &              dtheta(2),tol(2),deigen(2),thtinc,
     &              ftol,mxiter,alpha,prec,rpoly,nrtlst
     &      /sw_wgou/
     &              tp(mxeigen),capt(4,mxeigen),fofr(mxeigen),lu_mds

     &      /wf_iter/
     &              dtheta2(2),tol2(2),thtinc2,alpha2,h2,prec2,
     &              iter_flag,mxiter2,nriter

      dimension     param1(mxprm1),param2(mxprm2,mxeigen),

     &              stndev(4),

     &              heading(mxlwf),
     &              dst_lwf(mxpts),
     &              amp_lwf(mxpts,3),amp_rho(mxpts,2,mxpath),
     &              phs_lwf(mxpts,3),sig_rho(mxpts,2,mxpath),

     &              xclt(mxlat),cxclt(mxlat),sxclt(mxlat),xlng(mxlon),
     &              amp_grd(mxlon,mxlat,2),sig_grd(mxlon,mxlat,2),
     &              vsd_grd(mxlon,mxlat,2)

      equivalence  (oplat1,op_lat(1)),(oplon1,op_lon(1)),
     &             (oplat2,op_lat(2)),(oplon2,op_lon(2))

c     LWPC common
      data          prgm_id/'LWPM 2.1'/,
     &              case_id/'        '/,
     &              prfl_id/'        '/,
     &              xmtr_id/'        '/,
     &              freq/0./,tlat/0./,tlon/0./,range_max/20000./,
     &              rlat/0./,rlon/0./,rrho/0./,bearing/0./,pflag/2/,
     &              ranger/60.,90./,rangei/0.,-5./,atnmax/50./,h/50./,
     &              select_hgts/1.e2,1.e-4/,
     &              archive/'***'/,
c     MC Common
     &              max_wf_ht/120.d0/,
     &              mc_test/0.d0/,mc_step/2/,arb_azim/1/,
c     MF Common
     &              autos/1/,bxrtio/1./,iexact/0/,
     &              rtol/0.003/,xintrp/0.8/,
     &              lprnt,lgprnt,mprnt,mrprnt/4*0/,
     &              msprnt,mzprnt,rprnt,xprnt/4*0/,
     &              path_bearing/mxpath*0./,path_range/mxpath*0./,
     &              rxlat/mxpath*0./,rxlon/mxpath*0./,rxrho/mxpath*0./,
c     SW Common
     &              drmin/125./,drmax/500./,mdir/0/,print_swg/0/,
     &              ftol/.1/,mxiter/5/,alpha/3.14e-4/,prec/2./,
     &              rpoly/1/,nrtlst/5/,
c     WF Common
     &              dtheta2/.01,.001/,tol2/.05,.005/,thtinc2/.5/,
     &              alpha2/3.14d-4/,h2/50./,prec2/2./,
     &              iter_flag/1/,mxiter2/5/,
c     Miscellaneous
     &              gcpath/.false./,lwflds/.false./,opa_grid/.false./,
     &              preseg/.false./,

c     Do not change these values
     &              mds_format/'Binary'/,
     &              lwf_format/'Binary'/,
c     Programmer selectable
     &              grd_format/'ASCII'/,

     &              n_model/'ntia'/,
     &              n_month,n_day,n_year,n_UT/4*0/,bandw/1000./,

     &              nrcmp/1/,power/1./,incl,headng,talt,ralt/4*0./,
     &              month/07/,day/15/,year/1984/,UT/1615/,

     &              print_mds/0/,print_lwf/0/,print_mc/0/,print_wf/0/,
     &              print_grd/0/,

     &              brsltn/15.,3./,stndev/3.,5.,5.,5./,
     &              lwf_dst_min,lwf_dst_max,lwf_dst_inc/0.,20000.,20./,
     &              dist_avg/.false./,
     &              nrlwf/1/

      character* 40 CMDList(5)
      character*120 CMDLine

c     Get the location of the LWPC data
      call LWPC_DAT_LOC

c     Get the command line
      call GET_COMMAND_LINE (LenCMD,CMDLine)
      
c     CMDLine now contains the arguments of the command line. For this
c     program we assume that there is only the input file name.
      if (LenCMD .eq. 0) then
         file_name='lwpm'
      else
         call DECODE_LIST_STR (CMDLine,5,NrCMD,CMDList)
         file_name=CMDList(1)
      end if
      nf=STR_LENGTH(file_name)

      file_name(nf+1:nf+4)='.inp'
      OPEN (lwpcINP_lun,file=file_name,status='old')

      file_name(nf+1:nf+4)='.log'
      OPEN (lwpcLOG_lun,file=file_name,status='unknown')

c     Initialize character strings
      xmtr_id='    '
      path_id='    '
      area_id='    '
      do i=1,120,8
         root_file(i:i+7)='        '
      end do


c      PRINT *,'Marker LWPM 1'


c     Initialize logical units
      lu_dat=lwpcDAT_lun ! OpArea data, Xmtr data
      lu_ndx=lwpcNDX_lun ! ionospheric profile index file
      lu_prf=lwpcPRF_lun ! ionospheric profile tables
      lu_mds=lwpcMDS_lun ! storage of mode parameters   by LWPM
      lu_lwf=lwpcLWF_lun ! storage of mode signal       by LW_VS_D
      lu_grd=lwpcGRD_lun ! storage of op area grid data by OPA_GRID

      lu_wfd=lwpcWFD_lun ! temporary storage of wf data: direct azimuth
      lu_wfa=lwpcWFA_lun ! temporary storage of wf data: adjoint
      lu_wfp=lwpcWFP_lun ! temporary storage of wf data: previous

c     Print program's identification.
      write(lwpcLOG_lun,'(''Program identifier: '',a)') prgm_id

c     Print date/time of run.
      call GET_DATE (c_year,c_month,c_day)
      call GET_TIME (c_hrs,c_mins,c_secs,c_hsecs)
      write(lwpcLOG_lun,
     &    '(/''Run started  on '',i2.2,''-'',i2.2,''-'',i4.4,
     &       '' at '',2(i2.2,'':''),i2.2)')
     &       c_month,c_day,c_year,c_hrs,c_mins,c_secs



10    call LWP_INPUT
     &    (more_data,root_file,
     &     plt_device,plt_orientation,
     &     gcpath,lwflds,opa_grid,preseg,
     &     power,incl,headng,talt,ralt,
     &     print_lwf,print_mc,print_wf,nrcmp,
     &     lwf_dst_min,lwf_dst_max,lwf_dst_inc,
     &     map_id,map_prjctn,map_type,
     &     map_lat,map_lon,map_size,
     &     bflag,area_id,range_max,
     &     n_model,n_month,n_day,n_year,n_UT,bandw,
     &     month,day,year,UT)


      if (.not.more_data) then

c        Print date/time of completion of run.
         call GET_DATE (c_year,c_month,c_day)
         call GET_TIME (c_hrs,c_mins,c_secs,c_hsecs)
         write(lwpcLOG_lun,
     &       '(/''Run finished on '',i2.2,''-'',i2.2,''-'',i4.4,
     &          '' at '',2(i2.2,'':''),i2.2)')
     &          c_month,c_day,c_year,c_hrs,c_mins,c_secs
         STOP
      else

c        Set flags

         if (gcpath) then

c           Print path segmentation only
            do_mds=.false.
            do_lwf=.false.
            do_opa=.false.
         else
     &   if (lwflds .or. opa_grid) then

c           The user has asked for a mode sum and/or op area grid;
c           set up generation of MDS file; if it already exists and
c           is complete, then the program will only do the mode sum
c           or op area grid file
            do_mds=.true.
            if (lwflds) then
               do_lwf=.true.
            else
               do_lwf=.false.
            end if
            if (opa_grid) then
               do_opa=.true.
            else
               do_opa=.false.
            end if
         else

c           Basic case;
c           assume the MDS file needs to be created or completed
c           followed by creation of a mode sum and op area grid file
            do_mds=.true.
            do_lwf=.true.
            do_opa=.true.
         end if
      end if
      

      
c     Length of the root file name
      nfchr=STR_LENGTH(root_file)

c     Get bearing angles
      rng_mx=range_max
      call LWP_SET_BRNG
     &    (tlat,tlon,bflag,op_lat,op_lon,brsltn,
     &     mxpath,nrpath,path_bearing,path_range,
     &     rxlat,rxlon,rxrho,rng_mx)

c     Check if this is a preview (GCPATH)

      if (gcpath) then

c        Print path segmentation only
c        Loop over path bearings
         do nb=1,nrpath

            bearing=path_bearing(nb)
            range=path_range(nb)

            if (bflag .eq. 2) then ! store receiver coordinates
               rlat=rxlat(nb)
               rlon=rxlon(nb)
               rrho=rxrho(nb)
            else
               rlat=99.
               rlon=0.
               rrho=0.
            end if
            if (preseg) then
               call LWP_PRESEG (month,day,year,UT)
            else
               call LWP_PATH (month,day,year,UT)
            end if

            write(lwpcLOG_lun,
     &          '(/''case_id:   '',a/
     &             ''xmtr_id               tlat    tlon   '',
     &             ''bearing  range    rlat    rlon    rrho''/
     &               a20,f7.2,f8.2,f8.1,f9.0,f7.2,f8.2,f9.0)')
     &               case_id(:max0(1,STR_LENGTH(case_id))),
     &               xmtr_id,tlat,tlon,bearing,range,
     &               rlat,rlon,rrho
            write(lwpcLOG_lun,
     &          '(''   rho     lat     lon   azim    dip  bfield'',
     &            ''   sigma epsr ncd    chi  beta  hprime npr'')')

            do nsgmnt=1,nrsgmnt
               write(lwpcLOG_lun,
     &             '(f7.0,f7.1,f8.1,f7.1,f7.1,f8.3,
     &               1pe8.0,0pf5.0,i4,f7.1,f6.2,f7.1,i5)')
     &               dst(nsgmnt),xla(nsgmnt),xlo(nsgmnt),
     &               azm(nsgmnt),xdp(nsgmnt),fld(nsgmnt),
     &               sgm(nsgmnt),eps(nsgmnt),ncd(nsgmnt),
     &               chi(nsgmnt),bta(nsgmnt),hpr(nsgmnt),npr(nsgmnt)
            end do
            write(lwpcLOG_lun,'('' '')')
         end do

      else

c        Set up the mode parameter calculations.

c        Check if the MDS file exists:
c        if so, then assume that this is a re-start;
c        otherwise, assume that this is a new case.

c        File for storage of mode data
         if (STR_LENGTH(lwpcMDS_loc) .gt. 1) then
c        Changed STR_LENGTH to LEN_TRIM
            file_name=lwpcMDS_loc(:LEN_TRIM(lwpcMDS_loc))//
     &                root_file(:nfchr)//'.mds'
         else
            file_name=root_file(:nfchr)//'.mds'

         end if
         INQUIRE (file=file_name,exist=exists)

         if (.not.exists) then
c         PRINT *,'Marker LWPM New MDS'
c           The mode data file does not exist;
c           set up a new set of paths.

c           New file for storage of mode data
            OPEN (lu_mds,file=file_name,
     &                   status='new',form='unformatted',
     &                   iostat=iocheck,err=90)

            archive='***'
            call SET_FILE_ID (lu_mds,prgm_id,file_id(1))
            file_id(2)='***'
            file_id(3)='***'

c           Write header
            call WRITE_HDR
     &          (lu_mds,print_mds,
     &           archive,file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,tlat,tlon,
     &           path_id,oplat1,oplon1,oplat2,oplon2,
     &           mxpath,nrpath,path_bearing,path_range,
     &           rxlat,rxlon,
     &           begin_file_mds,
     &           mds_format)

c           Set the first path.
            npath1=1

c           Set the maximum range of the mode sum to the value
c           specified by the control strings OP-AREA or RANGE-MAX.
            range_lwf=rng_mx
c            PRINT *,'Marker LWPM New MDS Made',domds
         else

c           This is a re-start; find the next path to process.

c           Old file for storage of mode data
c          PRINT *,'Marker LWPM Open old mds'

            OPEN (lu_mds,file=file_name,
     &                   status='old',form='unformatted',
     &                   iostat=iocheck,err=90)

c           Go to the end of the mode data file.
c          PRINT *,'Marker LWPM Call LWP EOF MDS'

            call LWP_EOF_MDS
     &          (lu_mds,
     &           file_id,xmtridx,pathidx,
     &           mxpath,path_bearing,path_range,rxlat,rxlon,
     &           mxprm1,param1,
     &           mxeigen,eigen,
     &           mxprm2,param2,
     &           number_of_paths)

            write(lwpcLOG_lun,
     &          '(/''Restart: Number of complete paths ='',i3)')
     &               number_of_paths

            if (number_of_paths .eq. 0) then

c              File for storage of mode data is empty; start over
               archive='***'
               call SET_FILE_ID (lu_mds,prgm_id,file_id(1))
               file_id(2)='***'
               file_id(3)='***'

c              Write header
               call WRITE_HDR
     &             (lu_mds,print_mds,
     &              archive,file_id,prgm_id,
     &              case_id,prfl_id,
     &              xmtr_id,freq,tlat,tlon,
     &              path_id,oplat1,oplon1,oplat2,oplon2,
     &              mxpath,nrpath,path_bearing,path_range,
     &              rxlat,rxlon,
     &              begin_file_mds,
     &              mds_format)

c              Set the first path.
               npath1=1

c              Set the maximum range of the mode sum to the value
c              specified by the control strings OP-AREA or RANGE-MAX.
               range_lwf=rng_mx
            else

c              Verify the op area
               if (xmtr_id .ne. xmtridx .or.
     &             path_id .ne. pathidx) then

                  write(lwpcLOG_lun,
     &                '(/''ERROR: ''/
     &                   ''Existing MDS file has data for the '',
     &                   ''following transmitter and op area.''/
     &                     3x,a/3x,a/
     &                   ''These do not match the input.'')')
     &                     xmtridx,pathidx
                  STOP
               end if

               if (number_of_paths .eq. nrpath) then

c                 The mode data file is complete;
c                 skip mode parameter generation.
                  do_mds=.false.

                  CLOSE(lu_mds)

c                 Set the maximum range of the mode sum to the value
c                 specified by the control string LWF-VS-DIST.
                  range_lwf=lwf_dst_max
               else

c                 Set the next path.
                  npath1=number_of_paths+1

c                 Set the maximum range of the mode sum to the value
c                 specified by the control strings OP-AREA or RANGE-MAX.
                  range_lwf=rng_mx
               end if
            end if
         end if
      end if
c      PRINT *,'Marker LWPM MDS over?',do_mds
      if (do_mds) then

c        Loop over path bearings
         do nb=npath1,nrpath

            bearing=path_bearing(nb)
            range=path_range(nb)

            if (path_id(:8) .eq. 'Receiver') then
c              Store receiver coordinates
               rlat=rxlat(nb)
               rlon=rxlon(nb)
               rrho=rxrho(nb)
            else
               rlat=99.
               rlon=0.
               rrho=0.
            end if

            write(lwpcLOG_lun,
     &          '(''xmtr_id                tlat    tlon   '',
     &            ''bearing  range''/a,f7.2,f8.2,f9.1,f9.0)')
     &              xmtr_id,tlat,tlon,bearing,range
c            PRINT *,'Marker LWPM New MDS 1'
            call FLUSH_UNIT (lwpcLOG_lun)
c            PRINT *,'Marker LWPM New MDS 2'
            if (preseg) then
c            PRINT *,'Marker LWPM New MDS 3'
               call LWP_PRESEG (month,day,year,UT)
            else
c             PRINT *,'Marker LWPM New MDS 4'

               call LWP_PATH (month,day,year,UT)
            end if
c             PRINT *,'Marker LWPM New MDS 5'
            call LWP_DRIVER (begin_file_mds)
c            PRINT *,'Marker LWPM New MDS 6'
         end do
c         PRINT *,'Marker LWPM New MDS 7'

         CLOSE(lu_mds)
      end if
c      PRINT *,'Marker LWPM Finish do mds, start do lwf'
      if (do_lwf) then

c        Get signal vs distance
         write(lwpcLOG_lun,'(/''LWFields'')')

         if (STR_LENGTH(lwpcMDS_loc) .gt. 1) then
            file_name=lwpcMDS_loc(:STR_LENGTH(lwpcMDS_loc))//
     &                root_file(:nfchr)//'.mds'
         else
            file_name=root_file(:nfchr)//'.mds'
         end if
         OPEN (lu_mds,file=file_name,
     &                status='old',form='unformatted',
     &                iostat=iocheck,err=90)

c        Get header data from the mode parameter file
         call READ_HDR
     &       (lu_mds,print_mds,
     &        archive,file_id,prgmidx,
     &        case_id,prfl_id,
     &        xmtr_id,freq,tlat,tlon,
     &        path_id,oplat1,oplon1,oplat2,oplon2,
     &        mxpath,nrpath,path_bearing,path_range,rxlat,rxlon,
     &        begin_file_mds,end_file_mds,
     &        mds_format)

         if (STR_LENGTH(lwpcLWF_loc) .gt. 1) then
            file_name=lwpcLWF_loc(:STR_LENGTH(lwpcLWF_loc))//
     &                root_file(:nfchr)//'.lwf'
         else
            file_name=root_file(:nfchr)//'.lwf'
         end if
                
         OPEN (lu_lwf,file=file_name,
     &                status='unknown',form='unformatted',
     &                iostat=iocheck,err=90)

         archive='***'
         call SET_FILE_ID (lu_lwf,prgm_id,file_id(2))
         file_id(3)='***'
c         PRINT *,'Marker LWPM Should be done with pauses'
         call WRITE_HDR
     &       (lu_lwf,print_lwf,
     &        archive,file_id,prgm_id,
     &        case_id,prfl_id,
     &        xmtr_id,freq,tlat,tlon,
     &        path_id,oplat1,oplon1,oplat2,oplon2,
     &        mxpath,nrpath,path_bearing,path_range,rxlat,rxlon,
     &        begin_file_lwf,
     &        lwf_format)

c        Set parametric variation (currently over heading)
         heading(1)=headng
           
         call LW_VS_D
     &       (lu_mds,lu_lwf,print_lwf,print_mc,print_wf,
     &        begin_file_mds,end_file_mds,begin_file_lwf,
     &        nrcmp,ralt,power,
     &        mxlwf,nrlwf,incl,heading,talt,
     &        lwf_dst_min,range_lwf,lwf_dst_inc,
     &        dist_avg,
     &        mxpts,nrpts,dst_lwf,amp_lwf,phs_lwf)

         CLOSE(lu_mds)
         CLOSE(lu_lwf)
      end if
            
      if (do_opa) then

c        Generate op area grid
         write(lwpcLOG_lun,'(/''OpArea Grid'')')

c        Open the input LWF file
         if (STR_LENGTH(lwpcLWF_loc) .gt. 1) then
            file_name=lwpcLWF_loc(:STR_LENGTH(lwpcLWF_loc))//
     &                root_file(:nfchr)//'.lwf'
         else
            file_name=root_file(:nfchr)//'.lwf'
         end if
         OPEN (lu_lwf,file=file_name,
     &                status='old',form='unformatted',
     &                iostat=iocheck,err=90)

c        Make sure this file is compatible with generation of
c        a grid file

c        Get header data from the mode parameter file
         call READ_HDR
     &       (lu_lwf,print_lwf,
     &        archive,file_id,prgmidx,
     &        case_id,prfl_id,
     &        xmtr_id,freq,tlat,tlon,
     &        path_id,oplat1,oplon1,oplat2,oplon2,
     &        mxpath,nrpath,path_bearing,path_range,rxlat,rxlon,
     &        begin_file_mds,end_file_mds,
     &        lwf_format)

         if (path_id(:8) .ne. 'Receiver' .and.
     &       path_id(:8) .ne. 'Bearings') then

c           This file contains paths designed to cover an op area
            REWIND (lu_lwf)

c           Set up op area grid
            call OPA_SET_UP_GRID
     &          (1.25,mxlat,nrlat,mxlon,nrlon,
     &           oplat1,oplon1,oplat2,oplon2,
     &           xclt,cxclt,sxclt,xlng)

c           Formulate grid file name using the root of the transmitter
c           name and the area identification
            nrchr_a=STR_LENGTH(area_id)
            nrchr_f=nfchr

cxxc           Drop FAT compatibility.
cxx            nrchr_a=MIN(5,nrchr_a)
cxx            nrchr_f=MIN(3,nrchr_f)

            call STR_LOWER (area_id,1,nrchr_a)
            call STR_UPPER (area_id,1,1)
            call STR_LOWER (root_file,1,nrchr_f)

c           Attach the directory name
            if (STR_LENGTH(lwpcGRD_loc) .gt. 1) then
               file_name=lwpcGRD_loc(:STR_LENGTH(lwpcGRD_loc))//
     &                   root_file(:nrchr_f)//
     &                   area_id(:nrchr_a)//'.grd'
            else
               file_name=root_file(:nrchr_f)//
     &                   area_id(:nrchr_a)//'.grd'
            end if

c           Open the GRD file
            if (grd_format(1:1) .eq. 'A' .or.
     &          grd_format(1:1) .eq. 'a') then

               OPEN (lu_grd,file=file_name,
     &                      status='unknown',form='formatted',
     &                      iostat=iocheck,err=90)
            else

               OPEN (lu_grd,file=file_name,
     &                      status='unknown',form='unformatted',
     &                      iostat=iocheck,err=90)
            end if

c           Fill grid for this transmitter
            call OPA_SIGNAL_GRID
     &          (lu_lwf,lu_grd,print_grd,
     &           prgm_id,prfl_id,
     &           area_id,oplat1,oplon1,oplat2,oplon2,
     &           freq,power,ralt,stndev,
     &           mxpath,nrpath,path_bearing,path_range,rxlat,rxlon,
     &           mxprm1,nrprm1,param1,
     &           mxpts,nrpts,dst_lwf,amp_lwf,phs_lwf,amp_rho,sig_rho,
     &           nrcmp,mxlat,nrlat,mxlon,nrlon,
     &           xclt,cxclt,sxclt,xlng,amp_grd,sig_grd,
     &           grd_format)

            CLOSE(lu_lwf)
            CLOSE(lu_grd)

c           Now that the frequency is known, generate noise data grid.
            if (pflag .eq. 4 .or. STR_LENGTH(n_model) .gt. 0) then

               if (pflag .eq. 4) then

c                 Get noise for the specific date
                  imonth=month
                  iday  =day
                  iyear =year
                  iUT   =UT
               else

c                 Use values from A-NOISE control string
                  imonth=n_month
                  iday  =n_day
                  iyear =n_year
                  iUT   =n_UT
               end if

c              Form noise grid file name from signal grid file name
               if (STR_LENGTH(lwpcGRD_loc) .gt. 1) then
                  file_name=lwpcGRD_loc(:STR_LENGTH(lwpcGRD_loc))//
     &                      root_file(:nrchr_f)//
     &                      area_id(:nrchr_a)//'.'//n_model(:3)
               else
                  file_name=root_file(:nrchr_f)//
     &                      area_id(:nrchr_a)//'.'//n_model(:3)
               end if

c              Open the noise grid file
               if (grd_format(1:1) .eq. 'A' .or.
     &             grd_format(1:1) .eq. 'a') then

                  OPEN (lu_grd,file=file_name,
     &                         status='unknown',form='formatted',
     &                         iostat=iocheck,err=90)
               else

                  OPEN (lu_grd,file=file_name,
     &                         status='unknown',form='unformatted',
     &                         iostat=iocheck,err=90)
               end if

               call OPA_NOISE_GRID
     &             (lu_grd,print_grd,
     &              prgm_id,
     &              area_id,oplat1,oplon1,oplat2,oplon2,
     &              n_model,freq,imonth,iday,iyear,iUT,bandw,ralt,
     &              mxpath,nrpath,path_bearing,path_range,rxlat,rxlon,
     &              mxprm1,nrprm1,param1,
     &              nrcmp,mxlat,nrlat,mxlon,nrlon,
     &              xclt,xlng,amp_grd,sig_grd,vsd_grd,
     &              grd_format)

               CLOSE(lu_grd)
            end if
         end if
      end if
            
      go to 10
      

c     Error exits
90    write(lwpcLOG_lun,
     &    '(/''ERROR: '',
     &       ''I/O error '',i3,'' occurred trying to open '',
     &       ''file: '',a)')
     &         iocheck,file_name(:STR_LENGTH(file_name))

      END   ! LWPM