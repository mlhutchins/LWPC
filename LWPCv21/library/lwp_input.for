      SUBROUTINE LWP_INPUT
     &          (more_data,root_file,
     &           plt_device,plt_orientation,
     &           gcpath,lwflds,opa_grid,preseg,
     &           power,incl,headng,talt,ralt,
     &           print_lwf,print_mc,print_wf,nrcmp,
     &           lwf_dst_min,lwf_dst_max,lwf_dst_inc,
     &           map_id,map_prjctn,map_type,
     &           map_lat,map_lon,map_size,
     &           bflag,area_id,range_max,
     &           n_model,n_month,n_day,n_year,n_UT,bandw,
     &           month,day,year,UT)

c***********************************************************************

c     Reads input data for LWPM.

c***********************************************************************

c CONTROL STRINGS:

c PLOTTER     PLOT-DEVICE PLOT-ORIENTATION
c FILE-MDS    directory-location-of-MDS-files
c FILE-LWF    directory-location-of-LWF-files
c FILE-GRD    directory-location-of-GRD-files
c FILE-PRF    directory-location-of-PRF-files
c FILE-NDX    directory-location-of-NDX-files
c FILE-LWT    directory-location-of-LWT-files
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
c LWTIME      MONTH/day/year hour:minute
c BEARINGS    bearing-1 bearing-2 ...
c +BEARINGS   bearing-n bearing-m ...
c RECEIVERS   rx-lat-1 rx-lon-1 rx-lat-2 rx-lon-2 ...
c +RECEIVERS  rx-lat-n rx-lon-n rx-lat-m rx-lon-m ...
c RANGE-MAX   maximum-range-for-BEARINGS-and-RECEIVERS
c OP-AREA     AREA-ID op-lat1 op-lon1 op-lat2 op-lon2
c OP-AREA     AREA-ID OP-AREA-SPECIFICATION-FILE
c A-NOISE     N_MODEL MONTH/day/year hour:minute band-width
c LWF-VS-DIST lwf-dist-max lwf-dist-inc
c MC-OPTIONS  MC-STEP mc-test wf-iterate
c MAP-AREA    MAP-ID RECTANGULAR  lat1 lon1 lat2 lon2 size-x size-y
c MAP-AREA    MAP-ID MERCATOR     lat1 lon1 lat2 lon2 size-x size-y
c MAP-AREA    MAP-ID GNOMONIC     lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID AZIMUTHAL    lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID ORTHOGRAPHIC lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID MAP-AREA-SPECIFICATION-FILE
c MAP-TYPE    LAND
c MAP-TYPE    COAST
c MAP-TYPE    LAND COAST
c MAP-TYPE    COAST LAND
c PRINT-LWF   print-lwf
c PRINT-MC    print-mc
c PRINT-MDS   print-mds
c PRINT-SWG   print-swg
c PRINT-WF    print-wf
c GCPATH
c LWFLDS
c OPA-GRID
c PRESEG
c START
c QUIT

c***********************************************************************

c PLOTTER           Defines the graphical output device

c    INPUT PARAMETERS:

c    PLOT-DEVICE    Plotter device (SYS-SCR/SYS-PRN/FILE-NAME)

c    PLOT-ORIENTATION
c                   Orientation (LANDSCAPE/PORTRAIT)


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


c LWTIME            Defines the date and time for the output.

c    NOTE:          This control string used only by program LWTime.

c    INPUT PARAMETERS:

c    MONTH/day/year hour:minute
c                   indicates the specific date and time to be used;
c                   the parameters of the date and time are:
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


c BEARINGS          Defines a list of bearing angles used to define
c                   paths.

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

c    lwf-dist-max   distance at which to end the mode sum
c    lwf-dist-inc   distance increment


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
c                   =1: adds conversion coefficients
c                   =2: adds integrals


c PRINT-WF          Indicates levels of print from WaveFields

c    INPUT PARAMETERS:

c    print-wf       =0: minimum print
c                   =1: adds iterations
c                   =1: adds fields vs. height


c MAP-AREA          Defines the map onto which the path segmentation is
c                   to be plotted for the "PREVIEW".

c    INPUT PARAMETERS:

c    MAP-ID         map area name [20 characters]

c    MERCATOR       calls for a Mercator projection

c    lat1           bottom latitude  of the map area
c    lon1           left   longitude of the map area
c    lat2           top    latitude  of the map area
c    lon2           right  longitude of the map area

c    RECTANGULAR    calls for a projection which is linear in latitude
c                   and longitude

c    lat1           bottom latitude  of the map area
c    lon1           left   longitude of the map area
c    lat2           top    latitude  of the map area
c    lon2           right  longitude of the map area

c    AZIMUTHAL      Azimuthal-equidistant projection

c    lat0           center latitude  of the map area
c    lon0           center longitude of the map area
c    range          maximum range to be mapped

c    GNOMONIC       call for a projection in which great circles are
c                   straight lines

c    lat0           center latitude  of the map area
c    lon0           center longitude of the map area
c    range          maximum range to be mapped

c    ORTHOGRAPHIC   Orthographic projection

c    lat0           center latitude  of the map area
c    lon0           center longitude of the map area
c    range          maximum range to be mapped

c    size-x         length of horizontal axis
c    size-y         length of vertical   axis

c    MAP-AREA-SPECIFICATION-FILE
c                   name of a file which contains records containing
c                   the above list of parameters for the map area.
c                   the file is searched for a match with MAP-ID and
c                   the corresponding parameters are returned.


c MAP-TYPE          Indicates the method of denoting land masses for the
c                   map plots.

c    INPUT PARAMETERS:

c    LAND           does land mass overlay from LWPC_DAT:LAND$D
c    COAST          does coastal outlines  from LWPC_DAT:WRLD$D


c PRINT-LWF         Indicates levels of print out for the mode summation

c    INPUT PARAMETERS:

c    print-lwf      =0: minimum print from LWF file
c                   =1: adds fields vs. distance


c PRINT-MC          Indicates levels of print from ModeConversion

c    INPUT PARAMETERS:

c    print-mc       =0: minimum print
c                    1: adds conversion coefficients
c                    2: adds integrals


c PRINT-MDS         Indicates levels of print out for the mode
c                   parameters

c    INPUT PARAMETERS:

c    print-mds      =0: minimum print from MDS file
c                   =1: adds attenuation rate, etc.
c                   =2: adds conversion coefficients
c                   =3: adds slab integrals


c PRINT-SWG         Indicates levels of print out for the calculations
c                   along the propagation paths

c    INPUT PARAMETERS:

c    print-swg      =0: none
c                   =1: path parameters and mode lists:   LWP_DRIVER
c                   =2: mode parameter outputs:           SW_WVGD
c                   =3: extrapolation parameter outputs:  SW_EXTRAP


c PRINT-WF          Indicates levels of print from WaveFields

c    INPUT PARAMETERS:

c    print-wf       =0: minimum print
c                    1: adds iterations
c                    2: adds fields vs. height


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


c START             Indicates that all inputs are complete and processing
c                   of the paths can begin.


c QUIT              Terminates the run.


c***********************************************************************

c NAMELIST          Set miscellaneous parameters with NAMELIST (DATUM).

c    INPUT PARAMETERS:

c    brsltn         bearing angle resolution in degrees; used when
c                   BFLAG=0

c    Input unique to SW:

c    ftol           F-function tolerance used to terminate iterations

c    maxitr         maximum number of iterations

c    rpoly          flag to control inexact/exact calculations

c    nrtlst         number of T-list angles to use in the inexact
c                   interpolation of the reflection coefficients

c    Input unique to MF:

c    mf_range       limits search area for modes; a 2x3 matrix
c                   (1,1) FREQ1; (1,2) min RANGER; (1,3) min RANGEI
c                   (2,1) FREQ2; (2,2) min RANGER; (2,3) min RANGEI

c    atnmax         maximum attenuation rate used to restrict search
c                   area for modes

c    The following variables control debug print out:

c    lprnt:         search box; branch points; gmax; bxrtio; tmesh; lub;
c                   htintrp; g;
c                   adjmsh;
c                   theta, reflht

c    mprnt:         ITRATE

c    mrprnt:        search box; branch points; gmax; bxrtio; tmesh; lub;
c                   zeta; eigen vectors;
c                   search for htintrp; htintrp; g;
c                   adjmsh;
c                   theta, reflht;
c                   iterations on final eigens;
c                   INIT_ROE; INIT_LG

c    msprnt:        list of search boxes;
c                   WGSORT

c    mzprnt:        FZEROS

c    rprnt:         theta; q; r; x

c    xprnt:         r; dr; ep; dep; v; dv

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
c    frequency      kz
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
c     23 Oct 90     Modified to use the more rigorous testing of maps,
c                   op areas and transmitters found in GET_MAP, GET_OPA
c                   and GET_XMTR.

c     28 Feb 91     Modified to accept MF printout flags through DATUM.

c     14 Mar 91     Store noise year MOD 100.

c     30 Jan 92     Modified to use DECODE_* routines.

c     10 Feb 92     Modified /LWPC_IN/ to store HOFWR, TOPHT, BOTHT and
c                   PINDEX

c     27 Feb 92     Deleted /LWPC_PR/.

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

c     20 Jan 93     Added frequency dependent limits for RANGER and
c                   RANGEI.

c     10 Nov 93     Added solar zenith angle dependent ionospheres.

c     11 Nov 93     Removed PREVIEW control string.

c     30 Dec 93     Added control strings for LWTime and SetupLWT.

c     17 Jan 94     Removed unused common MF_SW_2.

c     09 Feb 94     Changed PLT_DEVICE and PLT_ORIENTATION to
c                   CHARACTER*(*).

c     22 Feb 94     Added FILE-LWT to specify the location of the LWT
c                   files created and used by LWTime.

c     20 Apr 95     Ensure that the 'lwpcXXX_loc' end with '\'.

c     26 Sep 95     Changed all PRINTs to WRITE(6)s.

c     21 Oct 95     Changed to get logical units from LWPC_LUN.CMN.

c     18 Dec 95     Added PRINT-MC and PRINT-WF parameters to
c                   argument list.

c     16 Dec 96     Generalize delimiters used with file names.

c     04 Mar 97     Added options for presegmentation.

c     15 Jan 98     Dropped PLT_UNIT; use PLT_DEVICE to store file name
c                   when output to a file.

c     05 Feb 98     Removed control string FILE-DAT.

c*******************!***************************************************

c     LWPC parameters
      INCLUDE      'lwpc_cfg.cmn'
      INCLUDE      'lwpc_lun.cmn'

      parameter    (mxpath=120,mxsgmnt=201,mxeigen=50)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex

      real     *  4 ranger,rangei,atnmax,lub,h
      complex  *  8 eigen

      common/lwpc_mf/
     &              ranger(2),rangei(2),atnmax,lub,h,
     &              eigen(mxeigen),nreigen

      real     *  4 hten,algen,htnu,algnu,
     &              charge,ratiom,select_hgts,hgts

      common/lwpc_pr/
     &              hten(51),algen(51,3),nrhten,ihten,
     &              htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &              charge(3),ratiom(3),nrspec,lu_prf,
     &              select_hgts(2),hgts(3)

      integer       rprnt,xprnt

      common/mf_prnt/
     &              lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

      common/mf_sw_3/
     &              op_lat(2),op_lon(2),brsltn(2),
     &              bearing(mxpath),rhomax(mxpath),
     &              rxlat(mxpath),rxlon(mxpath),rxrho(mxpath),nrpath

      integer       print_swg
      real     *  4 drmin,drmax

      common/sw_path/
     &              drmin,drmax,mdir,lost,lx,print_swg

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),thtinc,
     &              ftol,maxitr,alpha,prec,rpoly,nrtlst

      complex       tp,param2

      common/sw_wgou/
     &              tp(mxeigen),param2(5,mxeigen),lu_mds

      character*120 prfl_file
      real     *  8 max_wf_ht,mc_test
      integer       arb_azim

      common/mc_inpt/
     &              prfl_file,max_wf_ht,mc_test,
     &              lu_wfd,lu_wfa,lu_wfp,mc_step,arb_azim

      common/mf_sw_1/
     &              dst(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt

      character*(*) plt_device,plt_orientation
      character*  1 components
      character*  1 delimiter(3)
      character*  3 month_string
      character*  4 map_type(2),map_prjctn,n_model
      character*  5 time_string
      character* 12 prfl_model
      character* 20 area_id,map_id,
     &              mc_option(3)
      character* 22 date_string
      character*120 string,control_string,data_string,
     &              directory,root_file,extension
      character*200 error_msg
      logical       more_data,bdata,fdata,xdata,
     &              gcpath,lwflds,opa_grid,preseg
      integer       str_length,
     &              bflag,
     &              month,day,year,hour,minute,UT,
     &              print_mds,print_lwf,print_mc,print_wf
      real          incl,map_lat(2),map_lon(2),map_size(2),
     &              lwf_dst_min,lwf_dst_max,lwf_dst_inc,
     &              mf_range(2,3),
     &              preseg_list(10)

      dimension     rx_location(mxpath),dst_max_inc(2)

      namelist/datum/
     &              brsltn,
     &              select_hgts,
     &              ftol,maxitr,rpoly,nrtlst,
     &              atnmax,mf_range,
     &              lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

      data          bdata,fdata,xdata/3*.false./,

c                            f1  f2  r1  r2  i1  i2
     &              mf_range/20.,60.,60.,40.,-9.,-6./


c     Get delimiters used in the file names
      call GET_DELIMITER (delimiter)

      more_data=.true.
10    read (lwpcINP_lun,'(a)',end=90) string
      write(lwpcLOG_lun,'(a)') string

      if (string(1:1) .eq. ' ') go to 10

c     Get the limits of the data within the control string
      call DECODE_CONTROL_DATA (string,control_string,data_string)

c     Convert the control string to upper case
      call STR_UPPER (control_string,0,0)

c FILE-LWF
      if (control_string(1:8) .eq. 'FILE-LWF') then
         if (data_string(1:1) .eq. ' ') then
c           Current directory is default
            lwpcLWF_loc=' '
         else
            lwpcLWF_loc=data_string(1:STR_LENGTH(data_string))
            n=STR_LENGTH(lwpcLWF_loc)
            if (lwpcLWF_loc(n:n) .ne. delimiter(3)) then
               n=n+1
               lwpcLWF_loc(n:n)=delimiter(3)
            end if
         end if
         go to  10
      end if

c FILE-GRD
      if (control_string(1:8) .eq. 'FILE-GRD') then
         if (data_string(1:1) .eq. ' ') then
c           Current directory is default
            lwpcGRD_loc=' '
         else
            lwpcGRD_loc=data_string(1:STR_LENGTH(data_string))
            n=STR_LENGTH(lwpcGRD_loc)
            if (lwpcGRD_loc(n:n) .ne. delimiter(3)) then
               n=n+1
               lwpcGRD_loc(n:n)=delimiter(3)
            end if
         end if
         go to 10
      end if

c FILE-MDS
      if (control_string(1:8) .eq. 'FILE-MDS') then
         if (data_string(1:1) .eq. ' ') then
c           Current directory is default
            lwpcMDS_loc=' '
         else
            lwpcMDS_loc=data_string(1:STR_LENGTH(data_string))
            n=STR_LENGTH(lwpcMDS_loc)
            if (lwpcMDS_loc(n:n) .ne. delimiter(3)) then
               n=n+1
               lwpcMDS_loc(n:n)=delimiter(3)
            end if
         end if
         go to 10
      end if

c FILE-LWT
      if (control_string(1:8) .eq. 'FILE-LWT') then
         if (data_string(1:1) .eq. ' ') then
c           Current directory is default
            lwpcLWT_loc=' '
         else
            lwpcLWT_loc=data_string(1:STR_LENGTH(data_string))
            n=STR_LENGTH(lwpcLWT_loc)
            if (lwpcLWT_loc(n:n) .ne. delimiter(3)) then
               n=n+1
               lwpcLWT_loc(n:n)=delimiter(3)
            end if
         end if
         go to 10
      end if

c FILE-PRF
      if (control_string(1:8) .eq. 'FILE-PRF') then
         if (data_string(1:1) .eq. ' ') then
c           Current directory is default
            lwpcPRF_loc=' '
         else
            lwpcPRF_loc=data_string(1:STR_LENGTH(data_string))
            n=STR_LENGTH(lwpcPRF_loc)
            if (lwpcPRF_loc(n:n) .ne. delimiter(3)) then
               n=n+1
               lwpcPRF_loc(n:n)=delimiter(3)
            end if
         end if
         go to 10
      end if

c FILE-NDX
      if (control_string(1:8) .eq. 'FILE-NDX') then
         if (data_string(1:1) .eq. ' ') then
c           Current directory is default
            lwpcNDX_loc=' '
         else
            lwpcNDX_loc=data_string(1:STR_LENGTH(data_string))
            n=STR_LENGTH(lwpcNDX_loc)
            if (lwpcNDX_loc(n:n) .ne. delimiter(3)) then
               n=n+1
               lwpcNDX_loc(n:n)=delimiter(3)
            end if
         end if
         go to 10
      end if

c A-NOISE
      if (control_string(1:1) .eq. 'A') then

         call DECODE_A_NOISE
     &       (data_string,n_model,n_month,n_day,n_year,n_UT,bandw)
         go to 10
      end if

c BEARINGS
      if (control_string(1:1) .eq. 'B') then

         call DECODE_LIST_FLT
     &       (data_string,mxpath,nrpath,bearing)

         bflag=1
         bdata=.true.
         path_id='Bearings'
         op_lat(1)=0.
         op_lon(1)=0.
         op_lat(2)=0.
         op_lon(2)=0.
         go to 10
      end if

c +BEARINGS
      if (control_string(1:2) .eq. '+B') then

         call DECODE_LIST_FLT
     &       (data_string,mxpath-nrpath,nrlist,bearing(nrpath+1))
         nrpath=nrpath+nrlist
         go to 10
      end if

c CASE-ID
      if (control_string(1:1) .eq. 'C') then

         if (data_string(1:1) .eq. ' ') then
            write(error_msg,
     &          '(''[LWP_INPUT]: Data string missing'')')
            call LWPC_ERROR ('ERROR',error_msg)
         end if

         case_id=data_string
         go to 10
      end if

c GCPATH
      if (control_string(1:1) .eq. 'G') then

         gcpath=.true.
         go to 10
      end if

c IONOSPHERE
      if (control_string(1:1) .eq. 'I') then

         call DECODE_IONOSPHERE
     &       (data_string,
     &        prfl_model,month,day,year,UT,pflag,beta,hprime,prfl_file)
         go to 10
      end if

c LWTIME
      if (control_string(1:3) .eq. 'LWT') then

         call STR_COUNT_LIST (data_string,0,0,nritem)
         if (nritem .ne. 2) then
            write(error_msg,
     &          '(''[LWP_INPUT]: '',
     &            ''Date/Time specification is incomplete'')')
            call LWPC_ERROR ('ERROR',error_msg)
         end if

         call STR_GET_ITEM (1,data_string,date_string,n1,n2)
         call STR_UPPER (date_string,0,0)
         call DECODE_DATE (date_string,month_string,day,year)

         call STR_UPPER (month_string,1,1)
         call STR_LOWER (month_string,2,0)
         call MONTH_NUMBER (month_string,month)

         year=MOD(year,100) ! store last two digits of the year

         call STR_GET_ITEM (2,data_string,time_string,n1,n2)
         call DECODE_TIME (time_string,hour,minute)
         UT=100*hour+minute
         go to 10
      end if

c LWF-VS-DISTANCE
      if (control_string(1:4) .eq. 'LWF-' .or.
     &    control_string(1:4) .eq. 'LWF_') then

         call DECODE_LIST_FLT
     &       (data_string,2,nrlist,dst_max_inc)
         lwf_dst_min=0.
         lwf_dst_max=dst_max_inc(1)
         if (nrlist .eq. 2) lwf_dst_inc=dst_max_inc(2)
         go to 10
      end if

c MC-OPTIONS
      if (control_string(1:2) .eq. 'MC') then

         call DECODE_LIST_STR
     &       (data_string,3,nrlist,mc_option)

         call STR_LOWER (mc_option(1),0,0)
         if (mc_option(1)(1:1) .eq. 'f') then
            mc_step=0
         else
     &   if (mc_option(1)(1:1) .eq. 'm') then
            mc_step=1
         else
     &   if (mc_option(1)(1:1) .eq. 'a') then
            mc_step=2
         else
            write(error_msg,
     &          '(''[LWP_INPUT]: '',
     &            ''MC-STEP option not recognized'')')
            call LWPC_ERROR ('ERROR',error_msg)
         end if

         if (nrlist .gt. 1) then
            write(string,'(''(f'',i3.3,''.0)'')')
     &            STR_LENGTH(mc_option(2))
            read (mc_option(2),string) mc_test

            if (nrlist .gt. 2) then
               call STR_LOWER (mc_option(3),0,0)
               if (mc_option(3)(1:1) .eq. 't') then
                  iter_flag=1
               else
                  iter_flag=0
               end if
            end if
         end if
         go to 10
      end if

c LWFLDS
      if (control_string(1:3) .eq. 'LWF') then

         lwflds=.true.
         go to 10
      end if

c MAP-AREA
      if (control_string(1:5) .eq. 'MAP-A' .or.
     &    control_string(1:5) .eq. 'MAP_A') then

         call DECODE_MAP_AREA
     &       (data_string,.true.,
     &        map_id,map_prjctn,
     &        map_lon(1),map_lon(2),map_size(1),
     &        map_lat(1),map_lat(2),map_size(2))
         call STR_LOWER (map_id, 0,0)
         call STR_LOWER (map_prjctn,0,0)
cxxcDEBUG
cxx         write(lwpcLOG_lun,
cxx     &       '(/''[LWP_INPUT] DEBUG: '',a,1x,a,1x,6f7.1)')
cxx     &          map_id(:STR_LENGTH(map_id)),map_prjctn,
cxx     &          map_lat(1),map_lon(1),map_lat(2),map_lon(2)
         go to 10
      end if

c MAP-TYPE
      if (control_string(1:5) .eq. 'MAP-T' .or.
     &    control_string(1:5) .eq. 'MAP_T') then

         call DECODE_LIST_STR
     &       (data_string,2,nrlist,map_type)
         do n=1,nrlist
            call STR_LOWER (map_type(n),0,0)
         enddo
         go to 10
      end if

c OP-AREA
      if (control_string(1:3) .eq. 'OP-' .or.
     &    control_string(1:3) .eq. 'OP_') then

         call DECODE_OP_AREA
     &       (data_string,.true.,
     &        area_id,op_lon(1),op_lon(2),op_lat(1),op_lat(2))

         call STR_LOWER (area_id, 0,0)

         bflag=0
         bdata=.true.
         path_id=area_id
cxxcDEBUG
cxx         write(lwpcLOG_lun,
cxx     &       '(/''[LWP_INPUT] DEBUG: '',a,1x,4f7.1)')
cxx     &          area_id(:STR_LENGTH(area_id)),
cxx     &          op_lat(1),op_lon(1),op_lat(2),op_lon(2)
         go to 10
      end if

c OPA-GRID
      if (control_string(1:3) .eq. 'OPA') then

         opa_grid=.true.
         go to 10
      end if

c PLOTTER
      if (control_string(1:2) .eq. 'PL') then

         call DECODE_PLOTTER
     &       (data_string,plt_device,plt_orientation)
         call STR_LOWER (plt_device,0,0)
         go to 10
      end if

c PRESEG
      if (control_string(1:3) .eq. 'PRE') then

c        Read precomputed segmentation data
         rho=0.
         nrsgmnt=0
         do while (rho .lt. 40000.)
            read (lwpcINP_lun,'(a)') data_string
            call DECODE_LIST_FLT
     &          (data_string,10,nrlist,preseg_list)
            rho=preseg_list(1)
            if (rho .lt. 40000.) then
               nrsgmnt=nrsgmnt+1
               num(nrsgmnt)=0
               dst(nrsgmnt)=preseg_list( 1)
               azm(nrsgmnt)=preseg_list( 2)
               xdp(nrsgmnt)=preseg_list( 3)
               fld(nrsgmnt)=preseg_list( 4)
               ncd(nrsgmnt)=preseg_list( 5)
               sgm(nrsgmnt)=preseg_list( 6)
               eps(nrsgmnt)=preseg_list( 7)
               npr(nrsgmnt)=preseg_list( 8)
               bta(nrsgmnt)=preseg_list( 9)
               hpr(nrsgmnt)=preseg_list(10)
            end if
         end do
         preseg=.true.
         go to 10
      end if

c PRINT-LWF
      if (control_string(1:7) .eq. 'PRINT-L' .or.
     &    control_string(1:7) .eq. 'PRINT_L') then

         call DECODE_LIST_INT
     &       (data_string,1,nlist,print_lwf)
         go to 10
      end if

c PRINT-MC
      if (control_string(1:8) .eq. 'PRINT-MC' .or.
     &    control_string(1:8) .eq. 'PRINT_MC') then

         call DECODE_LIST_INT
     &       (data_string,1,nlist,print_mc)
         go to 10
      end if

c PRINT-MDS
      if (control_string(1:8) .eq. 'PRINT-MD' .or.
     &    control_string(1:8) .eq. 'PRINT_MD') then

         call DECODE_LIST_INT
     &       (data_string,1,nlist,print_mds)
         go to 10
      end if

c PRINT-SWG
      if (control_string(1:7) .eq. 'PRINT-S' .or.
     &    control_string(1:7) .eq. 'PRINT_S') then

         call DECODE_LIST_INT
     &       (data_string,1,nlist,print_swg)
         go to 10
      end if

c PRINT-WF
      if (control_string(1:7) .eq. 'PRINT-W' .or.
     &    control_string(1:7) .eq. 'PRINT_W') then

         call DECODE_LIST_INT
     &       (data_string,1,nlist,print_wf)
         go to 10
      end if

c QUIT
      if (control_string(1:1) .eq. 'Q') then

         more_data=.false.
         RETURN
      end if

c RANGE-MAX
      if (control_string(1:2) .eq. 'RA') then

         call DECODE_LIST_FLT
     &       (data_string,1,nrlist,range_max)
         go to 10
      end if

c RECEIVERS
      if (control_string(1:2) .eq. 'RE') then

c        Read the data in linear order. Later, this array must be
c        rearranged to put RX_LAT and RX_LON in proper order.
         call DECODE_LIST_FLT
     &       (data_string,mxpath,nrlist,rx_location)
         nrpath=0
         do m=1,nrlist,2
            nrpath=nrpath+1
            rxlat(nrpath)=rx_location(m)
            rxlon(nrpath)=rx_location(m+1)
         end do

         bflag=2
         bdata=.true.
         path_id='Receivers'
         op_lat(1)=0.
         op_lon(1)=0.
         op_lat(2)=0.
         op_lon(2)=0.
         go to 10
      end if

c +RECEIVERS
      if (control_string(1:2) .eq. '+R') then

         call DECODE_LIST_FLT
     &       (data_string,mxpath,nrlist,rx_location)
         do m=1,nrlist,2
            nrpath=nrpath+1
            rxlat(nrpath)=rx_location(m)
            rxlon(nrpath)=rx_location(m+1)
         end do
         go to 10
      end if

c RX-DATA
      if (control_string(1:2) .eq. 'RX') then

         call DECODE_RX_DATA
     &       (data_string,components,nrcmp,ralt)
         go to 10
      end if

c TX-DATA
c JX-DATA
      if (control_string(1:4) .eq. 'TX-D' .or.
     &    control_string(1:4) .eq. 'TX_D' .or.
     &    control_string(1:4) .eq. 'JX-D' .or.
     &    control_string(1:4) .eq. 'JX_D') then

         call DECODE_TX_DATA
     &       (data_string,.true.,
     &        xmtr_id,freq,tlat,tlon,power,incl,headng,talt)

         xdata=.true.
cxxcDEBUG
cxx         write(lwpcLOG_lun,
cxx     &       '(/''[LWP_INPUT] DEBUG: '',a,1x,7f7.1)')
cxx     &          xmtr_id(:STR_LENGTH(xmtr_id)),
cxx     &          freq,tlat,tlon,power,incl,headng,talt
         go to 10
      end if

c TX-NTR
c JX-NJR
      if (control_string(1:1) .eq. 'T' .or.
     &    control_string(1:1) .eq. 'J') then

         call DECODE_FILE_NAME
     &       (data_string,directory,root_file,extension)

         fdata=.true.
         go to 10
      end if

c***********************************************************************
c NAMELIST
      if (control_string(1:1) .eq. 'N') then

c        Get NAMELIST parameters (DEBUG only)
         read (lwpcINP_lun,datum)
         write(lwpcLOG_lun,datum)
         go to 10
      end if
c***********************************************************************

c START
      if (control_string(1:1) .ne. 'S') then
         write(error_msg,
     &       '(''[LWP_INPUT]: Control string not recognized'')')
         call LWPC_ERROR ('ERROR',error_msg)
      end if

      if (.not.bdata) then
         write(error_msg,
     &       '(''[LWP_INPUT]: Op area or path data not specified'')')
         call LWPC_ERROR ('ERROR',error_msg)
      end if

      if (.not.fdata) then
         write(error_msg,
     &       '(''[LWP_INPUT]: Root file name not specified'')')
         call LWPC_ERROR ('ERROR',error_msg)
      end if

      if (.not.xdata) then
         write(error_msg,
     &       '(''[LWP_INPUT]: Xmtr/Jammer data not specified'')')
         call LWPC_ERROR ('ERROR',error_msg)
      end if
      go to 99

c     End of input file.
90    write(lwpcLOG_lun,'(''End of input file'')')
      more_data=.false.
      RETURN

c     Set profile identification
99    call PRFL_SPECIFICATION
     &    (.true.,print_swg,
     &     prfl_file,prfl_id,pflag,
     &     freq,month,day,year,UT,
     &     lat,lon,range,dip,zenith,
     &     hpr_mid,beta,hprime,pindex)

c     Select MODEFNDR search range
      ranger(2)=90.
      rangei(1)=0.
      if (freq .lt. mf_range(1,1)) then
         ranger(1)=mf_range(1,2)
         rangei(2)=mf_range(1,3)
      else
         ranger(1)=mf_range(2,2)
         rangei(2)=mf_range(2,3)
      end if

      RETURN
      END      ! LWP_INPUT