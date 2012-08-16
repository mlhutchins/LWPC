      SUBROUTINE FORDRIVE

c     PROGRAM prvwPlot

c***********************************************************************

c Preview option for Long Wave Propagation Model (LWPM)

c Generates displays of propagation paths for LWPM runs. All control
c strings used by LWPM are recognized by this program but may not be
c used by this program. Additional control strings are shown below.

c***********************************************************************

c CONTROL STRINGS:

c PLOTTER     PLOT_DEVICE PLOT_ORIENTATION
c MAP-AREA    MAP-ID RECTANGULAR   lat1 lon1 lat2 lon2 size-x size-y
c MAP-AREA    MAP-ID MERCATOR      lat1 lon1 lat2 lon2 size-x size-y
c MAP-AREA    MAP-ID GNOMONIC      lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID AZIMUTHAL     lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID ORTHOGRAPHIC  lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID STEREOGRAPHIC lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID MAP-AREA-SPECIFICATION-FILE
c MAP-TYPE    LAND
c MAP-TYPE    COAST
c MAP-TYPE    LAND COAST
c MAP-TYPE    COAST LAND

c***********************************************************************

c PLOTTER           Defines the graphical output device

c    INPUT PARAMETERS:

c    PLOT_DEVICE    plotter device (SYS/SYS-PRN/FILE)

c                   If FILE, then PLOT-ORIENTATION is output file name


c MAP-AREA          Defines the map onto which the path segmentation is
c                   to be plotted for the "PREVIEW".

c    INPUT PARAMETERS:

c    MAP-ID         map area name [20 characters]

c    MERCATOR       Mercator projection

c    lat1           bottom latitude  of the map area
c    lon1           left   longitude of the map area
c    lat2           top    latitude  of the map area
c    lon2           right  longitude of the map area

c    RECTANGULAR    linear in latitude and longitude

c    lat1           bottom latitude  of the map area
c    lon1           left   longitude of the map area
c    lat2           top    latitude  of the map area
c    lon2           right  longitude of the map area

c    AZIMUTHAL      azimuthal-equidistant

c    lat0           center latitude  of the map area
c    lon0           center longitude of the map area
c    range          maximum range to be mapped

c    GNOMONIC       great circles are straight lines

c    lat0           center latitude  of the map area
c    lon0           center longitude of the map area
c    range          maximum range to be mapped

c    ORTHOGRAPHIC   orthographic

c    lat0           center latitude  of the map area
c    lon0           center longitude of the map area
c    range          maximum range to be mapped

c    size-x         length of horizontal axis
c    size-y         length of vertical   axis

c    STEREOGRAPHIC  stereographic

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


c    MAP-AREA-SPECIFICATION-FILE
c                   name of a file which contains records containing
c                   the above list of parameters for the map area.
c                   the file is searched for a match with MAP-ID and
c                   the corresponding parameters are returned.


c MAP-TYPE          Indicates the method of denoting land masses for the
c                   map plots.

c    INPUT PARAMETERS:

c    LAND           does land mass overlay from LWPC_DAT:LAND$D
c    COAST          does coastal outlines  from LWPC_DAT:COAST$D


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
c    frequency       kHz
c    power           kW
c    altitude        km
c    distance        km
c    latitude        degrees North
c    longitude       degrees West
c    inclination     degrees from vertical
c    heading         degrees East of North (geographic)
c    bearing         degrees East of North (geographic)

c Conventions:
c    Southern latitudes are input as negative values
c    Eastern longitudes are input as negative values
c    Dates are input as MONTH/day/year
c    Times are input as hour:minute
c    Antennae are modeled as short dipoles

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

c*******************!***************************************************

c     LWPC parameters
      include      'sysStrct.cmn'
      include      'lwpc_cfg.cmn'
      include      'lwpc_lun.cmn'

      include      'lwpc_lun.ini'

      parameter    (mxpath=120,
     &              mxeigen=50,mxsgmnt=201)

c     Variables for preview plot
      character*  4 map_type(2),map_prjctn,n_model
      character*  8 archive,prgm_id
      character* 12 plt_orientation
      character* 20 null,run_name,
     &              xmtr_id,path_id,area_id,map_id
      character* 40 prfl_id
      character* 80 case_id
      character* 96 plt_device
      character*120 file_id,
     &              root_file,
     &              file_name
      logical       more_data,
     &              gcpath,lwflds,opa_grid,preseg

      integer       str_length,
     &              bflag,pflag,pindex,year,day,UT,
     &              autos,rprnt,xprnt,rpoly,
     &              print_swg,print_lwf,print_mc,print_wf

      real          lat,lon,lub,incl,
     &              map_lat(2),map_lon(2),map_size(2),
     &              lwf_dst_min,lwf_dst_max,lwf_dst_inc

      complex       eigen,tp,param2

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

     &      /mf_area/
     &              gmax,bxrtio,tmesh,autos
     &      /mf_flag/
     &              iexact,iovflo,kexact,lowg,nodivd,noevct,nofinl,
     &              nointg,nomesh,notlin
     &      /mf_hgts/
     &              rtol,xintrp,htntrp,d,z0,zz
     &      /mf_prnt/
     &              lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

     &      /mf_sw_1/
     &              dst(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt

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
     &              tp(mxeigen),param2(5,mxeigen),lu_mds

      equivalence  (oplat1,op_lat(1)),(oplon1,op_lon(1)),
     &             (oplat2,op_lat(2)),(oplon2,op_lon(2))

c     LWPC common
      data          prgm_id/'PRVW 1.0'/,
     &              case_id/'        '/,
     &              prfl_id/'        '/,
     &              xmtr_id/'        '/,
     &              freq/0./,tlat/0./,tlon/0./,range_max/20000./,
     &              rlat/0./,rlon/0./,rrho/0./,bearing/0./,pflag/2/,
     &              ranger/60.,90./,rangei/0.,-5./,atnmax/50./,h/50./,
     &              select_hgts/1.e2,1.e-4/,
     &              archive/'***'/
c     MF Common
     &              autos/1/,bxrtio/1./,iexact/0/,
     &              rtol/0.003/,xintrp/0.8/,
     &              lprnt,lgprnt,mprnt,mrprnt/4*0/,
     &              msprnt,mzprnt,rprnt,xprnt/4*0/,
c     SW Common
     &              drmin/125./,drmax/500./,mdir/0/,print_swg/0/,
     &              ftol/1./,mxiter/15/,alpha/3.14e-4/,prec/2./,
     &              rpoly/1/,nrtlst/5/,
c     Miscellaneous
     &              gcpath/.false./,lwflds/.false./,opa_grid/.false./,
     &              preseg/.false./,
     &              n_model/'ntia'/,
     &              n_month,n_day,n_year,n_UT/4*0/,bandw/1000./,
     &              nrcmp/1/,power/1./,incl,headng,talt,ralt/4*0./,
     &              month/07/,day/15/,year/1984/,UT/1615/,
     &              print_lwf/0/,

     &              brsltn/15.,3./,
     &              lwf_dst_min,lwf_dst_max,lwf_dst_inc/0.,20000.,20./,

     &              null/z0/

c     Get the location of the LWPC data
      call LWPC_DAT_LOC

c     Test for input file
      if (ArgList(1) .eq. null) then
         run_name='prvwPlot'
      else
         run_name=ArgList(1)
      end if
      nf=STR_LENGTH(run_name)

      file_name=run_name(:nf)//'.inp'
      OPEN (lwpcINP_lun,file=file_name,status='old')

      file_name=run_name(:nf)//'.log'
      OPEN (lwpcLOG_lun,file=file_name,status='unknown')

c     Initialize character strings
      xmtr_id='    '
      path_id='    '
      area_id='    '
      do i=1,120,8
         root_file(i:i+7)='        '
      end do

c     Initialize logical units
      lu_prf=lwpcPRF_lun
      lu_mds=lwpcMDS_lun

      more_data=.true.
      do while (more_data)

         call LWP_INPUT
     &       (more_data,root_file,
     &        plt_device,plt_orientation,
     &        gcpath,lwflds,opa_grid,preseg,
     &        power,incl,headng,talt,ralt,
     &        print_lwf,print_mc,print_wf,nrcmp,
     &        lwf_dst_min,lwf_dst_max,lwf_dst_inc,
     &        map_id,map_prjctn,map_type,
     &        map_lat,map_lon,map_size,
     &        bflag,area_id,range_max,
     &        n_model,n_month,n_day,n_year,n_UT,bandw,
     &        month,day,year,UT)

         if (more_data) then

c           Get bearing angles
            rng_mx=range_max
            call LWP_SET_BRNG
     &          (tlat,tlon,bflag,op_lat,op_lon,brsltn,
     &           mxpath,nrpath,path_bearing,path_range,
     &           rxlat,rxlon,rxrho,rng_mx)

c           Loop over path bearings
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

               call LWP_PREVIEW
     &             (plt_device,plt_orientation,
     &              map_type,map_prjctn,
     &              map_lat,map_lon,map_size,
     &              op_lat,op_lon,
     &              pflag,year,month,day,UT,
     &              tlat,tlon,bearing,nb,
     &              dst,mxsgmnt,nrsgmnt)
            end do
         end if
      end do

      call GRF_DONE
      call CLOSE_FORDRIVE

      END      ! prvwPlot
      SUBROUTINE LWP_PREVIEW
     &          (plt_device,plt_orientation,
     &           map_type,map_projection,map_lat,map_lon,
     &           map_size,opa_lat,opa_lon,
     &           pflag,year,month_num,day,UT,
     &           Tx_lat,Tx_lon,bearng,nbrng,
     &           dist,mxdist,nrdist)

c***********************************************************************

c     Plots path segmentation data onto a map

c     Sign convention is + for N/W and - for S/E

c Parameters:

c     plt_device    plot device (SYS-SRC/SYS-PRN/FILE-NAME)
c     plt_orientation
c                   orientation (LANDSCAPE/PORTRAIT)

c     map_type      land/sea map type
c                   ='    ':  no map
c                   ='land':  use lwpcDAT_loc\LAND$D
c                   ='cond':  use lwpcDAT_loc\COND$D
c                   ='coas':  use lwpcDAT_loc\COAST$D

c     map_projection
c                   =mercator       Mercator
c                   =rectangular    linear in longitude and latitude
c                   =azimuthal      azimuthal-equidistant
c                   =gnomonic       great circles are straight lines
c                   =orthographic   orthographic projection
c                   =stereographic  stereographic projection

c     map_lat       latitude  bounds; deg North
c     map_lon       longitude bounds; deg West
c     map_size      lengths of map axes; inches

c     opa_lat       latitude  bounds of op area; deg North
c     opa_lon       longitude bounds of op area; deg West

c     pflag         ionosphere flag; a value of 3 indicates that a
c                   specific date has been input
c     year          year
c     month_num     number of the month
c     day           day of the month
c     UT            UT in hhmm format

c     Tx_lat        transmitter latitude;  deg North
c     Tx_lon        transmitter longitude; deg West

c     bearng        path bearing; deg East of North
c     nbrng         index of path bearing

c     dist          array of distances; km
c     mxdist        dimension of array  DIST
c     nrdist        number of points in DIST

c***********************************************************************

c Change history:
c     11 Nov 1993   Dropped PLT_UNIT for conversion to OS/2.

c     30 Jun 1994   Dropped BFLAG from argument list.

c     12 Jul 1994   Modified to use new WORLD_MAP.

c     04 Sep 1994   Modified to use new WORLD_MAP; dropped AZIM_MAXD
c                   from argument list.

c     05 Mar 1995   Modified to use new geophysics routines.

c     16 Apr 1995   Modified to use new geophysics routines
c                   with colors taken from the GRAPHICS.INI file.

c     08 Apr 1997   Modified to use date and time to show the
c                   terminator.

c     15 Jan 98     Dropped PLT_UNIT; use PLT_DEVICE to store file name
c                   when output to a file.

c*******************!***************************************************

c     LWPC map data
      include      'lwpc_geo.cmn'

      character*(*) plt_device,plt_orientation,
     &              map_type(2),map_projection
      character*  8 path_color
      logical       map_grid,
     &              inside,nxtpnt,nxtsym
      integer       pflag,year,day,UT,
     &              map_symbol(3)
      real          map_lat(2),map_lon(2),map_size(2),
     &              map_tics(2),lon,lat

      dimension     opa_lat(2),opa_lon(2),dist(mxdist)

      data          map_grid/.false./,
     &              map_tics/10.,30./,
     &              dtr/.01745329252/,
     &              path_color/'color1'/,
     &              map_symbol/3,0,11/

c     Terminator parameters
      character*  3 month_nam
      logical       term_label,
     &              sub_solar
      integer       term_line
      real          chi(2)

      data          term_label/.true./,
     &              term_line/0/,
     &              sub_solar/.false./,
     &              chi/90.,99./


      if (nbrng .eq. 1) then

c        This is the first path; set up map
         call GRF_BEGIN (plt_device,plt_orientation)
         call GRF_ORIGIN (.4,.6)

         call GEO_DEFS (map_projection,
     &                  map_lat,map_lon,map_size)

         call WORLD_MAP (map_type,map_grid,map_tics)

         call GEO_OPA_BORDER (opa_lat,opa_lon)

         if (pflag .eq. 4) then

c           Get month name
            call MONTH_NAME (month_num,month_nam)

c           Convert UT in HHMM format to hours
            UT_hours=(UT-INT(UT/100.)*40.)/60.

            call GEO_TERMINATOR (term_label,term_line,sub_solar,
     &                           month_nam,day,year,UT_hours,chi)
         end if

         call GRF_COLOR (path_color)

         tclt=(90.-Tx_lat)*dtr
         ctclt=COS(tclt)
         stclt=SIN(tclt)
         tlng=Tx_lon*dtr

         lat=Tx_lat
         lon=Tx_lon

         call GEO_COORD (lat,lon,
     &                   geo_rng,geo_brng,geo_x,geo_y,
     &                   plt_x,plt_y)

         SELECT CASE( geo_prjctn )

         CASE( 'A','G','O','S' )

            if (geo_rng .gt. geo_rng_max .or.
     &          plt_x .lt. 0. .or. plt_x .gt. geo_size_x .or.
     &          plt_y .lt. 0. .or. plt_y .gt. geo_size_y) then

               inside=.false.
            else

               inside=.true.
            end if

         CASE( 'M','R' )

            if (plt_x .lt. 0. .or. plt_x .gt. geo_size_x .or.
     &          plt_y .lt. 0. .or. plt_y .gt. geo_size_y) then

               inside=.false.
            else

               inside=.true.
            end if

         END SELECT

         if (inside) then

            call GRF_SYMBOL (plt_x,plt_y,.02,map_symbol(1),0.,-1)
         end if
      end if

c     Set plot position at the transmitter
      call GEO_COORD (Tx_lat,Tx_lon,
     &                geo_rng,geo_brng,geo_x,geo_y,
     &                plt_x,plt_y)

      call GRF_MOVE (plt_x,plt_y)

c     Plot propagation path
      brng=bearng*dtr

      nr=2
      dst=0.
      prev_x=999.
      prev_y=999.
      do while (nr .le. nrdist)

         nxtsym=.false.
         nxtpnt=.false.
         do while (.not.nxtpnt)

            dst=dst+1.
            if (ABS(dst-dist(nr)) .le. 1.) then

               rho=dist(nr)/6366.
               nr=nr+1
               nxtpnt=.true.
               nxtsym=.true.
            else
     &      if (MOD(dst,50.) .eq. 0.) then

               rho=dst/6366.
               nxtpnt=.true.
               nxtsym=.false.
            end if
         end do

         call RECVR2 (tlng,tclt,ctclt,stclt,brng,rho,
     &                rlng,rclt,crclt,srclt)

         lat=90.-rclt/dtr
         lon=rlng/dtr

         call GEO_COORD (lat,lon,
     &                   geo_rng,geo_brng,geo_x,geo_y,
     &                   plt_x,plt_y)

         SELECT CASE( geo_prjctn )

         CASE( 'A','G','O','S' )

            if (geo_rng .gt. geo_rng_max .or.
     &          plt_x .lt. 0. .or. plt_x .gt. geo_size_x .or.
     &          plt_y .lt. 0. .or. plt_y .gt. geo_size_y) then

               inside=.false.
            else

               inside=.true.
            end if

         CASE( 'M','R' )

            if (plt_x .lt. 0. .or. plt_x .gt. geo_size_x .or.
     &          plt_y .lt. 0. .or. plt_y .gt. geo_size_y .or.

     &          ABS(plt_x-prev_x) .gt. 1. .or.
     &          ABS(plt_y-prev_y) .gt. 1.) then

               inside=.false.
            else

               inside=.true.
            end if

         END SELECT

         if (inside) then

            call GRF_DRAW (plt_x,plt_y)

            if (dst .gt. 200. .and. nxtsym)

     &      call GRF_SYMBOL (plt_x,plt_y,.02,map_symbol(2),0.,-1)
         else

            call GRF_MOVE (plt_x,plt_y)
         end if

         prev_x=plt_x
         prev_y=plt_y
      end do

      call GRF_COLOR ('labels')

      RETURN
      END      ! LWP_PREVIEW
