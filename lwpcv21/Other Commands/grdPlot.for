      SUBROUTINE FORDRIVE

c     PROGRAM    grdPlot

c***********************************************************************

c Draws contours in a geographic area using GRD files.

c***********************************************************************

c CONTROL STRINGS:

c PLOTTER     PLOT-DEVICE PLOT-ORIENTATION
c FILE-GRD    directory-location-of-GRD-data-files
c FILE-TYPE   ASCII
c FILE-TYPE   BINARY
c PLOT-LBL    PLOT_LBL
c OP-AREA     AREA_ID oplat1 oplon1 oplat2 oplon2
c OP-AREA     AREA_ID OP-AREA-SPECIFICATION-FILE
c MAP-AREA    MAP-ID RECTANGULAR   lat1 lon1 lat2 lon2 size-x size-y
c MAP-AREA    MAP-ID MERCATOR      lat1 lon1 lat2 lon2 size-x size-y
c MAP-AREA    MAP-ID GNOMONIC      lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID AZIMUTHAL     lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID ORTHOGRAPHIC  lat0 lon0 range     size-x size-y
c MAP-AREA    MAP-ID STEREOGRAPHIC lat0 lon0 range     size-x size-y
c MAP-AREA    MAP_ID MAP-AREA-SPECIFICATION-FILE
c MAP-GRID    GRID minor-tic major-tic
c MAP-TYPE    COAST
c MAP-TYPE    LAND
c MAP-TYPE    LAND COAST
c MAP-TYPE    COAST LAND
c TX          TX-DATA-FILE
c TX-NTR      TX-DATA-FILE
c JX          JX-DATA-FILE
c JX-NJR      JX-DATA-FILE
c RX-DATA     VERTICAL
c RX-DATA     HORIZONTAL
c RX-MODEL    MITRE
c RX-MODEL    PSR
c A-NOISE     N_MODEL DATE TIME bandw
c P-NOISE     mean-platform-noise
c MODIFY-PWR  power
c CNTR-LEVELS cntr-lvl-1  cntr-lvl-2  ...
c CNTR-RANGE  cntr-lvl-mn cntr-lvl-mx cntr-lvl-in
c TA-LEVELS   ta-lvl-1 ta-lvl-2 ...
c THRESHOLDS  threshold-1 threshold-2 ...
c TERMINATOR  chi1 chi2
c BRIEF-LABELS
c DEPTH       depth ANTENNA
c PLT-J       jplot
c PLT-J/N     jnplot
c PLT-N       nplot
c PLT-S       splot
c PLT-S/I     siplot
c PLT-S/J     sjplot
c PLT-S/N     snplot
c START

c INPUT PARAMETERS:

c PLOT_DEVICE plotter device (SYS-SCR/SYS-PRN/FILE-NAME)

c plot_lbl    character string to be used to label each plot

c area_id     op area name [20 characters]

c oplat1      lower left  coordinates of the op area
c oplon1
c oplat2      upper right coordinates of the op area
c oplon2

c map_id      map area name [20 characters]

c    Map area parameters depend on the projection.

c    MERCATOR     Mercator projection

c    lat1         bottom latitude  of the map area
c    lon1         left   longitude of the map area
c    lat2         top    latitude  of the map area
c    lon2         right  longitude of the map area

c    RECTANGULAR  linear in latitude and longitude

c    lat1         bottom latitude  of the map area
c    lon1         left   longitude of the map area
c    lat2         top    latitude  of the map area
c    lon2         right  longitude of the map area

c    AZIMUTHAL    azimuthal-equidistant projection

c    lat0         center latitude  of the map area
c    lon0         center longitude of the map area
c    range        maximum range to be mapped

c    GNOMONIC     projection in which great circles are straight lines

c    lat0         center latitude  of the map area
c    lon0         center longitude of the map area
c    range        maximum range to be mapped

c    ORTHOGRAPHIC orthographic projection

c    lat0         center latitude  of the map area
c    lon0         center longitude of the map area
c    range        maximum range to be mapped

c    STEREOGRAPHIC  Stereographic projection

c    lat0           center latitude  of the map area
c    lon0           center longitude of the map area
c    range          maximum range to be mapped

c    size-x         length of horizontal axis
c    size-y         length of vertical   axis

c size-x      length of horizontal axis
c size-y      length of vertical   axis

c map-type    indicates method of denoting land masses

c             COAST does land mass overlay from LWPC_DIR:COAST$D
c             LAND  does coastal outlines  from LWPC_DIR:COAST$D

c cntr-lvl-i  specific contour level in dB [max of 7]

c cntr-lvl-mn minimum contour level level in dB
c cntr-lvl-mx maximum contour level level in dB
c cntr-lvl-in increment between contour levels in dB

c             NOTE: If either cntr-lvl-min or cntr-lvl-max is -99,
c                   then its value is determined from the grid data.

c ta_lvl      per-cent time availability for signal contours [max of 7]

c power       value of transmitter power in kW to be used in place of
c             the one used to do the mode sum (LWFLDS)

c threshold   signal, SNR, etc. threshold for time availability contours
c             [max of 7]

c             NOTE: If thresholds are specified, then time availibility
c                   contours are automatically selected.

c jplot       flag to plot signal  contours for this jamr
c jnplot      flag to plot j/n     contours
c nplot       flag to plot noise   contours for this frequency
c splot       flag to plot signal  contours for this xmtr
c siplot      flag to plot s/(n+j) contours
c sjplot      flag to plot s/j     contours
c snplot      flag to plot s/n     contours for this xmtr

c             NOTE: Each of JPLOT, JNPLOT, SPLOT, SIPLOT, SJPLOT and
c                   SNPLOT is coded to contain a list of combinations
c                   to be used; for example, to use jammers 1, 3 and 6,
c                   enter SJPLOT as 136

c N_MODEL     noise model name [4 characters: ITSN, NTIA]

c DATE        date for atmospheric noise in the form "MONTH/day/year"

c TIME        Universal Time in the form "hour:minute"

c bandw       atmospheric noise bandwidth in Hz

c depth       antenna depth in feet

c ANTENNA     antenna name (BRA-34, BRA-34-Mod, OE-315, AT-317F)

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
c     17 Jan 94     Dropped FAT compatibility in GRD file names.

c     21 Jan 94     Changed file naming convention for noise grids to
c                   be the signal grid name with extension equal to the
c                   first 3 characters of the noise model name.

c     29 Jan 94     Added ASCII/BINARY option for grid files.

c     09 Feb 94     Changed PLT_ORIENTATION to CHARACTER*80 to handle
c                   longer output file names when PLT_DEVICE is FILE.

c     30 Jun 94     Dropped NTLIST and NTL from the call to PLT_CNTR_N;
c                   dropped AREA_ID from call to PLT_READ_SIGNAL;
c                   dropped PRGM_ID, RUN_DATE and PLT_UNIT from call to
c                   PLT_CNTR.

c     28 Sep 94     Made this compatible with new Contour routines;
c                   dropped PRGM_ID, RUN_DATE and PLT_UNIT from call to
c                   PLT_EXCD.

c                   The CNTR-LEVELS and TA-LEVELS operators now use an
c                   upper and lower bound for the filled contour to be
c                   plotted, i.e., CNTR-LEVELS 30 6.5 -1.5 -6.5 will
c                   generate contours from 30 to 6.5, 6.5 to -1.5 and
c                   -1.5 to -6.5 on a single plot.

c     06 Mar 95     Modifications for new geophysics routines.

c     20 Apr 95     Modified for new geophysics routines.

c     07 Jun 95     Added terminator display and brief labels; moved
c                   label generation and mapping to the main routine
c                   from PLT_CNTR and PLT_EXCD.

c     25 Oct 95     Changed to use lwpc_LUN.cmn and lwpc_LUN.ini to set
c                   logical units.

c     30 Jan 96     Added V sub d grid array.

c     31 Jan 96     Added receiver models.

c     01 Feb 96     Moved calculation of the contour data array from
c                   PLT_CNTR and PLT_EXCD; added set up of contour
c                   levels to display to be in an array; added call to
c                   PLT_CNTR_RANGE.

c     05 Feb 96     Added call to routine to interpolate coarse LNP grid
c                   to that specified by the signal grid.

c     24 Feb 96     Moved cells,mxcells,nrpts,mxholes,xwork,ywork,mxwork
c                   from argument list to new common block grf$cntr.

c     11 Apr 96     Added FILE-TYPE control string to indicate format
c                   of the GRD files (ASCII or BINARY).

c     17 Apr 96     Modified to use new PLT_AREA_SIZE routine; dropped
c                   SEAWATER and COS_LAT.

c     16 Dec 96     Generalize delimiters used with file names.

c     24 Jun 97     Moved common blocks of code from grdPlot to the
c                   contour routines PLT_CNTR and PLT_EXCD.

c     01 Jul 97     Added depth correction and revised calls to receiver
c                   model.

c     15 Jan 98     Dropped PLT_UNIT; use PLT_DEVICE to store file name
c                   when output to a file.

c     05 Feb 98     Removed control string FILE-DAT.

c     15 Aug 98     Added C-Binary format;
c                   added noise data directory lwpcNOI_loc

c*******************!***************************************************

      parameter    (mxprm=21,mxpath=120,
     &              mxlat=74,mxlon=146,
     &              mxlvl=101)

c     LWPC parameters
      include      'sysStrct.cmn'
      include      'lwpc_cfg.cmn'
      include      'lwpc_lun.cmn'

      include      'lwpc_lun.ini'

c     Variables for grdPlot
      character*  1 delimiter(3)
      character*  1 zyx,N_S,E_W
      character*  3 n_mnth_nam
      character*  4 map_type(2),map_prjctn,n_model,
     &              grid_status
      character*  5 rx_model
      character*  8 grd_format
      character* 10 antenna,antenna_list(4)
      character* 12 plt_orientation
      character* 20 null,run_name,
     &              xmtr_id,area_id,map_id,
     &              string_list(10),
     &              graf_lbl,
     &              cntr_lbl(mxlvl)
      character* 28 xmtr_lbl(-1:6)
      character* 40 prfl_id,
     &              prfl_lbl(-1:6),data_lbl(-1:6)
      character* 48 rcvr_lbl(-1:1)
      character* 80 plot_lbl,tx_grd(6),tx_dir(6)
      character* 96 plt_device
      character*120 file_id(3),
     &              bcd,control_string,data_string,
     &              directory,extension,file_name,
     &              file_lbl(-1:6)
      character*200 error_msg
      logical       exists,
     &              noise,
     &              map_grid,
     &              brief,
     &              terminator,
     &              chi_label,
     &              use_rx_model,
     &              use_antenna,
     &              use_pnoise
      integer       str_length,
     &              rcomp,
     &              jplot(5),jnplot(5),
     &              splot(5),snplot(5),siplot(15),sjplot(15)
      real          freq,tlat,tlon,
     &              opa_lat(2),opa_lon(2),
     &              grd_lat(2),grd_lon(2),
     &              map_lat(2),map_lon(2),map_size(2),map_tics(2),
     &              bearing(mxpath),rhomax(mxpath),
     &              rxlat(mxpath),rxlon(mxpath),
     &              cntr_lvl(mxlvl),

     &              param(mxprm),
     &              tpwr,incl,headng,talt,ralt,power

      dimension     cntr_level(7),cntr_range(3),
     &              plevel(7),threshold(7),chi_list(2),
     &              ntlist(5),power_mod(5),

     &              antenna_spec(6,0:4),

     &              ampt(mxlon,mxlat,2,5),stdt(mxlon,mxlat,2,5),
     &              ampn(mxlon,mxlat,2  ),stdn(mxlon,mxlat,2  ),
     &              vsdn(mxlon,mxlat,2  ),
     &              amp1(mxlon,mxlat    ),std1(mxlon,mxlat    ),
     &              amp2(mxlon,mxlat    ),std2(mxlon,mxlat    ),
     &              ampl(mxlon,mxlat    )

      data          plt_device/'sys-scn'/,
     &              plt_orientation/'landscape'/,

     &              grd_format/'ASCII'/,

     &              lwpcGRD_loc/' '/,
     &              lwpcNOI_loc/' '/,

     &              map_type/2*'    '/,

     &              map_grid/.true./,map_tics/10.,30./,

     &              rcomp/1/,

     &              noise/.false./,
     &              n_model/'ntia'/,
     &              n_mnth_num,n_day,n_year,n_UT/4*0/,bandw/1000./,

     &              rx_model/z0/,

     &              nclvl/0/,cntr_range/-60.,-99.,3./
     &              nplvl/1/,plevel/50.,90.,99.,4*0./,
     &              ntlvl/0/,threshold/7*0./,power/0./,power_mod/5*0./,

     &              jplot/5*0/,jnplot/5*0/,nplot/0/,pnoise/-1./,
     &              splot/5*0/,snplot/5*0/,siplot/15*0/,sjplot/15*0/,

     &              brief/.false./,
     &              terminator/.false./,chi_list/90.,180./,
     &              chi_max,chi_min,chi_inc/96.,90.,2./,
     &              chi_label,chi_line/.false.,0/,

     &              antenna/z0/,depth/0./,

     &              antenna_list/'BRA-34','BRA-34-Mod',
     &                           'OE-315','AT-317F'/,
     &              antenna_spec/10.0,20.0,30.0,40.0,50.0,60.0,
     &                           29.2,23.2,18.7,18.6,17.1,15.8,
     &                           27.2,19.5,18.4,16.2,15.9,15.7,
     &                           11.5, 9.6, 6.7, 4.0, 2.9, 1.3,
     &                           20.2,14.5, 9.5, 9.8, 8.2, 6.7/,

     &              null/z0/

c     The parameter next_graph comes from sysStrct.cmn
c
c     next_graph=0 First graph
c                1 Display next graph
c                2 Re-draw current graph
c                3 Display all remaining graphs
c                4 Print current graph
c                5 Option button 1
c                6 Option button 2
c
c     The buttons on the graphics window only behave as
c     implied when cases which produce single graphs.

      if (next_graph .gt. 0) go to 9

c     Get the location of the LWPC data
      call LWPC_DAT_LOC

c     Get delimiters used in the file names
      call GET_DELIMITER (delimiter)

c     Test for input file
      if (ArgList(1) .eq. null) then
         run_name='grdPlot'
      else
         run_name=ArgList(1)
      end if
      nf=STR_LENGTH(run_name)

      file_name=run_name(:nf)//'.inp'
      OPEN (lwpcINP_lun,file=file_name,status='old')

      file_name=run_name(:nf)//'.log'
      OPEN (lwpcLOG_lun,file=file_name,status='unknown')

c     Initialize logical units
      lu_grd=lwpcGRD_lun

      if (plt_device(:5) .eq. 'sys-p' .or.
     &    plt_device(:5) .eq. 'sys_p') next_graph=3

9     if (next_graph .eq. 2) go to 51
      if (next_graph .eq. 0 .or.
     &    next_graph .eq. 1 .or.
     &    next_graph .eq. 3) go to 11
c     Option buttons not yet enabled; do nothing.
      if (next_graph .eq. 5 .or.
     &    next_graph .eq. 6) RETURN
c     Must be button 4
      plt_device='sys-prn'
      go to 51

c     Read data defining op area and transmitters to be considered
11    read (lwpcINP_lun,'(a)',end=9999) bcd
      write(lwpcLOG_lun,'(a)') bcd

      if (bcd(1:1) .eq. ' ') go to 11

      call DECODE_CONTROL_DATA (bcd,control_string,data_string)
      call STR_UPPER (control_string,1,STR_LENGTH(control_string))

c QUIT
      if (control_string(1:1) .eq. 'Q') go to 9999

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
         go to 11
      end if

c FILE-NOI
      if (control_string(1:8) .eq. 'FILE-NOI') then
         if (data_string(1:1) .eq. ' ') then
c           Current directory is default
            lwpcNOI_loc=' '
         else
            lwpcNOI_loc=data_string(1:STR_LENGTH(data_string))
            n=STR_LENGTH(lwpcNOI_loc)
            if (lwpcNOI_loc(n:n) .ne. delimiter(3)) then
               n=n+1
               lwpcNOI_loc(n:n)=delimiter(3)
            end if
         end if
         go to 11
      end if

c FILE-TYPE
      if (control_string(1:8) .eq. 'FILE-TYP') then

         grd_format=data_string(1:STR_LENGTH(data_string))
         go to 11
      end if

c A-NOISE
      if (control_string(1:1) .eq. 'A') then

         call DECODE_A_NOISE
     &       (data_string,n_model,n_mnth_num,n_day,n_year,n_UT,bandw)
         go to 11
      end if

c BRIEF-LABELS
      if (control_string(1:1) .eq. 'B') then

         brief=.true.
         go to 11
      end if

c CNTR-LEVELS
      if (control_string(1:6) .eq. 'CNTR-L' .or.
     &    control_string(1:6) .eq. 'CNTR_L') then

         call DECODE_LIST_FLT
     &       (data_string,7,nclvl,cntr_level)
         go to 11
      end if

c CNTR-RANGE
      if (control_string(1:6) .eq. 'CNTR-R' .or.
     &    control_string(1:6) .eq. 'CNTR_R') then

         call DECODE_LIST_FLT
     &       (data_string,3,nrlist,cntr_range)
         nclvl=0
         go to 11
      end if

c DEPTH
      if (control_string(1:1) .eq. 'D') then

         call DECODE_LIST_STR
     &       (data_string,2,nrlist,string_list)

c        Get depth
         read (string_list(1),*) depth

c        Get antenna name if specified
         if (nrlist .gt. 1) then

            antenna=string_list(2)
            use_antenna=.true.

            call STR_UPPER (antenna,0,0)
            if (antenna(:6) .eq. 'OE-315') then

c              Floating wire antenna; force depth to zero.
               depth=0.
            end if
         else
            antenna=null
            use_antenna=.false.
         end if
         go to 11
      end if

c JX-NJR
      if (control_string(1:1) .eq. 'J') then

c        Check for jammer index
         k1=INDEX(control_string,'-')
         if (k1 .eq. 0) then
            njr=1
         else
            read (control_string(k1+1:),*) njr
         end if

         call DECODE_FILE_NAME
     &       (data_string,directory,file_name,extension)

         ntr=6-njr
         if (STR_LENGTH(directory) .eq. 0) then
            tx_grd(ntr)=file_name
            tx_dir(ntr)=' '
         else
            tx_grd(ntr)=file_name
            tx_dir(ntr)=directory(:STR_LENGTH(directory))
         end if
         power_mod(ntr)=power
         write(xmtr_lbl(ntr),'(''Jx'',i3,'': '')') njr
         go to 11
      end if

c MAP-AREA
      if (control_string(1:5) .eq. 'MAP-A' .or.
     &    control_string(1:5) .eq. 'MAP_A') then

         call DECODE_MAP_AREA
     &       (data_string,.true.,
     &        map_id,map_prjctn,
     &        map_lon(1),map_lon(2),map_size(1),
     &        map_lat(1),map_lat(2),map_size(2))
         call STR_LOWER (map_id,0,0)
         call STR_LOWER (map_prjctn,0,0)
         go to 11
      end if

c MAP-GRID
      if (control_string(1:5) .eq. 'MAP-G' .or.
     &    control_string(1:5) .eq. 'MAP_G') then

         call DECODE_LIST_STR
     &        (data_string,1,nrlist,grid_status)
         call STR_UPPER (grid_status,0,0)
         if (grid_status(1:2) .eq. 'ON') then
            map_grid=.true.
         else
            map_grid=.false.
         end if
         call STR_COUNT_LIST
     &       (data_string,0,0,nl)
         if (nl .gt. 1) then
c           Get tic mark intervals
            call STR_GET_ITEM
     &          (2,data_string,bcd,n1,n2)
            call DECODE_LIST_FLT
     &          (data_string(n1:),2,nrlist,map_tics)
         end if
         go to 11
      end if

c MAP-TYPE
      if (control_string(1:5) .eq. 'MAP-T' .or.
     &    control_string(1:5) .eq. 'MAP_T') then

         call DECODE_LIST_STR
     &       (data_string,2,nrlist,map_type)
         do n=1,nrlist
            call STR_LOWER (map_type(n),0,0)
         end do
         go to 11
      end if

c MODIFY-PWR
      if (control_string(1:2) .eq. 'MO') then

         call DECODE_LIST_FLT
     &       (data_string,1,nrlist,power)
         go to 11
      end if

c OP-AREA
      if (control_string(1:2) .eq. 'OP') then

         call DECODE_OP_AREA
     &       (data_string,.true.,
     &        area_id,opa_lon(1),opa_lon(2),opa_lat(1),opa_lat(2))
         call STR_LOWER (area_id,0,0)
         go to 11
      end if

c P-NOISE
      if (control_string(1:2) .eq. 'P-' .or.
     &    control_string(1:2) .eq. 'P_') then

         call DECODE_LIST_FLT
     &       (data_string,1,nrlist,pnoise)

         if (pnoise .eq. -1.) then
            use_pnoise=.false.
         else
            use_pnoise=.true.
         end if
         go to 11
      end if

c PLOT-LBL
      if (control_string(1:5) .eq. 'PLOT-' .or.
     &    control_string(1:5) .eq. 'PLOT_') then

         plot_lbl=data_string
         go to 11
      end if

c PLOTTER
      if (control_string(1:5) .eq. 'PLOTT') then

         call DECODE_PLOTTER
     &       (data_string,plt_device,plt_orientation)
         call STR_LOWER (plt_device,0,0)
         go to 11
      end if

c PLT-J/N
      if (control_string(1:7) .eq. 'PLT-J/N' .or.
     &    control_string(1:7) .eq. 'PLT_J/N') then

c        Initialize the list
         do i=1,5
            jnplot(i)=0
         end do

c        Read the list
         call DECODE_LIST_INT
     &       (data_string,5,nl,jnplot)

         noise=.true.
         go to 11
      end if

c PLT-J
      if (control_string(1:5) .eq. 'PLT-J' .or.
     &    control_string(1:5) .eq. 'PLT_J') then

c        Initialize the list
         do i=1,5
            jplot(i)=0
         end do

c        Read the list
         call DECODE_LIST_INT
     &       (data_string,5,nl,jplot)
         go to 11
      end if

c PLT-N
      if (control_string(1:5) .eq. 'PLT-N' .or.
     &    control_string(1:5) .eq. 'PLT_N') then

c        Read the list
         call DECODE_LIST_INT
     &       (data_string,1,nl,nplot)

         noise=.true.
         go to 11
      end if

c PLT-S/I
      if (control_string(1:7) .eq. 'PLT-S/I' .or.
     &    control_string(1:7) .eq. 'PLT_S/I') then

c        Initialize the list
         do i=1,15
            siplot(i)=0
         end do

c        Read the list
         call DECODE_LIST_INT
     &       (data_string,15,nl,siplot)

         noise=.true.
         go to 11
      end if

c PLT-S/J
      if (control_string(1:7) .eq. 'PLT-S/J' .or.
     &    control_string(1:7) .eq. 'PLT_S/J') then

c        Initialize the list
         do i=1,15
            sjplot(i)=0
         end do

c        Read the list
         call DECODE_LIST_INT
     &       (data_string,15,nl,sjplot)
         go to 11
      end if

c PLT-S/N
      if (control_string(1:7) .eq. 'PLT-S/N' .or.
     &    control_string(1:7) .eq. 'PLT_S/N') then

c        Initialize the list
         do i=1,5
            snplot(i)=0
         end do

c        Read the list
         call DECODE_LIST_INT
     &       (data_string,5,nl,snplot)

         noise=.true.
         go to 11
      end if

c PLT-S
      if (control_string(1:5) .eq. 'PLT-S' .or.
     &    control_string(1:5) .eq. 'PLT_S') then

c        Initialize the list
         do i=1,5
            splot(i)=0
         end do

c        Read the list
         call DECODE_LIST_INT
     &       (data_string,5,nl,splot)
         go to 11
      end if

c RX-DATA
      if (control_string(1:4) .eq. 'RX-D' .or.
     &    control_string(1:4) .eq. 'RX_D') then

         call DECODE_RX_DATA
     &       (data_string,zyx,rcomp,ralt)
         go to 11
      end if

c RX-MODEL
      if (control_string(1:4) .eq. 'RX-M' .or.
     &    control_string(1:4) .eq. 'RX_M') then

         if (data_string(1:1) .eq. ' ') then

c           No receiver model
            rx_model=null
            use_rx_model=.false.
         else

            rx_model=data_string
            use_rx_model=.true.
            call STR_UPPER (rx_model,0,0)
         end if
         go to 11
      end if

c TA-LEVELS
      if (control_string(1:2) .eq. 'TA') then

         call DECODE_LIST_FLT
     &       (data_string,7,nplvl,plevel)
         go to 11
      end if

c TERMINATOR
      if (control_string(1:2) .eq. 'TE') then

         if (STR_LENGTH(data_string) .eq. 0) then

            chi_list(1)=90.
            chi_list(2)=96.
         else

            call DECODE_LIST_FLT
     &          (data_string,2,nrchi,chi_list)

            if (nrchi .eq. 1)
     &         call LWPC_ERROR
     &             ('Error','Two values of CHI are required')
         end if
         terminator=.true.
         go to 11
      end if

c THRESHOLDS
      if (control_string(1:2) .eq. 'TH') then

         call DECODE_LIST_FLT
     &       (data_string,7,ntlvl,threshold)
         go to 11
      end if

c TX-NTR
      if (control_string(1:2) .eq. 'TX') then

c        Check for transmitter index
         k1=INDEX(control_string,'-')
         if (k1 .eq. 0) then
            ntr=1
         else
            call DECODE_LIST_INT (control_string(k1+1:),1,nt,ntr)
         end if

         call DECODE_FILE_NAME
     &       (data_string,directory,file_name,extension)

         if (STR_LENGTH(directory) .eq. 0) then
            tx_grd(ntr)=file_name
            tx_dir(ntr)=' '
         else
            tx_grd(ntr)=file_name
            tx_dir(ntr)=directory(:STR_LENGTH(directory))
         end if
         power_mod(ntr)=power
         write(xmtr_lbl(ntr),'(''Tx'',i3,'': '')') ntr
         go to 11
      end if

c START
      if (control_string(1:1) .ne. 'S') then

c        Last chance for recognized control string
         call LWPC_ERROR ('Error','Unrecognized control string')
      end if

c     Generate headings for transmitter data label
      xmtr_lbl(-1)='    '
      xmtr_lbl( 0)='    '

      if (brief) then
         data_lbl(-1)='          '
         data_lbl( 0)=' Freq Prad'
      else
         data_lbl(-1)=' Freq  Lat     Lon   Prad In Hdg Alt'
         data_lbl( 0)='  kHz dg:mn   dg:mn    kW dg  dg  km'
      end if

c     Generate headings for ionospheric profile label
      prfl_lbl(-1)='Ionosphere'
      prfl_lbl( 0)='    '

c     Generate headings for file identification label
      file_lbl(-1)='File'
      file_lbl( 0)='    '

c     Generate headings for receiver data label
      if (rcomp .eq. 1) then
         zyx='V'
      else
         zyx='H'
      end if
      if (brief) then
         write(rcvr_lbl(-1),'(a)')
     &        'Rx:  Depth'
         write(rcvr_lbl( 0),'(a)')
     &        '        ft'
         write(rcvr_lbl( 1),'(4x,i6)')
     &         INT(depth)
      else
         write(rcvr_lbl(-1),'(a)')
     &        'Rx: E Alt Bandw Depth'
         write(rcvr_lbl( 0),'(a)')
     &        '       km    Hz    ft'
         write(rcvr_lbl( 1),
     &       '(4x,a1,i4,i6,i6)')
     &         zyx,INT(ralt+.5),INT(bandw),INT(depth)
      end if
      if (use_pnoise) then
         lx=STR_LENGTH(rcvr_lbl(-1))+2
         write(rcvr_lbl(-1)(lx:),'(a)')
     &        '   N0'
         write(rcvr_lbl( 0)(lx:),'(a)')
     &        '   dB'
         write(rcvr_lbl( 1)(lx:),'(f5.1)')
     &         pnoise
      end if
      if (use_rx_model) then
         lx=STR_LENGTH(rcvr_lbl(-1))+2
         write(rcvr_lbl(-1)(lx:),'(a)')
     &        'Model'
         write(rcvr_lbl( 1)(lx:),'(a)')
     &         rx_model
      end if
      if (use_antenna) then
         lx=STR_LENGTH(rcvr_lbl(-1))+2
         write(rcvr_lbl(-1)(lx:),'(a)')
     &        'Antenna'
         write(rcvr_lbl( 1)(lx:),'(a)')
     &         antenna
      end if

c     Begin processing op area grids

c.....LOOP ON TRANSMITTERS

      do nt=1,5

         if (xmtr_lbl(nt)(1:1) .eq. 'T' .or.
     &       xmtr_lbl(nt)(1:1) .eq. 'J') then

c           Formulate GRD file name using the root of the transmitter
c           name and the area identification
            nrchr_a=STR_LENGTH(area_id)
            nrchr_f=STR_LENGTH(tx_grd(nt))

c           Drop FAT compatibility
cxx            nrchr_a=MIN(5,nrchr_a)
cxx            nrchr_f=MIN(3,nrchr_f)

            call STR_LOWER (area_id,1,nrchr_a)
            call STR_UPPER (area_id,1,1)
            call STR_LOWER (tx_grd(nt),1,nrchr_f)

c           Set the full file name
            if (grd_format(1:1) .eq. 'C' .or.
     &          grd_format(1:1) .eq. 'c') then

c              C-Binary files do not use the area id since it is
c              fixed as the world from 180 W to 180 E.
               if (STR_LENGTH(tx_dir(nt)) .eq. 0) then
                  if (STR_LENGTH(lwpcGRD_loc) .eq. 0) then
                     file_name=tx_grd(nt)(:nrchr_f)//'.grd'
                  else
                     nrchar=STR_LENGTH(lwpcGRD_loc)
                     file_name=lwpcGRD_loc(:nrchar)//
     &                         tx_grd(nt)(:nrchr_f)//'.grd'
                  end if
               else
                  file_name=tx_dir(nt)(:STR_LENGTH(tx_dir(nt)))//
     &                      tx_grd(nt)(:nrchr_f)//'.grd'
               end if
            else

c              Standard ASCII and Binary files append the
c              area id to the file name
               if (STR_LENGTH(tx_dir(nt)) .eq. 0) then
                  if (STR_LENGTH(lwpcGRD_loc) .eq. 0) then
                     file_name=tx_grd(nt)(:nrchr_f)//
     &                         area_id(:nrchr_a)//'.grd'
                  else
                     nrchar=STR_LENGTH(lwpcGRD_loc)
                     file_name=lwpcGRD_loc(:nrchar)//
     &                         tx_grd(nt)(:nrchr_f)//
     &                         area_id(:nrchr_a)//'.grd'
                  end if
               else
                  file_name=tx_dir(nt)(:STR_LENGTH(tx_dir(nt)))//
     &                      tx_grd(nt)(:nrchr_f)//
     &                      area_id(:nrchr_a)//'.grd'
               end if
            end if
cxxcDEBUG
cxx            write(lwpcLOG_lun,'(a)') 'Signal file is:'//file_name

            if (xmtr_lbl(nt)(1:1) .eq. 'T') then
               ntr=nt
cxxcDEBUG
cxx               write(lwpcLOG_lun,
cxx     &             '(/''Tx number'',i2,'' is in file '',a)')
cxx     &                  ntr,file_name(:STR_LENGTH(file_name))
            else
               njr=6-nt
cxxcDEBUG
cxx               write(lwpcLOG_lun,
cxx     &             '(/''Jx number'',i2,'' is in file '',a)')
cxx     &                  njr,file_name(:STR_LENGTH(file_name))
            end if

c           Open the GRD file
            if (grd_format(1:1) .eq. 'C' .or.
     &          grd_format(1:1) .eq. 'c') then

               OPEN (lu_grd,file=file_name,
     &                      status='old',form='unformatted',
     &                      access='sequential',recordtype='fixed',
     &                      iostat=iocheck,err=900,action='read')
            else
     &      if (grd_format(1:1) .eq. 'A' .or.
     &          grd_format(1:1) .eq. 'a') then

               OPEN (lu_grd,file=file_name,
     &                      status='old',form='formatted',
     &                      iostat=iocheck,err=900,action='read')
            else

               OPEN (lu_grd,file=file_name,
     &                      status='old',form='unformatted',
     &                      iostat=iocheck,err=900,action='read')
            end if

c           Fill grid for this transmitter
            call PLT_READ_SIGNAL
     &          (lu_grd,
     &           file_id,prfl_id,
     &           xmtr_id,freq,tlat,tlon,tpwr,
     &           incl,headng,talt,ralt,
     &           mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &           mxprm,nrprm,param,
     &           nrcmp,mxlat,nrlat,mxlon,nrlon,
     &           opa_lat(1),opa_lon(1),opa_lat(2),opa_lon(2),
     &           ampt(1,1,1,nt),stdt(1,1,1,nt),
     &           grd_format)

            CLOSE(lu_grd)

            if (power_mod(nt) .gt. 0.) then

c              Adjust the power
               pwr_adj=10.*LOG10(power_mod(nt)/tpwr)
               tpwr=power_mod(nt)
               do i=1,nrlon
                  do j=1,nrlat
                     do k=1,nrcmp
                        ampt(i,j,k,nt)=ampt(i,j,k,nt)+pwr_adj
                     end do
                  end do
               end do
            end if

            if (use_antenna .and. depth .gt. 0.) then

c              Adjust the signal strength for depth.
               depth_factor=.33*SQRT(freq)*depth
               do i=1,nrlon
                  do j=1,nrlat
                     do k=1,nrcmp
                        ampt(i,j,k,nt)=ampt(i,j,k,nt)-depth_factor
                     end do
                  end do
               end do
            end if

c           Generate transmitter data label
            file_lbl(nt)=file_id(3)
            prfl_lbl(nt)=prfl_id
            xmtr_lbl(nt)=xmtr_lbl(nt)(1:7)//xmtr_id

            if (tlat .ge. 0.) then
               N_S='N'
            else
               N_S='S'
            end if
            xlt=ABS(tlat)
            latd=INT(xlt)
            latm=(xlt-latd)*60.

            if (tlon .ge. 0.) then
               E_W='W'
            else
               E_W='E'
            end if
            xln=ABS(tlon)
            lond=INT(xln)
            lonm=(xln-lond)*60.

            if (brief) then
               write(data_lbl(nt),
     &             '(f7.1,i5)')
     &               freq,INT(tpwr)
            else
               write(data_lbl(nt),
     &             '(f7.1,i3,'':'',i2.2,a,i4,'':'',i2.2,a,i5,
     &               i3,i4,i4)')
     &               freq,latd,latm,N_S,lond,lonm,E_W,INT(tpwr),
     &               INT(incl),INT(headng),INT(talt+.5)
            end if
         end if
      end do

c.....END LOOP ON TRANSMITTERS TO BE CONSIDERED

      if (noise) then

         call MONTH_NAME (n_mnth_num,n_mnth_nam)

         if (n_model(:3) .eq. 'lnp') grd_format='ASCII'

         if (STR_LENGTH(lwpcNOI_loc) .eq. 0) then

c           Form noise grid file name from signal grid file name

c           NOTE: Any one of the transmitter directories can be used.
            exists=.false.
            nt=1
            do while (nt .le. 5)

               if (xmtr_lbl(nt)(1:1) .eq. 'T' .or.
     &             xmtr_lbl(nt)(1:1) .eq. 'J') then

                  nrchr_f=STR_LENGTH(tx_grd(nt))
                  call STR_LOWER (tx_grd(nt),1,nrchr_f)

                  if (grd_format(1:1) .eq. 'C' .or.
     &                grd_format(1:1) .eq. 'c') then

c                    C-Binary files do not use the area id since it is
c                    fixed as the world from 180 W to 180 E.
                     if (STR_LENGTH(tx_dir(nt)) .eq. 0) then
                        if (STR_LENGTH(lwpcGRD_loc) .eq. 0) then
                           file_name=tx_grd(nt)(:nrchr_f)//'.'//
     &                               n_model(:3)
                        else
                           nrchar=STR_LENGTH(lwpcGRD_loc)
                           file_name=lwpcGRD_loc(:nrchar)//
     &                               tx_grd(nt)(:nrchr_f)//'.'//
     &                               n_model(:3)
                        end if
                     else
                        file_name=tx_dir(nt)(:STR_LENGTH(tx_dir(nt)))//
     &                            tx_grd(nt)(:nrchr_f)//'.'//
     &                            n_model(:3)
                     end if
                  else

c                    Standard ASCII and Binary files append the
c                    area id to the file name
                     nrchr_a=STR_LENGTH(area_id)
                     call STR_LOWER (area_id,1,nrchr_a)
                     call STR_UPPER (area_id,1,1)

                     if (STR_LENGTH(tx_dir(nt)) .eq. 0) then
                        if (STR_LENGTH(lwpcGRD_loc) .eq. 0) then
                           file_name=tx_grd(nt)(:nrchr_f)//
     &                               area_id(:nrchr_a)//'.'//
     &                               n_model(:3)
                        else
                           nrchar=STR_LENGTH(lwpcGRD_loc)
                           file_name=lwpcGRD_loc(:nrchar)//
     &                               tx_grd(nt)(:nrchr_f)//
     &                               area_id(:nrchr_a)//'.'//
     &                               n_model(:3)
                        end if
                     else
                        file_name=tx_dir(nt)(:STR_LENGTH(tx_dir(nt)))//
     &                            tx_grd(nt)(:nrchr_f)//
     &                            area_id(:nrchr_a)//'.'//
     &                            n_model(:3)
                     end if
                  end if
cxxcDEBUG
cxx                  write(lwpcLOG_lun,'(a)') 'Noise  file is:'//file_name

                  INQUIRE (file=file_name,exist=exists)
                  if (exists) then

                     nt=5
                  else
     &            if (nt .eq. 5) then

                     write(error_msg,
     &                   '(''Noise grid file not found: '',a)')
     &                     file_name
                     call LWPC_ERROR ('Error',error_msg)
                  end if
               end if
               nt=nt+1
            end do
         else

c           Formulate file name using lwpcNOI_loc, the name of the month
c           and the frequency
            write (file_name,
     &           '(a,''f''i7.7,''\'',a,i4.4,''.'',a))')
     &             lwpcNOI_loc(:STR_LENGTH(lwpcNOI_loc)),
     &             INT((freq+.00005)*10000.),
     &             n_mnth_nam(:3),n_UT,
     &             n_model(:3)
cxxcDEBUG
cxx            write(lwpcLOG_lun,'(a)') 'Noise  file is:'//file_name

            INQUIRE (file=file_name,exist=exists)
         end if

         if (.NOT.exists) then

            write(error_msg,
     &          '(''Noise grid file not found: '',a)')
     &            file_name
            call LWPC_ERROR ('Error',error_msg)
         end if

c        Open the Grid file
         if (grd_format(1:1) .eq. 'C' .or.
     &       grd_format(1:1) .eq. 'c') then

            OPEN (lu_grd,file=file_name,
     &                   status='old',form='unformatted',
     &                   access='sequential',recordtype='fixed',
     &                   iostat=iocheck,err=900,action='read')
         else
     &   if (grd_format(1:1) .eq. 'A' .or.
     &       grd_format(1:1) .eq. 'a') then

            OPEN (lu_grd,file=file_name,
     &                   status='old',form='formatted',
     &                   iostat=iocheck,err=900,action='read')
         else

            OPEN (lu_grd,file=file_name,
     &                   status='old',form='unformatted',
     &                   iostat=iocheck,err=900,action='read')
         end if

         nrlat_n=nrlat
         nrlon_n=nrlon

         call PLT_READ_NOISE
     &       (lu_grd,
     &        file_id,n_model,
     &        freq,n_mnth_num,n_day,n_year,n_UT,
     &        bandw,ralt,adjny,vsd_flag,
     &        mxpath,nrbrng,bearing,rhomax,rxlat,rxlon,
     &        mxprm,nrprm,param,
     &        nrcmp,mxlat,nrlat_n,mxlon,nrlon_n,
     &        area_id,opa_lat(1),opa_lon(1),opa_lat(2),opa_lon(2),
     &        ampn,stdn,vsdn,
     &        grd_format)

         CLOSE(lu_grd)

         if (n_model(:3) .eq. 'lnp') then

c           Interpolate the coarse LNP grid to the required spacing.
            call PLT_LNP_TO_GRD
     &          (nrcmp,mxlat,mxlon,
     &           nrlat_n,nrlon_n,nrlat,nrlon,
     &           opa_lat(1),opa_lon(1),opa_lat(2),opa_lon(2),
     &           ampn,stdn,vsdn,amp1)
         end if

c        Generate label for noise data
         file_lbl(6)=file_id(3)
         prfl_lbl(6)='    '
         write(xmtr_lbl( 6),'(''Noise: '',a)') n_model

         if (rcomp .eq. 1) then
            write(data_lbl(6),
     &          '(f5.1,2x,a3,i5.4,''UT'')')
     &            freq,n_mnth_nam,n_UT
         else
            write(data_lbl(6),
     &          '(f5.1,2x,a3,i5.4,''UT'',
     &            ''  dN='',f4.1,''dB'')')
     &            freq,n_mnth_nam,n_UT,adjny
         end if

         if (use_antenna .or. use_pnoise) then

c           Adjust noise parameters to account for internal noise
c           specification of the specified antenna and/or the EMI
c           level (platform noise).
            if (use_antenna) then

c              Adjust the atmospheric noise strength for depth.
               depth_factor=.33*SQRT(freq)*depth

c              Antenna noise
               ampi=-1.
               n=1
               do while (ampi .eq. -1.)
                  if (antenna .eq. antenna_list(n)) then
                     m=2
                     do while (freq .gt. antenna_spec(m,0))
                        m=m+1
                     end do
                     ampi=(freq               -antenna_spec(m-1,0))
     &                   /(antenna_spec(m  ,0)-antenna_spec(m-1,0))
     &                   *(antenna_spec(m  ,n)-antenna_spec(m-1,n))
     &                   + antenna_spec(m-1,n)
                  end if
                  n=n+1
               end do
            else
               ampi=-200.
            end if

            if (use_pnoise) then

c              EMI (platform noise)
               ampe=pnoise
            else
               ampe=-200.
            end if

c           Combined interference (sum of powers)
            pwri=10.**(ampi/10.)+10.**(ampe/10.)

            do j=1,nrlat
               do i=1,nrlon

c                 Compensate for depth and get noise power.
                  pwrn=10.**((ampn(i,j,1)-depth_factor)/10.)

c                 Total noise at depth.
                  ampn(i,j,1)=10.*LOG10(pwri+pwrn)

c                 It is assummed that the interference is specified at
c                 the 90% TA; so we determine the modified standard
c                 deviation of the noise by calculating the noise at 50%
c                 and 90%. It is assummed that the interference from the
c                 antenna and EMI are steady (std dev is 0).
                  sign=stdn(i,j,1)
                  amp50=10.*LOG10(pwri+pwrn)
                  amp90=10.*LOG10(pwri+pwrn*10.**(.128*sign))
                  stdn(i,j,1)=(amp90-amp50)/1.28

                  if (rcomp .gt. 1) then
c                    Repeat calculations for horizontal component
                     pwrn=10.**((ampn(i,j,2)-depth_factor)/10.)
                     ampn(i,j,2)=10.*LOG10(pwri+pwrn)
                     sign=stdn(i,j,2)
                     amp50=10.*LOG10(pwri+pwrn)
                     amp90=10.*LOG10(pwri+pwrn*10.**(.128*sign))
                     stdn(i,j,2)=(amp90-amp50)/1.28
                  end if
               end do
            end do
         end if
      end if

Changed 07 Nov 90: Allow map area to be a subset of the op area.

c     Store the op area
      grd_lat(1)=opa_lat(1)
      grd_lat(2)=opa_lat(2)
      grd_lon(1)=opa_lon(1)
      grd_lon(2)=opa_lon(2)

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(''--------'')')
cxx      do n=-1,6
cxx         write(lwpcLOG_lun,'(a)') xmtr_lbl(n)
cxx      end do
cxx      write(lwpcLOG_lun,'(''--------'')')
cxx      do n=-1,6
cxx         write(lwpcLOG_lun,'(a)') data_lbl(n)
cxx      end do
cxx      write(lwpcLOG_lun,'(''--------'')')
cxx      do n=-1,6
cxx         write(lwpcLOG_lun,'(a)') prfl_lbl(n)
cxx      end do
cxx      write(lwpcLOG_lun,'(''--------'')')
cxx      do n=-1,6
cxx         write(lwpcLOG_lun,'(a)') file_lbl(n)
cxx      end do
cxx      write(lwpcLOG_lun,'(''--------'')')
cxx      do n=-1,1
cxx         write(lwpcLOG_lun,'(a)') rcvr_lbl(n)
cxx      end do
cxx      write(lwpcLOG_lun,'(''--------'')')

c     Set up mapping
      call GEO_DEFS
     &    (map_prjctn,map_lat,map_lon,map_size)

51    if (nplot .gt. 0) then

c        Set plot label parameters
         graf_lbl='Noise (dB/uV/m)'

         nplt=1

         call PLT_CNTR_N
     &       (mxlat,nrlat,mxlon,nrlon,
     &        amp1,std1,
     &        rcomp,nplot,
     &        ampn,stdn)
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(/a)') 'Noise'
cxx         write(lwpcLOG_lun,
cxx     &       '(''nrLat='',i3,'' nrLon='',i3)') nrLat,nrLon
cxx         do n=1,nrlon,29
cxx            write(file_name,'(''nti.'',i3.3)') n
cxx            open (lwpcTMP_lun,file=file_name,status='unknown')
cxx            write(bcd,
cxx     &          '(''(i3,'',i2,''f5.1,i3)'')')
cxx     &            MIN(nrlon-n+1,29)
cxx            write(lwpcTMP_lun,
cxx     &           '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx            do j=nrlat,1,-1
cxx               write(lwpcTMP_lun,bcd)
cxx     &               j,(amp1(i,j),i=n,MIN(nrlon,n+28)),j
cxx            end do
cxx            write(lwpcTMP_lun,
cxx     &          '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx            close(lwpcTMP_lun)
cxx         end do

         if (ntlvl .eq. 0) then

            call PLT_CNTR
     &          (plt_device,plt_orientation,
     &           brief,xlbl,ylbl,
     &           ntlist,ntl,nplt,
     &           plot_lbl,graf_lbl,
     &           xmtr_lbl,data_lbl,
     &           prfl_lbl,file_lbl,
     &           rcvr_lbl,cntr_lbl,
     &           plevel,nplvl,
     &           cntr_level,nclvl,cntr_range,
     &           cntr_lvl,mxlvl,
     &           opa_lat,opa_lon,
     &           grd_lat,grd_lon,
     &           map_type,map_size,map_tics,map_grid,
     &           terminator,chi_list,
     &           chi_max,chi_min,chi_inc,
     &           chi_label,chi_line,
     &           amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
         else

            call PLT_EXCD
     &          (plt_device,plt_orientation,
     &           brief,xlbl,ylbl,
     &           ntlist,ntl,nplt,
     &           plot_lbl,graf_lbl,
     &           xmtr_lbl,data_lbl,
     &           prfl_lbl,file_lbl,
     &           rcvr_lbl,cntr_lbl,
     &           threshold,ntlvl,
     &           plevel,nplvl,
     &           cntr_lvl,mxlvl,
     &           opa_lat,opa_lon,
     &           grd_lat,grd_lon,
     &           map_type,map_size,map_tics,map_grid,
     &           terminator,chi_list,
     &           chi_max,chi_min,chi_inc,
     &           chi_label,chi_line,
     &           amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
         end if
      end if

      if (splot(1) .gt. 0) then

c        Set plot label parameters
         graf_lbl='Signal (dB/uV/m)'

         do nplt=1,5
            if (splot(nplt) .gt. 0) then

               call PLT_CNTR_S
     &             (mxlat,nrlat,mxlon,nrlon,
     &              amp1,std1,
     &              rcomp,splot(nplt),ntlist,ntl,
     &              ampt,stdt)
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(/a)') 'Signal'
cxx               write(lwpcLOG_lun,'(a,i3,a,i3)')
cxx     &              'nrLat= ',nrLat,' nrLon= ',nrLon
cxx               do n=1,nrlon,29
cxx                  write(file_name,'(''grd.'',i3.3)') n
cxx                  open (lwpcTMP_lun,file=file_name,status='unknown')
cxx                  write(bcd,
cxx     &                '(''(i3,'',i2,''f5.1,i3)'')')
cxx     &                  MIN(nrlon-n+1,29)
cxx                  write(lwpcTMP_lun,
cxx     &                 '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx                  do j=nrlat,1,-1
cxx                     write(lwpcTMP_lun,bcd)
cxx     &                     j,(amp1(i,j),i=n,MIN(nrlon,n+28)),j
cxx                  end do
cxx                  write(lwpcTMP_lun,
cxx     &                '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx                  close(lwpcTMP_lun)
cxx               end do

               if (ntlvl .eq. 0) then

                  call PLT_CNTR
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 plevel,nplvl,
     &                 cntr_level,nclvl,cntr_range,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               else

                  call PLT_EXCD
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 threshold,ntlvl,
     &                 plevel,nplvl,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               end if
            end if
         end do
      end if

      if (snplot(1) .gt. 0) then

c        Set plot label parameters
         graf_lbl='S/N (dB)'

         do nplt=1,5
            if (snplot(nplt) .gt. 0) then

               call PLT_CNTR_SN
     &             (mxlat,nrlat,mxlon,nrlon,
     &              amp1,std1,
     &              rcomp,snplot(nplt),ntlist,ntl,
     &              ampt,stdt,ampn,stdn)
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(/a)') 'SNR'
cxx               write(lwpcLOG_lun,'(a,i3,a,i3)')
cxx     &              'nrLat= ',nrLat,' nrLon= ',nrLon
cxx               do n=1,nrlon,29
cxx                  write(file_name,'(''snr.'',i3.3)') n
cxx                  open (lwpcTMP_lun,file=file_name,status='unknown')
cxx                  write(bcd,
cxx     &                '(''(i3,'',i2,''f5.1,i3)'')')
cxx     &                  MIN(nrlon-n+1,29)
cxx                  write(lwpcTMP_lun,
cxx     &                 '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx                  do j=nrlat,1,-1
cxx                     write(lwpcTMP_lun,bcd)
cxx     &                     j,(amp1(i,j),i=n,MIN(nrlon,n+28)),j
cxx                  end do
cxx                  write(lwpcTMP_lun,
cxx     &                '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx                  close(lwpcTMP_lun)
cxx               end do

c              Check if receiver model has been chosen
               if (use_rx_model) then

c                 Adjust the SNR according to specified receiver
                  call PLT_RX_MODEL
     &                (rx_model,rcomp,
     &                 amp1,ampn,ampi,ampe,vsdn,
     &                 mxlat,nrlat,mxlon,nrlon)
               end if

               if (ntlvl .eq. 0) then

                  call PLT_CNTR
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 plevel,nplvl,
     &                 cntr_level,nclvl,cntr_range,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               else

                  call PLT_EXCD
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 threshold,ntlvl,
     &                 plevel,nplvl,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               end if
            end if
         end do
      end if

      if (sjplot(1) .gt. 0) then

c        Set plot label parameters
         graf_lbl='S/J (dB)'

         do nplt=1,15
            if (sjplot(nplt) .gt. 0) then

               call PLT_CNTR_SJ
     &             (mxlat,nrlat,mxlon,nrlon,
     &              amp1,std1,amp2,std2,
     &              rcomp,sjplot(nplt),ntlist,ntl,
     &              ampt,stdt)
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(/a)') 'SJR'
cxx               write(lwpcLOG_lun,'(a,i3,a,i3)')
cxx     &              'nrLat= ',nrLat,' nrLon= ',nrLon
cxx               do n=1,nrlon,29
cxx                  write(file_name,'(''sjr.'',i3.3)') n
cxx                  open (lwpcTMP_lun,file=file_name,status='unknown')
cxx                  write(bcd,
cxx     &                '(''(i3,'',i2,''f5.1,i3)'')')
cxx     &                  MIN(nrlon-n+1,29)
cxx                  write(lwpcTMP_lun,
cxx     &                 '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx                  do j=nrlat,1,-1
cxx                     write(lwpcTMP_lun,bcd)
cxx     &                     j,(amp1(i,j),i=n,MIN(nrlon,n+28)),j
cxx                  end do
cxx                  write(lwpcTMP_lun,
cxx     &                '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx                  close(lwpcTMP_lun)
cxx               end do

               if (ntlvl .eq. 0) then

                  call PLT_CNTR
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 plevel,nplvl,
     &                 cntr_level,nclvl,cntr_range,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               else

                  call PLT_EXCD
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 threshold,ntlvl,
     &                 plevel,nplvl,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               end if
            end if
         end do
      end if

      if (siplot(1) .gt. 0) then

c        Set plot label parameters
         graf_lbl='S/(J+N) (dB)'

         do nplt=1,15
            if (siplot(nplt) .gt. 0) then

               call PLT_CNTR_SI
     &             (mxlat,nrlat,mxlon,nrlon,
     &              amp1,std1,amp2,std2,
     &              rcomp,siplot(nplt),ntlist,ntl,
     &              ampt,stdt,ampn,stdn)

c              Check if receiver model has been chosen
               if (use_rx_model) then

c                 Adjust the SNR according to
                  call PLT_RX_MODEL
     &                (rx_model,rcomp,
     &                 amp1,ampn,ampi,ampe,vsdn,
     &                 mxlat,nrlat,mxlon,nrlon)
               end if

               if (ntlvl .eq. 0) then

                  call PLT_CNTR
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 plevel,nplvl,
     &                 cntr_level,nclvl,cntr_range,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               else

                  call PLT_EXCD
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 threshold,ntlvl,
     &                 plevel,nplvl,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               end if
            end if
         end do
      end if

      if (jplot(1) .gt. 0) then

c        Set plot label parameters
         graf_lbl='Jammer (dB/uV/m)'

         do nplt=1,5
            if (jplot(nplt) .gt. 0) then

               call PLT_CNTR_S
     &             (mxlat,nrlat,mxlon,nrlon,
     &              amp1,std1,
     &              rcomp,6-jplot(nplt),ntlist,ntl,
     &              ampt,stdt)

               if (ntlvl .eq. 0) then

                  call PLT_CNTR
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 plevel,nplvl,
     &                 cntr_level,nclvl,cntr_range,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               else

                  call PLT_EXCD
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 threshold,ntlvl,
     &                 plevel,nplvl,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               end if
            end if
         end do
      end if

      if (jnplot(1) .gt. 0) then

c        Set plot label parameters
         graf_lbl='J/N (dB)'

         do nplt=1,4
            if (jnplot(nplt) .gt. 0) then

               call PLT_CNTR_SN
     &             (mxlat,nrlat,mxlon,nrlon,
     &              amp1,std1,
     &              rcomp,6-jnplot(nplt),ntlist,ntl,
     &              ampt,stdt,ampn,stdn)

c              Check if receiver model has been chosen
               if (use_rx_model) then

c                 Adjust the SNR according to
                  call PLT_RX_MODEL
     &                (rx_model,rcomp,
     &                 amp1,ampn,ampi,ampe,vsdn,
     &                 mxlat,nrlat,mxlon,nrlon)
               end if

               if (ntlvl .eq. 0) then

                  call PLT_CNTR
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 plevel,nplvl,
     &                 cntr_level,nclvl,cntr_range,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               else

                  call PLT_EXCD
     &                (plt_device,plt_orientation,
     &                 brief,xlbl,ylbl,
     &                 ntlist,ntl,nplt,
     &                 plot_lbl,graf_lbl,
     &                 xmtr_lbl,data_lbl,
     &                 prfl_lbl,file_lbl,
     &                 rcvr_lbl,cntr_lbl,
     &                 threshold,ntlvl,
     &                 plevel,nplvl,
     &                 cntr_lvl,mxlvl,
     &                 opa_lat,opa_lon,
     &                 grd_lat,grd_lon,
     &                 map_type,map_size,map_tics,map_grid,
     &                 terminator,chi_list,
     &                 chi_max,chi_min,chi_inc,
     &                 chi_label,chi_line,
     &                 amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)
               end if
            end if
         end do
      end if
      call GRF_DONE

c     If the GO button has been pressed or printing the graphs,
c     immediately get next graph
      if (next_graph .eq. 3) go to 11

      if (next_graph .eq. 4) plt_device='sys-scn'

c     Output is to the screen,
c     return to FORDRIVE and await user's input.
      RETURN
900   write(error_msg,
     &    '(''Error '',i3,'' occurred trying to open '',
     &      ''file: '',a)') iocheck,file_name
      call LWPC_ERROR ('Error',error_msg)

9999  CLOSE(lwpcINP_lun)
      CLOSE(lwpcLOG_lun)
      call CLOSE_FORDRIVE
      END      ! grdPlot
