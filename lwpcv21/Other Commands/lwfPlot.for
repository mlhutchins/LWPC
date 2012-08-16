      SUBROUTINE FORDRIVE

c     PROGRAM    LWF_PLOT

c***********************************************************************

c Plots signal data along a radial path using LWF files

c***********************************************************************

c CONTROL STRINGS:

c PLOTTER     PLOT_DEVICE
c FILE-DAT    directory-location-of-"fixed"-data-files
c FILE-LWF    directory-location-of-LWF-data-files
c DIST-AXIS   scalex sizex dstmax dsttic
c AMPL-AXIS   scalea sizea ampmin ampmax amptic
c PHAS-AXIS   scalep sizep phsmin phsmax phstic UNITS
c PHAS-PLOT   OPTION
c TX          TX-FILE-NAME
c TX-NTR      TX-FILE-NAME
c JX          JX-FILE-NAME
c JX-NJR      JX-FILE-NAME
c BEARINGS    list-of-bearings
c HDGS/GRAPH  number-of-headings-per-graph
c MODIFY-PWR  power
c PRINT-LWF   print-lwf
c RUNNING-AVG nravg
c START

c INPUT PARAMETERS:

c PLOT_DEVICE plotter device (SYS,SYS-PRN,FILE-NAME)

c scalex      distance  scale        in km/inch
c sizex       distance  axis lengths in inches
c dstmax      distance  max          in km
c dsttic      distance  tic interval in km
c NOTES:    1 If SCALEX is zero, then SIZEX and DSTMAX are used;
c             otherwise, SCALEX and DSTMAX are used.
c           2 If DSTMAX is -99, then its value is found from the data.

c scalea      amplitude scale        in dB/inch
c sizea       amplitude axis lengths in inches
c ampmin      amplitude min          in dB
c ampmax      amplitude max          in dB
c amptic      amplitude tic interval in dB
c NOTES:    1 If SCALEA is zero, then SIZEA, AMPMIN and AMPMAX are used;
c             otherwise, SCALEA, SIZEA and AMPMAX are used and AMPMIN is
c             found from the data.
c           2 If either AMPMAX or AMPMIN are -99, then their values are
c             determined from the data.

c scalep      phase scale        in units/inch
c sizep       phase axis lengths in inches
c phsmin      phase min          in units
c phsmax      phase max          in units
c phstic      phase tic interval in units
c UNITS       phase units (DEGREES or MICROSEC)
c NOTES:    1 If SCALEP is zero, then SIZEP, PHSMIN and PHSMAX are used;
c             otherwise, SCALEP, SIZEP and PHSMAX are used and PHSMIN is
c             found from the data.
c           2 If either PHSMAX or PHSMIN are -99, then their values are
c             determined from the data.

c power       power to be used to plot the data in kW; used if not zero

c print_lwf
c         =0: no print out; only plots generated
c         =1: print and plot

c mxcurv      number of headings to plot on each graph

c OPTION      YES/NO to turn on/off plots of the phase

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

c     20 Jan 94     Added 1 degree tolerance for selecting bearing to be
c                   plotted.

c     09 Feb 94     Changed PLT_ORIENTATION to CHARACTER*80 to handle
c                   longer output file names when PLT_DEVICE is FILE.

c     30 Jun 94     Dropped PLT_UNIT, NRPS, RHOMX, RLAT, RLON and RRHO
c                   from call to PLT_LWF.

c     25 Oct 95     Changed to use lwpc_LUN.cmn and lwpc_LUN.ini to set
c                   logical units.

c     07 Dec 95     Changes to allow plot of phase.

c     25 Jan 96     Added calls to GRF_BEGIN, GRF_ORIGIN and
c                   PLT_LWF_LABEL.

c     17 Apr 96     Added capability for running averages.

c     16 Dec 96     Generalize delimiters used with file names.

c     12 Mar 97     Modified to use new string routines in GRF.

c     25 Mar 98     Modified to use graphical controls available
c                   through NEXT_GRAPH.

c     06 Sep 98     Added coding which plots a file using program
c                   defaults just by double-clicking on the file;
c                   file types in Windows Explorer must be modified
c                   to execute the program

c*******************!***************************************************

c     I/O handling
      include      'sysStrct.cmn'

c     Plotter parameters
      character* 12 plt_orientation
      character* 96 plt_device

c     LWPC parameters
      include      'lwpc_cfg.cmn'
      include      'lwpc_lun.cmn'
      include      'lwpc_lun.ini'

c     Program parameters
      parameter    (mxprm=21,mxps=11,mxsgmnt=201,
     &              mxpath=120,mxpts=1002,mxlbl=8)

      character* 20 pgm_name
      data          pgm_name/'lwfPlot'/

      character*  1 delimiter(3)
      character*  8 archive,prgm_id,
     &              lwf_format,
     &              list_str(6),
     &              line_color(0:3),
     &              color
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id,
     &              control_string,data_string,
     &              directory,file_lwf,extension
      character*120 bcd,
     &              CMDLine,
     &              file_id(3),
     &              file_name,
     &              root_name
      character*128 plt_label(mxlbl),
     &              plblx
      character*200 error_msg
      logical       begin_file,end_file,
     &              do_grf,phs_plot,exists
      integer       str_length,
     &              print_lwf,
     &              phs_units,
     &              line_type(0:3)
      real          freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              oplat1,oplon1,oplat2,oplon2,
     &              bearing(mxpath),rhomax(mxpath),
     &              rxlat(mxpath),rxlon(mxpath),

     &              param(mxprm),brng(mxpath),sgmnt(mxps,mxsgmnt),

     &              dst(mxpts),amp(mxpts,3),phs(mxpts,3),

     &              pwr,dist,incl,headng,talt,ralt,power,

     &              xlbl(2),ylbl(2)

      dimension     dist_scale(4)

      equivalence  (dist_scale(1),scalex),(dist_scale(2),sizex ),
     &             (dist_scale(3),dstmax),(dist_scale(4),dsttic)

      dimension     ampl_scale(5)

      equivalence  (ampl_scale(1),scalea),(ampl_scale(2),sizea ),
     &             (ampl_scale(3),ampmin),(ampl_scale(4),ampmax),
     &             (ampl_scale(5),amptic)

      dimension     phas_scale(5)

      equivalence  (phas_scale(1),scalep),(phas_scale(2),sizep ),
     &             (phas_scale(3),phsmin),(phas_scale(4),phsmax),
     &             (phas_scale(5),phstic)

      data          scalex              /2000./,          sizex/5./,
     &              dstmin,dstmax,dsttic/-99.,-99.,1000./,
     &              scalea              /20./,            sizea/5./,
     &              ampmin,ampmax,amptic/-99.,-99.,10./,
     &              scalep              /60./,            sizep/5./,
     &              phsmin,phsmax,phstic/-180.,180.,30./,
     &              phs_units/0/

     &              nrbrng/0/,brng/mxpath*0./,
     &              power/0./,mxcurv/1/,

     &              plt_device/'sys-scn'/,

     &              line_color/'purple','black','blue','red'/,
     &              line_type/5,1,2,4/,
     &              sz/.1/,

     &              lwf_format/'Binary'/
     &              print_lwf/0/,
     &              phs_plot/.false./

      character*120 null
      data          null/z0/

c     The parameter next_graph comes from sysStrct.cmn
c
c     next_graph=0 First graph
c                1 Display next graph
c                2 Re-draw current graph
c                3 Display all remaining graphs
c                4 Print current graph
c                5 Option button 1
c                6 Option button 2

      if (next_graph .gt. 0) go to 9

c     Get the location of the LWPC data
      call LWPC_DAT_LOC

c     Get delimiters used in the file names
      call GET_DELIMITER (delimiter)

c     Get the command line
      call GET_COMMAND_LINE (LenCMD,CMDLine)

c     CMDLine now contains the arguments of the command line. For this
c     program we assume that there is only the input file name. This
c     file will be either the list of control strings (.inp) or the name
c     of the file to be plotted (.lwf).
      if (LenCMD .eq. 0) then

c        There must be an INP file with the default name
         root_name=pgm_name
         file_lwf=null
      else

c        Check for argument extension
         root_name=CMDLine
         call DECODE_FILE_NAME
     &       (CMDLine,directory,file_name,extension)
         call STR_LOWER(extension,0,0)
         if (extension(:3) .eq. 'inp') then
            root_name=file_name
            file_lwf=null
         else
            root_name=pgm_name
            file_lwf=file_name
         end if
      end if

      lrtnm=STR_LENGTH(root_name)

c     Input file
      write(file_name,'(a,''.inp'')') root_name(:lrtnm)
      INQUIRE (file=file_name,exist=exists)
      if (exists) then
         OPEN (lwpcINP_lun,file=file_name,status='old')
      end if

c     Log file
      write(file_name,'(a,''.log'')') root_name(:lrtnm)
      OPEN (lwpcLOG_lun,file=file_name,status='unknown')

c     Initialize logical units
      lu_lwf=lwpcLWF_lun

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

c     Get control string
11    if (.not.exists) go to 51

      read (lwpcINP_lun,'(a)',end=99) bcd
      write(lwpcLOG_lun,'(a)') bcd

      if (bcd(1:1) .eq. ' ') go to 11

      call DECODE_CONTROL_DATA (bcd,control_string,data_string)
      call STR_UPPER (control_string,0,0)

c QUIT
      if (control_string(1:1) .eq. 'Q') go to 99

c FILE-DAT
      if (control_string(1:6) .eq. 'FILE-D') then

         if (data_string(1:1) .eq. ' ') then

c           Default data directory
            call LWPC_DAT_LOC
         else

            lwpcDAT_loc=data_string(1:STR_LENGTH(data_string))
            n=STR_LENGTH(lwpcDAT_loc)
            if (lwpcDAT_loc(n:n) .ne. delimiter(3)) then
               n=n+1
               lwpcDAT_loc(n:n)=delimiter(3)
            end if
         end if
         go to 11
      end if

c FILE-LWF
      if (control_string(1:6) .eq. 'FILE-L') then

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
         go to  11
      end if

c AMPL-AXIS
      if (control_string(1:1) .eq. 'A') then

         call DECODE_LIST_FLT
     &       (data_string,5,nlist,ampl_scale)
         go to 11
      end if

c BEARINGS
      if (control_string(1:1) .eq. 'B') then

         if (data_string(1:1) .eq. ' ') then
            nrbrng=0
         else
            call DECODE_LIST_FLT
     &          (data_string,mxpath,nrbrng,brng)
         end if
         go to 11
      end if

c DIST-AXIS
      if (control_string(1:1) .eq. 'D') then

         call DECODE_LIST_FLT
     &       (data_string,4,nlist,dist_scale)
         go to 11
      end if

c HDGS/GRAPH
      if (control_string(1:1) .eq. 'H') then

         call DECODE_LIST_INT
     &       (data_string,1,nlist,mxcurv)
         go to 11
      end if

c MODIFY-PWR
      if (control_string(1:1) .eq. 'M') then

         call DECODE_LIST_FLT
     &       (data_string,1,nlist,power)
         go to 11
      end if

c PHAS-AXIS
      if (control_string(1:6) .eq. 'PHAS-A' .or.
     &    control_string(1:6) .eq. 'PHAS_A') then

         call STR_COUNT_LIST
     &       (data_string,0,0,nritem)

         call DECODE_LIST_FLT
     &       (data_string,5,nlist,phas_scale)

         if (nritem .eq. 6) then

            call DECODE_LIST_STR
     &          (data_string,6,nlist,list_str)

            call STR_UPPER (list_str(1),0,0)
            if (list_str(1)(1:1) .eq. 'D') then
               phs_units=0
            else
               phs_units=1
            end if
         end if
         go to 11
      end if

c PHAS-PLOT
      if (control_string(1:6) .eq. 'PHAS-P' .or.
     &    control_string(1:6) .eq. 'PHAS_P') then

         call DECODE_LIST_STR
     &       (data_string,6,nlist,list_str)

         call STR_UPPER (list_str(1),0,0)
         if (list_str(1)(1:1) .eq. 'Y') then
            phs_plot=.true.
         else
            phs_plot=.false.
         end if
         go to 11
      end if

c PLOTTER
      if (control_string(1:2) .eq. 'PL') then

         call DECODE_PLOTTER
     &       (data_string,plt_device,plt_orientation)
         call STR_LOWER (plt_device,0,0)

         if (plt_device(:5) .eq. 'sys-p' .or.
     &       plt_device(:5) .eq. 'sys_p') next_graph=3
         go to 11
      end if

c PRINT-LWF
      if (control_string(1:2) .eq. 'PR') then

         call DECODE_LIST_INT
     &       (data_string,1,nlist,print_lwf)
         go to 11
      end if

c RUNNING-AVG
      if (control_string(1:1) .eq. 'R') then

         call DECODE_LIST_INT
     &       (data_string,1,nlist,nravg)
         go to 11
      end if

c TX-NTR
c JX-NJR
      if (control_string(1:1) .eq. 'T' .or.
     &    control_string(1:1) .eq. 'J') then

         call DECODE_FILE_NAME
     &       (data_string,directory,file_lwf,extension)
         go to 11
      end if

c START
      if (control_string(1:1) .ne. 'S') then

c        Last chance for a valid control string: Error exit
         call LWPC_ERROR ('Error','Unrecognized control string')
      end if

51    if (STR_LENGTH(directory) .eq. 0) then
         if (STR_LENGTH(lwpcLWF_loc) .eq. 0) then
            file_name=file_lwf(:STR_LENGTH(file_lwf))//'.lwf'
         else
            file_name=lwpcLWF_loc(:STR_LENGTH(lwpcLWF_loc))//
     &                file_lwf(:STR_LENGTH(file_lwf))//'.lwf'
         end if
      else
         file_name=directory(:STR_LENGTH(directory))//
     &             file_lwf(:STR_LENGTH(file_lwf))//'.lwf'
      end if

      OPEN (lu_lwf,file=file_name,
     &             status='old',form='unformatted',
     &             iostat=iocheck,err=90 )

c     Get header data from the mode parameter file
      call READ_HDR
     &    (lu_lwf,print_lwf,
     &     archive,file_id,prgm_id,
     &     case_id,prfl_id,
     &     xmtr_id,freq,tlat,tlon,
     &     path_id,oplat1,oplon1,oplat2,oplon2,
     &     mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &     begin_file,end_file,
     &     lwf_format)

c     Read propagation path data
      do while (.not.end_file)

         nrcurv=0
         nlwf=1
         nrlwf=1
         do while (nlwf .le. nrlwf)

            call READ_LWF
     &          (lu_lwf,print_lwf,
     &           bearng,rhomx,rlat,rlon,rrho,
     &           mxps,nrps,mxsgmnt,nrsgmnt,sgmnt,
     &           mxprm,nrprm,param,nrcmp,nrlwf,
     &           mxpts,nrpts,dst,amp(1,1),phs(1,1),
     &           begin_file,end_file)

            if (.not.end_file) then

               if (nrcmp .gt. 1) then ! get remaining components
                  do ncmp=2,nrcmp
                     call READ_LWF
     &                   (lu_lwf,print_lwf,
     &                    bearng,rhomx,rlat,rlon,rrho,
     &                    mxps,nrps,mxsgmnt,nrsgmnt,sgmnt,
     &                    mxprm,nrprm,param,nrcmp,nrlwf,
     &                    mxpts,nrpts,dst,amp(1,ncmp),phs(1,ncmp),
     &                    begin_file,end_file)
                  end do
               end if

               pwr   =param( 1)
               dist  =param( 2)
               incl  =param( 3)
               headng=param( 4)
               talt  =param( 5)
               ralt  =param( 6)

               call PLT_LWF_AVG
     &             (mxpts,nrpts,nrcmp,amp,nravg)

               if (nlwf .eq. 1) then

c                 Get distance scaling
                  nxmin=1
                  if (dstmin .eq. -99.) then
                     xmin=AINT(dst(1)/1000.   )*1000.
                  else
                     xmin=dstmin
                     do while (xmin .gt. dst(nxmin))
                        nxmin=nxmin+1
                     end do
                  end if

                  if (rlat .eq. 99.) then
                     nxmax=nrpts
                  else
                     nxmax=nrpts-1
                  end if
                  if (dstmax .eq. -99.) then
                     xmax=AINT(dst(nxmax)/1000.+.5)*1000.
                  else
                     xmax=dstmax
                     do while (xmax .lt. dst(nxmax))
                        nxmax=nxmax-1
                     end do
                  end if
                  xtic=dsttic

                  if (scalex .eq. 0.) then
                     xlngth=sizex
                     xscale=(xmax-xmin)/xlngth
                  else
                     xscale=scalex
                     xlngth=(xmax-xmin)/xscale
                  end if

c                 Get amplitude scaling
                  if (power .ne. 0.) then
                     adj=10.*LOG10(power/pwr)
                     pwr=power
                     do i=1,nrpts
                        amp(i,1)=amp(i,1)+adj
                        if (nrcmp .gt. 1) amp(i,2)=amp(i,2)+adj
                        if (nrcmp .gt. 2) amp(i,3)=amp(i,3)+adj
                     end do
                  end if

                  if (ampmax .eq. -99.) then
                     if (nxmin .eq. 1) then
                        i1=2
                     else
                        i1=nxmin
                     end if
                     ymax=amp(i1,1)
                     if (nrcmp .gt. 1) then
                        ymax=MAX(ymax,amp(i1,2))
                        if (nrcmp .gt. 2) ymax=MAX(ymax,amp(i1,3))
                     end if
                     do i=i1+1,nxmax
                        ymax=MAX(ymax,amp(i,1))
                        if (nrcmp .gt. 1) then
                           ymax=MAX(ymax,amp(i,2))
                           if (nrcmp .gt. 2) ymax=MAX(ymax,amp(i,3))
                        end if
                     end do
                     ymax=AINT(ymax/10.+.5)*10.

c                    Make sure that no more than 1% of the points
c                    are above the calculated maximum
                     nt=(nxmax-nxmin+1)/100
                     ns=0
                     do while (ns .lt. nt)
                        nn=0
                        if (nxmin .eq. 1) then
                           i1=2
                        else
                           i1=nxmin
                        end if
                        do i=i1,nxmax
                           amax=amp(i,1)
                           if (nrcmp .gt. 1) amax=MAX(amax,amp(i,2))
                           if (nrcmp .gt. 2) amax=MAX(amax,amp(i,3))
                           if (amax .gt. ymax) nn=nn+1
                        end do
                        ns=nn
                        if (ns .lt. nt) ymax=ymax-10.
                     end do
                  else
                     ymax=ampmax
                  end if

                  if (ampmin .eq. -99.) then
                     if (nxmin .eq. 1) then
                        i1=2
                     else
                        i1=nxmin
                     end if
                     ymin=amp(i,1)
                     if (nrcmp .gt. 1) then
                        ymin=MIN(ymin,amp(i,2))
                        if (nrcmp .gt. 2) ymin=MIN(ymin,amp(i,3))
                     end if
                     do i=i1+1,nxmax
                        ymin=MIN(ymin,amp(i,1))
                        if (nrcmp .gt. 1) then
                           ymin=MIN(ymin,amp(i,2))
                           if (nrcmp .gt. 2) ymin=MIN(ymin,amp(i,3))
                        end if
                     end do
                     ymin=AINT(ymin/10.-.5)*10.

c                    Make sure that at least 90% of the points are
c                    above the calculated minimum
                     nt=(nxmax-nxmin+1)/10
                     ns=0
                     do while (ns .lt. nt)
                        nn=0
                        if (nxmin .eq. 1) then
                           i1=2
                        else
                           i1=nxmin
                        end if
                        do i=i1,nxmax
                           amin=amp(i,1)
                           if (nrcmp .gt. 1) then
                              amin=MIN(amin,amp(i,2))
                              if (nrcmp .gt. 2) amin=MIN(amin,amp(i,3))
                           end if
                           if (amin .lt. ymin) nn=nn+1
                        end do
                        ns=nn
                        if (ns .lt. nt) ymin=ymin+10.
                     end do
                  else
                     ymin=ampmin
                  end if

                  ytic=amptic
                  if (scalea .eq. 0.) then
                     ylngth=sizea
                     yscale=(ymax-ymin)/ylngth
                  else
                     ylngth=sizea
                     yscale=scalea
                     ymin=ymax-ylngth*yscale
                  end if
               end if

               if (nrbrng .gt. 0) then

c                 Look for specific bearings
                  do_grf=.false.
                  j=1
                  do while (j .le. nrbrng)
                     if (ABS(bearng-brng(j)) .lt. 1.) then

c                       At the desired bearing
                        do_grf=.true.
                        j=nrbrng
                     end if
                     j=j+1
                  end do
               else

c                 Do all bearings
                  do_grf=.true.
               end if

               if (do_grf) then

                  nrcurv=nrcurv+1
                  if (mxcurv .eq. 1 .or.
     &               (mxcurv .gt. 1 .and.
     &                MOD(nrcurv,mxcurv) .eq. 1)) then

                     call GRF_BEGIN
     &                   (plt_device,plt_orientation)

                     call GRF_ORIGIN
     &                   (1.,1.5)

                     call PLT_LWF_BORDER
     &                   (xlngth,xscale,xmin,xmax,xtic,xtic*2.,-1,
     &                    ylngth,yscale,ymin,ymax,ytic,ytic*2.,-1,
     &                    mxps,mxsgmnt,nrsgmnt,sgmnt)

                     plblx='Distance (Mm)'
                     call GRF_STRING
     &                   (.5*xlngth,-.4,.1,plblx,0.,'CB',retX)

                     plblx='Amplitude (dB above 1uV/m)'
                     call GRF_STRING
     &                   (-.5,.5*ylngth,.1,plblx,90.,'LC',retX)

                     xlabel=-.4
                     ylabel=-.6
                     call PLT_LWF_LABEL
     &                   (xlabel,ylabel,
     &                    file_id,prgm_id,
     &                    case_id,prfl_id,
     &                    xmtr_id,freq,tlat,tlon,bearng,
     &                    pwr,incl,headng,talt,ralt,nravg,
     &                    plt_label,mxlbl,nrlbl)

                     if (nrlwf .gt. 1 .and. mxcurv .gt. 1) then

c                       Put label for headings
                        call GRF_STRING
     &                      (xlngth+.1,ylngth-sz,sz,'headng',0.,
     &                      'LB',retX)

                        dy=sz*1.5
                        xlbl(1)=xlngth-.6
                        xlbl(2)=xlngth-.1
                        ylbl(1)=ylngth-sz
                        ylbl(2)=ylngth-sz
                        xl=xlngth+.1
                     end if
                  end if

c                 Set line type and color
                  if (mxcurv .eq. 1) then
                     line =1
                     color='black'
                  else
                     line =line_type (MOD(nrcurv,mxcurv))
                     color=line_color(MOD(nrcurv,mxcurv))
                  end if

                  if (nrlwf .gt. 1 .and. mxcurv .gt. 1) then

c                    Label the headings
                     ylbl(1)=ylbl(1)-dy
                     ylbl(2)=ylbl(2)-dy
                     write(plblx,'(f5.1)') headng
                     call GRF_COLOR
     &                   (color)
                     call GRF_STRING
     &                   (xl,ylbl(1)-.5*dy,sz,plblx,0.,'LB',retX)
                     call GRF_CURVE
     &                   (xlbl,ylbl,2,0.,0.,1.,1.,line,20)
                  end if

                  call PLT_LWF
     &                (print_lwf,.false.,1.,color,line,
     &                 plt_label,mxlbl,nrlbl,
     &                 xmin,xscale,
     &                 ymin,ymax,yscale,
     &                 mxpts,nrcmp,nxmin,nxmax,dst,amp,phs)

                  if (MOD(nrcurv,mxcurv) .eq. 0 .or. nlwf .eq. nrlwf)
     &               call GRF_DONE
               end if
            end if

            nlwf=nlwf+1
         end do
      end do

      if (phs_plot) then

c        Scaling
         ymax=phsmax
         ymin=phsmin
         ytic=phstic
         if (scalep .eq. 0.) then
            ylngth=sizep
            yscale=(ymax-ymin)/ylngth
         else
            ylngth=sizep
            yscale=scalep
            ymin=ymax-ylngth*yscale
         end if

         REWIND (lu_lwf)

c        Get header data from the mode parameter file
         call READ_HDR
     &       (lu_lwf,print_lwf,
     &        archive,file_id,prgm_id,
     &        case_id,prfl_id,
     &        xmtr_id,freq,tlat,tlon,
     &        path_id,oplat1,oplon1,oplat2,oplon2,
     &        mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &        begin_file,end_file,
     &        lwf_format)

c        Read propagation path data
         do while (.not.end_file)

            nrcurv=0
            nlwf=1
            nrlwf=1
            do while (nlwf .le. nrlwf)

               call READ_LWF
     &             (lu_lwf,print_lwf,
     &              bearng,rhomx,rlat,rlon,rrho,
     &              mxps,nrps,mxsgmnt,nrsgmnt,sgmnt,
     &              mxprm,nrprm,param,nrcmp,nrlwf,
     &              mxpts,nrpts,dst,amp(1,1),phs(1,1),
     &              begin_file,end_file)

               if (.not.end_file) then

                  if (nrcmp .gt. 1) then ! get remaining components
                     do ncmp=2,nrcmp
                        call READ_LWF
     &                      (lu_lwf,print_lwf,
     &                       bearng,rhomx,rlat,rlon,rrho,
     &                       mxps,nrps,mxsgmnt,nrsgmnt,sgmnt,
     &                       mxprm,nrprm,param,nrcmp,nrlwf,
     &                       mxpts,nrpts,dst,amp(1,ncmp),phs(1,ncmp),
     &                       begin_file,end_file)
                     end do
                  end if

                  if (nrbrng .gt. 0) then

c                    Look for specific bearings
                     do_grf=.false.
                     j=1
                     do while (j .le. nrbrng)
                        if (ABS(bearng-brng(j)) .lt. 1.) then

c                          At the desired bearing
                           do_grf=.true.
                           j=nrbrng
                        end if
                        j=j+1
                     end do
                  else

c                    Do all bearings
                     do_grf=.true.
                  end if

                  if (do_grf) then

                     nrcurv=nrcurv+1
                     if (mxcurv .eq. 1 .or.
     &                  (mxcurv .gt. 1 .and.
     &                   MOD(nrcurv,mxcurv) .eq. 1)) then

                        call GRF_BEGIN
     &                      (plt_device,plt_orientation)

                        call GRF_ORIGIN
     &                      (1.,1.5)

                        call PLT_LWF_BORDER
     &                      (xlngth,xscale,xmin,xmax,xtic,xtic*2.,-1,
     &                       ylngth,yscale,ymin,ymax,ytic,ytic*2.,-1,
     &                       mxps,mxsgmnt,nrsgmnt,sgmnt)

                        plblx='Distance (Mm)'
                        call GRF_STRING
     &                      (.5*xlngth,-.4,.1,plblx,0.,'CB',retX)

                        if (phs_units .eq. 0) then
                           plblx='Relative phase (deg)'
                        else
                           plblx='Relative phase (usec)'
                        end if
                        call GRF_STRING
     &                      (-.5,.5*ylngth,.1,plblx,90.,'LC',retX)

                        xlabel=-.4
                        ylabel=-.6
                        call PLT_LWF_LABEL
     &                      (xlabel,ylabel,
     &                       file_id,prgm_id,
     &                       case_id,prfl_id,
     &                       xmtr_id,freq,tlat,tlon,bearng,
     &                       pwr,incl,headng,talt,ralt,nravg,
     &                       plt_label,mxlbl,nrlbl)

                        if (nrcurv .gt. 1 .and. mxcurv .gt. 1) then

c                          Put label for headings
                           call GRF_STRING
     &                         (xlngth+.1,ylngth-sz,sz,'headng',0.,
     &                         'LB',retX)

                           dy=sz*1.5
                           xlbl(1)=xlngth-.6
                           xlbl(2)=xlngth-.1
                           ylbl(1)=ylngth-sz
                           ylbl(2)=ylngth-sz
                           xl=xlngth+.1
                        end if

c                       Set conversion factor for desired output
                        if (phs_units .eq. 0) then
c                          degrees
                           phs_conv=1.
                        else
c                          microseconds
                           phs_conv=1000./(360.*freq)
                        end if
                     end if

c                    Set line type and color
                     if (mxcurv .eq. 1) then
                        line =1
                        color='black'
                     else
                        line =line_type (MOD(nrcurv,mxcurv))
                        color=line_color(MOD(nrcurv,mxcurv))
                     end if

                     if (nrcurv .gt. 1 .and. mxcurv .gt. 1) then

c                       Label the headings
                        ylbl(1)=ylbl(1)-dy
                        ylbl(2)=ylbl(2)-dy
                        write(plblx,'(f5.1)') headng
                        call GRF_COLOR
     &                      (color)
                        call GRF_STRING
     &                      (xl,ylbl(1)-.5*dy,sz,plblx,0.,'LB',retX)
                        call GRF_CURVE
     &                      (xlbl,ylbl,2,0.,0.,1.,1.,line,20)
                     end if

                     call PLT_LWF
     &                   (print_lwf,phs_plot,phs_conv,color,line,
     &                    plt_label,mxlbl,nrlbl,
     &                    xmin,xscale,
     &                    ymin,ymax,yscale,
     &                    mxpts,nrcmp,nxmin,nxmax,dst,amp,phs)

                     if (MOD(nrcurv,mxcurv) .eq. 0 .or. nlwf .eq. nrlwf)
     &                  call GRF_DONE
                  end if
               end if

               nlwf=nlwf+1
            end do
         end do
      end if

      CLOSE(lu_lwf)

c     If the GO button has been pressed or printing the graphs,
c     immediately get next graph
      if (next_graph .eq. 3) go to 11

      if (next_graph .eq. 4) plt_device='sys-scn'

c     Output is to the screen,
c     return to FORDRIVE and await user's input.
      RETURN

90    write(error_msg,
     &    '(''Error '',i3,'' occurred trying to open '',
     &      ''file: '',a)') iocheck,file_name
      call LWPC_ERROR ('Error',error_msg)

99    CLOSE(lwpcINP_lun)
      CLOSE(lwpcLOG_lun)
      call CLOSE_FORDRIVE
      END
