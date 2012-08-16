      SUBROUTINE READ_C_GRD
     &          (lu_grd,
     &           print_hdr,print_grd,
     &           archive,file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,txlat,txlon,
     &           path_id,oplat1,oplon1,oplat2,oplon2,
     &           nrpath,bearing,rhomax,rxlat,rxlon,
     &           nrprm,nrcmp,nrlwf,param,
     &           areaidx,xlat1x,xlon1x,xlat2x,xlon2x,
     &           nrlat,nrlon,
     &           amp_grd,sig_grd,
     &           begin_file,end_file)

c***********************************************************************
c                         subroutine read_c_grd
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     22 December 1992

c  Function:
c     Reads the header and one set of data from MDS, LWF and GRD files.
c     The files are assumed to be binary.

c  Parameters passed:
c     lu_grd           [i] number of the logical unit; must already be
c                          openned by the calling routine
c     print_grd        [i] >0 prints parameters from passed data

c  Parameters returned:
c     archive          [s] archive data
c     file_id(1)       [s] MDS file identification
c     file_id(2)       [s] LWF file identification
c     file_id(3)       [s] HDR file identification
c     prgm_id          [s] program id
c     case_id          [s] case id
c     prfl_id          [s] profile id

c     xmtr_id          [s] transmitter id
c     freq             [r] frequency; kHz
c     txlat            [r] transmitter latitude;  degrees N
c     txlon            [r] transmitter longitude; degrees W

c     path_id          [s] identifies method used to set up paths
c     oplat1           [r] minimum latitude  of op area; degrees N
c     oplon1           [r] minimum longitude of op area; degrees W
c     oplat2           [r] maximum latitude  of op area; degrees N
c     oplon2           [r] maximum longitude of op area; degrees W

c     nrpath           [i] number of paths
c     bearing          [r] bearing angles of paths; degrees E of N
c     rhomax           [r] maximum range  of paths; km
c     rxlat            [r] latitude   of receivers; degrees N
c     rxlon            [r] longitude  of receivers; degrees W

c     begin_file       [l] indicates beginning of file; set to
c                          .TRUE. by this routine

c     end_file         [l] indicates end of the input file; set to
c                          .FALSE. by this routine

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     max
c     read
c     write

c     str_length

c  References:

c  Notes:
c     All files begin with a header record which identifies the program
c     which generated the data, the date the program was run,
c     transmitter parameters and the parameters of the paths.

c     Wherever indicated, ranges are distances from the transmitter.

c     PATH_ID can have one of three values:
c        AREA_ID    The identification of the op area defined by
c                   OPLAT1, OPLON1, OPLAT2 and OPLON2.

c        Bearings   Indicates that the paths were defined by
c                   specification of a list of bearing angles.

c        Receivers  Indicates that the paths were defined by
c                   specification of a list of receiver positions.

c  Change History:

c     04 Jan 1994   Replaced ERR=900 with ERR=99 in read statements;
c                   deleted fatal exit if errors occur in header.

c     06 Apr 1994   Changed from list directed to formatted output.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'
      include      'grd_data.cmn'

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id,areaidx
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id(3)
      logical       begin_file,end_file
      integer       print_hdr,print_grd,nrpath,
     &              str_length
      real     *  4 freq,txlat,txlon,
     &              oplat1,oplon1,oplat2,oplon2,
     &              param(max_params),
     &              bearing(max_path),rhomax(max_path),
     &              rxlat(max_path),rxlon(max_path),
     &              amp_grd(max_lons,max_lats),
     &              sig_grd(max_lons,max_lats)

c     LOCAL PARAMETERS
      integer       param_07,param_08,param_09,param_10,param_13

      read (lu_grd,iostat=iocode,err=900)
     &         grddata_archive,
     &         grddata_mds_status,grddata_lwf_status,grddata_grd_status,
     &         grddata_program_id,grddata_case_id,   grddata_profile_id,
     &         grddata_xmtr_id,   grddata_path_id,   grddata_area_id,
     &         grddata_txfreq,
     &         grddata_txlat,     grddata_txlon,
     &         grddata_oplat1,    grddata_oplon1,
     &         grddata_oplat2,    grddata_oplon2,
     &         grddata_nrpath,    grddata_bearing,   grddata_rhomax,
     &         grddata_rxlat,     grddata_rxlon,
     &         grddata_xlat1,     grddata_xlon1,
     &         grddata_xlat2,     grddata_xlon2,
     &         grddata_nrlon,     grddata_nrlat,
     &         grddata_nrprm,     grddata_nrcmp,
     &         grddata_nrlwf,     grddata_params,
     &         grddata_amp_grd,   grddata_sig_grd

c     Transfer data into approricate return slots
      archive   =grddata_archive   (:  8)
      file_id(1)=grddata_mds_status(:120)
      file_id(2)=grddata_lwf_status(:120)
      file_id(3)=grddata_grd_status(:120)
      prgm_id   =grddata_program_id(:  8)
      case_id   =grddata_case_id   (: 80)
      prfl_id   =grddata_profile_id(: 40)
      xmtr_id   =grddata_xmtr_id   (: 20)
      freq      =grddata_txfreq
      txlat     =grddata_txlat
      txlon     =grddata_txlon
      path_id   =grddata_path_id   (: 20)
      oplat1    =grddata_oplat1
      oplon1    =grddata_oplon1
      oplat2    =grddata_oplat2
      oplon2    =grddata_oplon2
      nrpath    =grddata_nrpath
      do i=1,grddata_nrpath
         bearing(i)=grddata_bearing(i)
         rhomax (i)=grddata_rhomax (i)
         rxlat  (i)=grddata_rxlat  (i)
         rxlon  (i)=grddata_rxlon  (i)
      end do

      nrprm=grddata_nrprm
      do i=1,nrprm
         param(i)=grddata_params(i)
      end do
      nrcmp  =grddata_nrcmp
      nrlwf  =grddata_nrlwf
      areaidx=grddata_area_id
      xlat1x =grddata_xlat1
      xlon1x =grddata_xlon1
      xlat2x =grddata_xlat2
      xlon2x =grddata_xlon2
      nrlat  =grddata_nrlat
      nrlon  =grddata_nrlon
      do i=1,nrlon
         do j=1,nrlat
            amp_grd(i,j)=grddata_amp_grd(i,j)*.01
            sig_grd(i,j)=grddata_sig_grd(i,j)*.01
         end do
      end do

      begin_file=.true.
      end_file=.false.

      if (print_hdr .gt. 0) then

         write(lwpcLOG_lun,
     &       '(''file_id:  '',a/10x,a/10x,a/
     &         ''prgm_id:  '',a/
     &         ''case_id:  '',a/
     &         ''prfl_id:  '',a)')
     &           file_id(1)(:MAX(1,STR_LENGTH(file_id(1)))),
     &           file_id(2)(:MAX(1,STR_LENGTH(file_id(2)))),
     &           file_id(3)(:MAX(1,STR_LENGTH(file_id(3)))),
     &           prgm_id,
     &           case_id   (:MAX(1,STR_LENGTH(case_id))),
     &           prfl_id

         write(lwpcLOG_lun,
     &       '(''xmtr_id                tlat    tlon   freq''/
     &           a20,f7.1,f8.1,f7.1)')
     &           xmtr_id,txlat,txlon,freq

         if (oplat1 .eq. 99.) then
            write(lwpcLOG_lun,
     &          '(''path_id:  '',a)')
     &              path_id
         else
            write(lwpcLOG_lun,
     &          '(''path_id                lat1    lon1   '',
     &            ''lat2    lon2''/a20,2(f7.1,f8.1))')
     &              path_id,oplat1,oplon1,oplat2,oplon2
         end if
      end if

      if (print_grd .gt. 0) then

         write(lwpcLOG_lun,
     &       '(''area_id               xlat1   xlon1  '',
     &         ''xlat2   xlon2  nlat nlon''/
     &           a20,2(f7.1,f8.1),i6,i5)')
     &           areaidx,xlat1x,xlon1x,xlat2x,xlon2x,nrlat,nrlon

         write(lwpcLOG_lun,
     &       '(''nc power  incl  headng  talt  ralt  '',
     &         ''mn/dy/yr:UT    bandw  adjny Vsd'')')

         param_07=param( 7)
         param_08=param( 8)
         param_09=param( 9)
         param_10=param(10)
         param_13=param(13)

c        Make sure that only the last two digits are to be printed
         param_09=MOD(param_09,100)

         write(lwpcLOG_lun,
     &       '(i2,f6.0,f6.0,f8.1,2f6.1,2x,
     &         2(i2.2,''/''),i2.2,'':'',i4.4,f7.0,f7.1,i4)')
     &         nrcmp,param(1),(param(i),i=3,6),
     &         param_07,param_08,param_09,param_10,
     &        (param(i),i=11,12),param_13
      end if
      RETURN

c     End of file or error
900   end_file=.true.
      RETURN
      END      ! READ_C_GRD