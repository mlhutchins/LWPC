      SUBROUTINE READ_HDR
     &          (lu_hdr,print_hdr,
     &           archive,file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,txlat,txlon,
     &           path_id,oplat1,oplon1,oplat2,oplon2,
     &           mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &           begin_file,end_file,
     &           data_format)

c***********************************************************************
c                         subroutine read_hdr
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     22 Dec 1992

c  Function:
c     Reads the header from MDS, LWF and GRD files.

c  Parameters passed:
c     lu_hdr           [i] number of the logical unit; must already be
c                          openned by the calling routine
c     print_hdr        [i] >0 prints parameters from passed data

c     mxpath           [r] dimension of the arrays BEARING, RHOMAX,
c                          RXLAT and RXLON in the calling routine

c     data_format      [s] 'b' if data is in binary format
c                          'a' if data is in ASCII format (i.e., GRD files)

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
c     str_length

c     max
c     print
c     read

c  References:

c  Change History:

c     04 Jan 94     Replaced ERR=900 with ERR=99 in read statements;
c                   deleted fatal exit if errors occur in header.

c     06 Apr 94     Changed from list directed to formatted output.

c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

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

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      character*(*) data_format
      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id(3)
      logical       begin_file,end_file
      integer       str_length,
     &              lu_hdr,print_hdr,mxpath,nrpath
      real     *  4 freq,txlat,txlon,
     &              oplat1,oplon1,oplat2,oplon2,
     &              bearing(mxpath),rhomax(mxpath),
     &              rxlat(mxpath),rxlon(mxpath)


      begin_file=.true.
      end_file=.false.

      if (data_format(1:1) .eq. 'A' .or.
     &    data_format(1:1) .eq. 'a') then

         read (lu_hdr,'(  a   )',end=99,err=99) archive
         read (lu_hdr,'(  a   )',end=99,err=99) file_id
         read (lu_hdr,'(  a   )',end=99,err=99) prgm_id
         read (lu_hdr,'(  a   )',end=99,err=99) case_id
         read (lu_hdr,'(  a   )',end=99,err=99) prfl_id
         read (lu_hdr,'(  a   )',end=99,err=99) xmtr_id
         read (lu_hdr,'( 9f9.2)',end=99,err=99) freq,txlat,txlon
         read (lu_hdr,'(  a   )',end=99,err=99) path_id
         read (lu_hdr,'( 9f9.2)',end=99,err=99) oplat1,oplon1,
     &                                          oplat2,oplon2
         read (lu_hdr,'(i6,8i9)',end=99,err=99) nrpath
         read (lu_hdr,'( 4f9.2)',end=99,err=99)(bearing(i),rhomax(i),
     &                                          rxlat  (i),rxlon (i),
     &                                          i=1,nrpath)
      else

         read (lu_hdr,           end=99,err=99) archive,
     &                                          file_id,
     &                                          prgm_id,
     &                                          case_id,
     &                                          prfl_id,
     &                                          xmtr_id,
     &                                          freq,txlat,txlon,
     &                                          path_id,
     &                                          oplat1,oplon1,
     &                                          oplat2,oplon2,
     &                                          nrpath,
     &                                         (bearing(i),rhomax(i),
     &                                          rxlat  (i),rxlon (i),
     &                                          i=1,nrpath)
      end if

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
      RETURN

c     End of file or error
99    end_file=.true.
      RETURN
      END      ! READ_HDR