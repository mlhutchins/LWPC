      SUBROUTINE WRITE_HDR
     &          (lu_hdr,print_hdr,
     &           archive,file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,txlat,txlon,
     &           path_id,oplat1,oplon1,oplat2,oplon2,
     &           mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &           begin_file,
     &           data_format)

c***********************************************************************
c                         subroutine write_hdr
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 Apr 1990

c  Function:
c     Writes the header for MDS, LWF and GRD files.

c  Parameters passed:
c     lu_hdr           [i] number of the logical unit; must already be
c                          openned by the calling routine
c     print_hdr        [i] >0 prints parameters from passed data

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

c     mxpath           [r] dimension of the arrays BEARING, RHOMAX,
c                          RXLAT and RXLON in the calling routine
c     nrpath           [i] number of paths
c     bearing          [r] bearing angles of paths; degrees E of N
c     rhomax           [r] maximum range  of paths; km
c     rxlat            [r] latitude   of receivers; degrees N
c     rxlon            [r] longitude  of receivers; degrees W

c     data_format      [s] 'A' if data file is ASCII (i.e., GRD file)
c                          'B' if data file is binary

c  Parameters returned:
c     begin_file       [l] indicates beginning of file; set to
c                          .TRUE. by this routine

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     str_length

c     max
c     print
c     write

c  References:

c  Change History:

c     11 Jan 94     Added call to flush the output buffer.

c     08 Feb 94     Added error checking on write statements.

c     06 Apr 94     Changed from list directed to formatted output.

c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     12 Dec 96     Modified to use new generic flush unit routine.

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
      logical       begin_file
      integer       lu_hdr,print_hdr,mxpath,nrpath
      real     *  4 freq,txlat,txlon,
     &              oplat1,oplon1,oplat2,oplon2,
     &              bearing(mxpath),rhomax(mxpath),
     &              rxlat(mxpath),rxlon(mxpath)

      character* 12 open_format,file_format
      character*200 open_name,error_msg
      logical       exists
      integer       str_length


      begin_file=.true.

      if (data_format(1:1) .eq. 'A' .or.
     &    data_format(1:1) .eq. 'a') then

         file_format='formatted'

         write(lu_hdr,'(  a   )',err=900) archive
         write(lu_hdr,'(  a   )',err=900) file_id
         write(lu_hdr,'(  a   )',err=900) prgm_id
         write(lu_hdr,'(  a   )',err=900) case_id
         write(lu_hdr,'(  a   )',err=900) prfl_id
         write(lu_hdr,'(  a   )',err=900) xmtr_id
         write(lu_hdr,'( 9f9.2)',err=900) freq,txlat,txlon
         write(lu_hdr,'(  a   )',err=900) path_id
         write(lu_hdr,'( 9f9.2)',err=900) oplat1,oplon1,
     &                                    oplat2,oplon2
         write(lu_hdr,'(i6,8i9)',err=900) nrpath
         write(lu_hdr,'( 4f9.2)',err=900)(bearing(i),rhomax(i),
     &                                    rxlat  (i),rxlon (i),
     &                                    i=1,nrpath)
      else

         file_format='unformatted'

         write(lu_hdr           ,err=900) archive,
     &                                    file_id,
     &                                    prgm_id,
     &                                    case_id,
     &                                    prfl_id,
     &                                    xmtr_id,
     &                                    freq,txlat,txlon,
     &                                    path_id,
     &                                    oplat1,oplon1,
     &                                    oplat2,oplon2,
     &                                    nrpath,
     &                                   (bearing(i),rhomax(i),
     &                                    rxlat  (i),rxlon (i),
     &                                    i=1,nrpath)
      end if

c     Flush the output buffer
      call FLUSH_UNIT (lu_hdr)

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

c     Error exits
900   INQUIRE (lu_hdr,opened=exists,name=open_name,form=open_format)
      if (exists) then

c        The file is open
         call STR_LOWER (open_name,0,0)
         call STR_LOWER (open_format,0,0)

         if ((open_format(1:1) .eq. 'f' .and.
     &        file_format(1:1) .eq. 'f') .or.
     &       (open_format(1:1) .eq. 'u' .and.
     &        file_format(1:1) .eq. 'u')) then

            write(error_msg,
     &          '(''[WRITE_HDR]: ''
     &            ''Writing file: '',a)')
     &              open_name(:STR_LENGTH(open_name))
         else

            write(error_msg,
     &          '(''[WRITE_HDR]: ''
     &            ''Format mismatch for file: '',a)')
     &              open_name(:STR_LENGTH(open_name))
         end if
         call LWPC_ERROR ('Error',error_msg)
      else

c        File has not been opened
         write(error_msg,
     &       '(''[WRITE_HDR]: '',
     &         ''Output file has not been opened'')')
         call LWPC_ERROR ('Error',error_msg)
      end if

      END      ! WRITE_HDR