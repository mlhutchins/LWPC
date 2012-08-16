      SUBROUTINE READ_GRD
     &          (lu_grd,print_grd,
     &           mxprm,nrprm,param,nrcmp,nrlwf,
     &           area_id,xlat1,xlon1,xlat2,xlon2,
     &           mxlat,nrlat,mxlon,nrlon,
     &           amp_grd,sig_grd,
     &           begin_file,end_file,
     &           data_format)

c***********************************************************************
c                         subroutine read_grd
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     22 December 1992

c  Function:
c     Reads grid data.

c  Parameters passed:
c     lu_grd           [i] number of the logical unit; must already be
c                          openned by the calling routine
c     print_grd        [i] >0 prints parameters from input records

c     mxprm            [i] dimension of array PARAM in calling routine
c     mxlat            [i] dimensions of arrays AMP_GRD and SIG_GRD in
c     mxlon            [i] the calling routine

c     begin_file       [l] indicates beginning of file; must be set to
c                          .TRUE. at  by the calling routine

c     data_format      [s] 'b' if data is in binary format
c                          'a' if data is in ASCII format

c  Parameters returned:
c     nrprm            [i] number of elements read into the array PARAM
c     param            [r] parameters (see below)
c     nrcmp            [i] number of components in the input file
c     nrlwf            [i] number of parametric cases in the input file

c     area_id          [s] op area identification
c     xlat1            [r] minimum latitude  of op area; degrees N
c     xlon1            [r] minimum longitude of op area; degrees W
c     xlat2            [r] maximum latitude  of op area; degrees N
c     xlon2            [r] maximum longitude of op area; degrees W

c     nrlat            [i] number of points in latitude
c     nrlon            [i] number of points in longitude
c     amp_grd          [r] array of amplitude of signal in the grid; dB
c     sig_grd          [r] array of standard deviation  in the grid; dB

c     end_file         [l] indicates end of the input file; set to
c                          .TRUE. by this routine as required

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     mod
c     print
c     read

c  References:

c  Change History:

c     29 Jan 94     Added ASCII/BINARY option.

c     06 Apr 94     Dropped printout of STNDEV; changed from list
c                   directed output to formatted output.

c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     30 Jan 96     Added coding for extra arrays to be read;
c                   specifically the atmospheric noise's V sub d.

c  Notes:
c     Wherever indicated, ranges are distances from the transmitter.

c     The file begins with a header record which identifies the program
c     which generated the data, the date the program was run,
c     transmitter parameters and the parameters of the paths. This
c     record is processed by the routine READ_HDR.

c     The next record identifies the op area by name and by its
c     boundaries.

c     area_id       op area id
c     xlat1         minimum latitude  of op area; degrees N
c     xlon1         minimum longitude of op area; degrees W
c     xlat2         maximum latitude  of op area; degrees N
c     xlon2         maximum longitude of op area; degrees W

c     nrlat         number of points in latitude
c     nrlon         number of points in longitude

c     nrcmp         number of components
c     nrlwf         number of parameteric variations (see LWFLDS)

c     The next record contains the parameters of the mode sum and grid
c     data. To allow for future expansion, the path parameters are
c     passed in an array along with the number of elements in the array.
c     This program reads these parameters into the array, PARAM, which
c     is assumed to contain the following:

c      1)power      power; kW
c      2)dist       distance; km
c      3)incl       inclination of the antenna; degrees from vertical
c      4)headng     heading     of the antenna; degrees E of N
c      5)talt       altitude of the transmitter; km
c      6)ralt       altitude of the    reciever; km
c      7)month      month number
c      8)day        day of the month
c      9)year       year
c     10)UT         Universal time; hours and minutes
c     11)bandw      band width of the receiver; Hz
c     12)adjny      adjustment to TM noise to get TE noise; dB
c     13)nrgrd      set to indicate the number of additional grid arrays
c                   in the file

c     The op area grid data are:

c     amp_grd       amplitude         ; dB
c     sig_grd       standard deviation; dB

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      character*(*) data_format
      character* 20 area_id
      logical       begin_file,end_file
      integer       lu_grd,print_grd,
     &              mxprm,nrprm,nrcmp,nrlwf,
     &              mxlat,nrlat,mxlon,nrlon
      real     *  4 param(mxprm),
     &              xlat1,xlon1,xlat2,xlon2,
     &              amp_grd(mxlon,mxlat),sig_grd(mxlon,mxlat)

c     LOCAL PARAMETERS
      integer       param_07,param_08,param_09,param_10,param_13


      if (begin_file) then

c        At the start of the file.
         begin_file=.false.
         end_file=.false.
         ngrd=0
         ncmp=0
         nlwf=0
      end if

      if (ncmp .eq. 0 .and. ngrd .eq. 0 .and. nlwf .eq. 0) then

c        Read area parameters
         if (data_format(1:1) .eq. 'A' .or.
     &       data_format(1:1) .eq. 'a') then

            read (lu_grd,'(  a   )',end=99) area_id
            read (lu_grd,'( 9f9.2)',end=99) xlat1,xlon1,
     &                                      xlat2,xlon2
            read (lu_grd,'(i6,8i9)',end=99) nrlon,nrlat,nrprm,
     &                                      nrcmp,nrlwf
         else

            read (lu_grd,           end=99) area_id,
     &                                      xlat1,xlon1,
     &                                      xlat2,xlon2,
     &                                      nrlon,nrlat,nrprm,
     &                                      nrcmp,nrlwf
         end if

         if (print_grd .gt. 0) then
            write(lwpcLOG_lun,
     &          '(''area_id               xlat1   xlon1  '',
     &            ''xlat2   xlon2  nlat nlon''/
     &              a20,2(f7.1,f8.1),i6,i5)')
     &              area_id,xlat1,xlon1,xlat2,xlon2,nrlat,nrlon

            write(lwpcLOG_lun,
     &          '(''nc power  incl  headng  talt  ralt  '',
     &            ''mn/dy/yr:UT    bandw  adjny Vsd'')')
         end if
      end if

c     Check if this is the first pass
      if (ngrd .eq. 0) then

         if (data_format(1:1) .eq. 'A' .or.
     &       data_format(1:1) .eq. 'a') then

            read (lu_grd,'(9f9.2)',end=99) (param  (i  ),i=1,nrprm)
            read (lu_grd,'(9f9.2)',end=99)((amp_grd(i,j),i=1,nrlon),
     &                                                   j=1,nrlat)
            read (lu_grd,'(9f9.2)',end=99)((sig_grd(i,j),i=1,nrlon),
     &                                                   j=1,nrlat)
         else

            read (lu_grd,          end=99) (param  (i  ),i=1,nrprm),
     &                                    ((amp_grd(i,j),i=1,nrlon),
     &                                                   j=1,nrlat),
     &                                    ((sig_grd(i,j),i=1,nrlon),
     &                                                   j=1,nrlat)
         end if

c        Set the number of extra arrays to be written
         if (nrprm .gt. 12) then
            param_13=param(13)
         else
            param_13=0
         end if
         nrgrd=2+param_13
         ngrd=2

         do i=nrprm+1,mxprm
            param(i)=0.
         end do

         if (print_grd .gt. 0) then
            if (ncmp .eq. 0) then
               param_07=param( 7)
               param_08=param( 8)
               param_09=param( 9)
               param_10=param(10)

c              Make sure that only the last two digits are to be printed
               param_09=MOD(param_09,100)

               write(lwpcLOG_lun,
     &             '(i2,f6.0,f6.0,f8.1,2f6.1,2x,
     &               2(i2.2,''/''),i2.2,'':'',i4.4,f7.0,f7.1,i4)')
     &               nrcmp,param(1),(param(i),i=3,6),
     &               param_07,param_08,param_09,param_10,
     &              (param(i),i=11,12),param_13
            end if
         end if
      else

         if (data_format(1:1) .eq. 'A' .or.
     &       data_format(1:1) .eq. 'a') then

            read (lu_grd,'(9f9.2)',end=99)((amp_grd(i,j),i=1,nrlon),
     &                                                   j=1,nrlat)
         else

            read (lu_grd,          end=99)((amp_grd(i,j),i=1,nrlon),
     &                                                   j=1,nrlat)
         end if
         ngrd=ngrd+1
      end if

      if (ngrd .eq. nrgrd) then
         ngrd=0
         ncmp=ncmp+1
         if (ncmp .eq. nrcmp) then
            ncmp=0
            nlwf=nlwf+1
            if (nlwf .ge. nrlwf) nlwf=0
         end if
      end if

c     End of current op area
      RETURN

c     End of file
99    end_file=.true.
      ngrd=0
      ncmp=0
      nlwf=0

      RETURN
      END      ! READ_GRD