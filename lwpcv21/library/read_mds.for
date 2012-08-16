      SUBROUTINE READ_MDS
     &          (lu_mds,print_mds,
     &           bearng,rhomx,rlat,rlon,rrho,
     &           mxprm1,nrprm1,param1,
     &           mxeigen,nreigen,eigen,
     &           mxprm2,nrprm2,param2,
     &           begin_file,end_file)

c***********************************************************************
c                         subroutine read_mds
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 Apr 1990

c  Function:
c     Reads mode solution data.

c  Parameters passed:
c     lu_mds           [i] number of the logical unit; must already be
c                          openned by the calling routine
c     print_mds        [i] >0 prints parameters from passed data

c     mxprm1           [i] dimension of array PARAM1 in the calling
c                          routine
c     mxeigen          [i] dimension of array EIGEN in the calling
c                          routine
c     mxprm2           [i] dimension of array PARAM2 in the calling
c                          routine

c     begin_file       [l] indicates beginning of file; must be set to
c                          .TRUE. at  by the calling routine

c  Parameters returned:
c     bearng           [r] bearing of path; degrees E of N
c     rhomx            [r] maximum range of path; km
c     rlat             [r] receiver  latitude; degrees N
c     rlon             [r] receiver longitude; degrees W
c     rrho             [r] range to receiver; km

c     nrprm1           [i] number of PARAM1
c     param1           [r] path parameters (see below)

c     nreigen          [i] number of EIGEN
c     eigen            [c] mode solutions

c     nrprm2           [i] number of PARAM2
c     param2           [c] mode parameters (see below)

c     end_file         [l] indicates end of the input file; set to
c                          .TRUE. by this routine as required

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     print
c     read

c  References:

c  Change History:
c     20 Feb 92     Added profile index to PARAM1.

c     03 Jan 94     Added END= and ERR= to read statements to aid
c                   error handling.

c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c  Notes:
c     Wherever indicated, ranges are distances from the transmitter.

c     The file begins with a header record which identifies the program
c     which generated the data, the date the program was run,
c     transmitter parameters and the parameters of the paths. This
c     record is processed by the routine READ_HDR.

c     Subsequent records contain data for the individual segments of the
c     path followed by records for each mode within that segment. To
c     allow for future expansion, the path parameters are passed in an
c     array along with the number of elements in the array. This program
c     reads these parameters into the array, PARAM1, which is assumed to
c     contain the following:

c      1)lat        latitude;  degrees North
c      2)lon        longitude; degrees West
c      3)rho        range to the segment; km
c      4)azim       geomagnetic azimuth; degrees E of path direction
c      5)dip        geomagnetic dip; degrees below horizontal
c      6)bfield     geomagnetic intensity; Gauss
c      7)sigma      ground conductivity; Seimens
c      8)epsr       ground dielectric constant
c      9)beta       exponential ionospheric conductivity slope; km-1
c     10)hprime     exponential ionospheric conductivity height; km
c     11)pindex     profile index for non-exponential profiles

Change: 20 Feb 92   This parameter added
c     11)pindex     profile index for non-exponential profiles

c     The mode parameters are read into the complex arrays EIGEN and
c     PARAM2. The array PARAM2 contains the terms required to calculate
c     excitation factors: T1, T2, T3, T4 and f(R).

c     The last segment is indicated by a record with NRPRM1=1 and LAT=99

c     The mode generation programs sometimes experience problems and
c     do not successfully obtain mode solutions. These cases are
c     indicated by NREIGEN<1.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      logical       begin_file,end_file,new_path
      integer       lu_mds,print_mds,
     &              mxprm1,nrprm1,mxeigen,nreigen,mxprm2,nrprm2
      real     *  4 bearng,rhomx,rlat,rlon,rrho,param1(mxprm1)
      complex  *  8 eigen(mxeigen),param2(mxprm2,mxeigen)

      if (begin_file) then

c        At the start of the file
         begin_file=.false.
         end_file=.false.
         new_path=.true.
      end if

      read (lu_mds,end=99,err=98)
     &      bearng,rhomx,rlat,rlon,rrho,
     &      nrprm1,nreigen,nrprm2,
     &     (param1(i),i=1,nrprm1)

      if (nreigen .gt. 0)
     &   read (lu_mds,end=99,err=98)
     &        (eigen(m),(param2(i,m),i=1,nrprm2),m=1,nreigen)

      if (print_mds .gt. 0) then

         if (new_path) then

            write(lwpcLOG_lun,
     &          '(''bearng   rlat    rlon    rrho''/
     &              f6.1,f7.1,f8.1,f8.0)')
     &              bearng,rlat,rlon,rrho

            write(lwpcLOG_lun,
     &          '(''mds    lat     lon    rho    azim    dip '',
     &            ''bfield   sigma epsr  beta   hpr  ndx'')')

            new_path=.false.
         end if

         if (param1(1) .eq. 99.) then

            write(lwpcLOG_lun,'(3x,f7.1)') param1(1)
            new_path=.true.
         else

            write(lwpcLOG_lun,
     &          '(i3,f7.1,f8.1,f8.0,f7.1,f7.1,f7.3,1pe8.0,0pf5.0,
     &            f6.2,f6.1,f6.0)')
     &            nreigen,(param1(i),i=1,nrprm1)

            if (nreigen .gt. 0 .and. print_mds .gt. 1) then

               write(lwpcLOG_lun,'(''     m  EIGENr   EIGENi '')')
               neigen=1
               do while (neigen .le. nreigen)
                  write(lwpcLOG_lun,'(3x,i3,2f9.4,1p10e10.2)')
     &                  neigen,eigen(neigen),
     &                 (param2(i,neigen),i=1,nrprm2)
                  neigen=neigen+1
               end do
               write(lwpcLOG_lun,
     &             '(/''mds    lat     lon    rho    azim    dip '',
     &                ''bfield  sigma epsr   beta   hpr  ndx'')')
            end if
         end if
      end if

c     End of segment
      RETURN

c     Error reading file; incomplete segment

98    param1(1)=-99.

c     End of file

99    end_file=.true.

      RETURN
      END      ! READ_MDS