      SUBROUTINE READ_LWF
     &          (lu_lwf,print_lwf,
     &           bearng,rhomx,rlat,rlon,rrho,
     &           mxps,nrps,mxsgmnt,nrsgmnt,sgmnt,
     &           mxprm,nrprm,param,nrcmp,nrlwf,
     &           mxpts,nrpts,xy,amp_lwf,phs_lwf,
     &           begin_file,end_file)

c***********************************************************************
c                         subroutine read_lwf
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 Apr 1990

c  Function:
c     Reads mode sum data.

c  Parameters passed:
c     lu_lwf           [i] number of the logical unit; must already be
c                          openned by the calling routine
c     print_lwf        [i] >0 prints parameters from passed data

c     mxps             [i] dimension of array SGMNT in calling routine
c     mxsgmnt          [i] dimension of array SGMNT in calling routine
c     mxprm            [i] dimension of array PARAM in calling routine

c     mxpts            [i] dimension of arrays XY, AMP_LWF and PHS_LWF

c     begin_file       [l] indicates beginning of file; must be set to
c                          .TRUE. at  by the calling routine

c  Parameters returned:
c     bearng           [r] bearing of path; degrees E of N
c     rhomx            [r] maximum range of path; km
c     rlat             [r] receiver  latitude; degrees N
c     rlon             [r] receiver longitude; degrees W
c     rrho             [r] range to receiver; km

c     nrps             [i] number of parameters passed in SGMNT
c     nrsgmnt          [i] number of segments passed in SGMNT
c     sgmnt            [r] path segmentation parameters (see below)

c     nrprm            [i] number of elements in the array PARAM
c     param            [r] parameters (see below)
c     nrcmp            [i] number of components to be read
c     nrlwf            [i] number of parametric cases to be read

c     nrpts            [i] number of points in XY, AMP_LWF and PHS_LWF
c     xy               [r] XY values for AMP_LWF and PHS_LWF
c     amp_lwf          [r] array of amplitude of signal; dB
c     phs_lwf          [r] array of relative phase; degrees

c     end_file         [l] indicates end of the input file; set to
c                          .TRUE. by this routine as required

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     min0
c     print
c     read

c  References:

c  Change History:
c     14 Mar 91     The parameter list may contain the date and time if
c                   LWPM was run for a specific date and time.

c     04 Dec 91     Added additional levels of print.

c     20 Feb 92     Added profile index to PARAM.

c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c  Notes:
c     Wherever indicated, ranges are distances from the transmitter.

c     The file begins with a header record which identifies the program
c     which generated the data, the date the program was run,
c     transmitter parameters and the parameters of the paths. This
c     record is processed by the routine READ_HDR.

c     Subsequent records contain data for the individual mode sums.
c     These mode sums are generated parametrically. To allow for future
c     expansion the parametric parameters are passed in a arrays along
c     with the number of elements in the array.

c     The first of these records contains information on the specific
c     path and a summary of the parameters of the segments used to
c     compute the mode sum. To allow for future expansion, the path
c     parameters are passed in an array along with the number of
c     elements in the array. This program reads these parameters into
c     a two dimensional array, SGMNT, which is assumed to contain the
c     following:

c      1)rho        range to the segment; km
c      2)lat        latitude;  degrees North
c      3)lon        longitude; degrees West
c      4)azim       geomagnetic azimuth; degrees E of path direction
c      5)dip        geomagnetic dip; degrees below horizontal
c      6)bfield     geomagnetic intensity; Gauss
c      7)sigma      ground conductivity; Seimens
c      8)epsr       ground dielectric constant
c      9)beta       exponential ionospheric conductivity slope; km-1
c     10)hprime     exponential ionospheric conductivity height; km

Change: 20 Feb 92   This parameter added
c     11)pindex     profile index for non-exponential profiles

Change: 06 Mar 92   This parameter modified by LW_FULL_MC
c     11)pindex     profile index+65536*(mc step index)

c     This record also contains the values of the parameters for which
c     the mode sums were generated. See LWFLDS for more details
c     regarding the following:

c     nrcmp         number of components
c     nrpts         number of points in primary output
c     nrlwf         number of parameteric variations

c     The next record contains the parameters of the mode sum and the
c     mode sum data. The parameters are read into the array, PARAM,
c     which is assumed to contain the following:

c      1)power      power; Kw
c      2)dist       distance; km
c      3)incl       inclination of the antenna; degrees from vertical
c      4)headng     heading     of the antenna; degrees E of N
c      5)talt       altitude of the transmitter; km
c      6)ralt       altitude of the    reciever; km

Change: 14 Mar 91   These parameters may be added by LW_VS_D
c      7)month      month number
c      8)day        day of the month
c      9)year       year
c     10)UT         Universal time; hours and minutes

c     The mode sum data are:

c     xy            value of the parameter which varies
c     amp           amplitude     ; dB above 1uv/m
c     phs           relative phase; degrees

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      character*  3 label
      logical       begin_file,end_file
      integer       lu_lwf,print_lwf,
     &              mxprm,nrprm,nrcmp,nrpts,nrlwf,
     &              mxps,nrps,mxsgmnt,nrsgmnt,mxpts
      real     *  4 bearng,rhomx,rlat,rlon,rrho,
     &              sgmnt(mxps,mxsgmnt),param(mxprm),
     &              xy(mxpts),amp_lwf(mxpts),phs_lwf(mxpts)

      data          label/'zyx'/


      if (begin_file) then

c        At the start of the file
         begin_file=.false.
         end_file=.false.
         ncmp=0
         if (print_lwf .eq. 1) then

c           Heading for summary output
            write(lwpcLOG_lun,
     &          '(''nc nrpt bearng  rhomx  rlat   rlon   rrho    '',
     &            ''pwr    dist incl headng talt ralt'')')
         end if
      end if

      if (ncmp .eq. 0) then

c        Read path parameters
         read (lu_lwf,end=99) bearng,rhomx,rlat,rlon,rrho,
     &                        nrps,nrsgmnt,nrprm,nrpts,nrcmp,nrlwf,
     &                      ((sgmnt(i,j),i=1,nrps),j=1,nrsgmnt)
         nlwf=0
      end if

      do i=1,mxprm
         param(i)=0.
      end do
      read (lu_lwf) (param(i),i=1,nrprm),
     &              (xy(i),amp_lwf(i),phs_lwf(i),i=1,nrpts)
      ncmp=ncmp+1

      if (print_lwf .eq. 1) then

c        Summary output
         if (ncmp .eq. 1) then
            write(lwpcLOG_lun,
     &          '(i2,i5,f7.1,f7.0,f6.1,f7.1,f7.0,
     &            f7.0,f8.0,f5.0,f7.1,2f5.1)')
     &            nrcmp,nrpts,bearng,rhomx,rlat,rlon,rrho,
     &           (param(i),i=1,MIN0(nrprm,6))
         end if
      else
     &if (print_lwf .gt. 1) then

c        Heading for detailed outputs
         write(lwpcLOG_lun,
     &       '(''nc nrpt bearng  rhomx  rlat   rlon   rrho    '',
     &         ''pwr    dist incl headng talt ralt'')')
         write(lwpcLOG_lun,
     &       '(i2,i5,f7.1,f7.0,f6.1,f7.1,f7.0,
     &         f7.0,f8.0,f5.0,f7.1,2f5.1)')
     &         nrcmp,nrpts,bearng,rhomx,rlat,rlon,rrho,
     &        (param(i),i=1,MIN0(nrprm,6))

         if (print_lwf .eq. 2) then

c           Output vs range
            write(lwpcLOG_lun,
     &          '(a,'' component''/
     &          3(''  dist   amplitude  phase  ''))')
     &            label(ncmp:ncmp)
            nl=(nrpts-1)/3+1
            do i1=1,nl
               write(lwpcLOG_lun,
     &             '(3(f7.0,2f10.4))')
     &              (xy(i),amp_lwf(i),phs_lwf(i),i=i1,nrpts,nl)
            end do
         else
     &   if (print_lwf .eq. 3) then

c           Output at receiver only
            if (ncmp .eq. 1) then
               write(lwpcLOG_lun,
     &             '(''E  dist   amplitude  phase  ''))')
            end if
            write(lwpcLOG_lun,
     &          '(a,f7.0,2f10.4)')
     &            label(ncmp:ncmp),
     &            xy(nrpts),amp_lwf(nrpts),phs_lwf(nrpts)
         end if
      end if

      if (ncmp .eq. nrcmp) then
         ncmp=0
         nlwf=nlwf+1
         if (nlwf .ge. nrlwf) nlwf=0
      end if

c     End of path
      RETURN

c     End of file
99    end_file=.true.
      nlwf=0

      RETURN
      END      ! READ_LWF