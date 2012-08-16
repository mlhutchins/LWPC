      SUBROUTINE PLT_LWF_LABEL
     &          (xlabel,ylabel,
     &           file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,tlat,tlon,bearng,
     &           power,incl,headng,talt,ralt,nravg,
     &           plt_label,mxlbl,nrlbl)

c***********************************************************************
c  Subroutine       plt_lwf_label
c***********************************************************************
c
c  Program Source:
c     Naval Ocean Systems Center - Code 542
c
c  Date:
c     26 Jan 1996
c
c  Function:
c     Places labels for LWF plots on a graph and returns them to the
c     calling routine.
c
c  Parameters passed:
c     xlabel        [r] position of the first and final line of the label
c     ylabel        [r]
c
c     file_id       [s] file identification
c     prgm_id       [s] program which generated the data
c     case_id       [s] user identification of the data
c     prfl_id       [s] ionospheric profile identification
c
c     xmtr_id       [s] transmitter identification
c     freq          [r] frequency
c     tlat          [r] transmitter latitude
c     tlon          [r] transmitter longitude
c     bearng        [r] bearing of the path
c
c     power         [r] transmitter power
c     incl          [r] antenna inclination
c     headng        [r] antenna heading
c     talt          [r] transmitter altitude
c     ralt          [r] receiver    altitude
c
c     nravg         [i] number of points for running average
c
c  Parameters returned:
c     plt_label     [s] array of data labels
c     mxlbl         [i] dimension of PLT_LABEL
c     nrlbl         [i] number    of PLT_LABELs generated
c
c  Common blocks referenced:
c     None
c
c  Functions and subroutines referenced:
c     None
c
c  References:
c     None
c
c  Change History:
c
c     12 Mar 1997   Modified to use new string routines in GRF.
c
c*******************!***************************************************

      character*(*) plt_label(mxlbl)
      character*  8 prgm_id
      character* 20 xmtr_id
      character* 40 prfl_id
      character* 60 frmt1,frmt2
      character* 80 case_id
      character*120 file_id(3)
      integer       str_length
      real          freq,tlat,tlon,bearng,
     &              power,incl,headng,talt,ralt,
     &              xlabel,ylabel


      numx=STR_LENGTH(xmtr_id)
      nums=numx-7
      if (numx .eq. 0) then
         write(frmt1,'(''(a,2x,a,a)'')')
         write(frmt2,'(''(7x,2f6.1,f7.1,f6.1,i5,i3,f6.1,2f5.1)'')')
      else
     &if (nums .eq. 0) then
         write(frmt1,'(''(a,2x,a,a)'')')
         write(frmt2,'(''(a,2f6.1,f7.1,f6.1,i5,i3,f6.1,2f5.1)'')')
      else
     &if (nums .lt. 0) then
         write(frmt1,'(''(a,2x,a,a)'')')
         write(frmt2,'(''(a,'',i2.2,''x,2f6.1,'',
     &                 ''f7.1,f6.1,i5,i3,f6.1,2f5.1)'')') -nums
      else
         write(frmt1,'(''(a,'',i2.2,''x,2x,a,a)'')') nums
         write(frmt2,'(''(a,2f6.1,f7.1,f6.1,i5,i3,f6.1,2f5.1)'')')
      end if

      nrlbl=1
      plt_label(nrlbl)=case_id
      nrlbl=nrlbl+1
      write(plt_label(nrlbl),frmt1)
     &     'xmtr_id',
     &     'freq  tlat   tlon  brng  pwr ',
     &     'in   hdg talt ralt'
      nrlbl=nrlbl+1
      if (numx .eq. 0) then
         write(plt_label(nrlbl),frmt2)
     &         freq,tlat,tlon,bearng,int(power),
     &         int(incl),headng,talt,ralt
      else
         write(plt_label(nrlbl),frmt2)
     &         xmtr_id(:numx),
     &         freq,tlat,tlon,bearng,int(power),
     &         int(incl),headng,talt,ralt
      end if
      nrlbl=nrlbl+1
      write(plt_label(nrlbl),'(''Prfl:  '',a)') prfl_id
      nrlbl=nrlbl+1
      write(plt_label(nrlbl),'(''File:  '',a)') file_id(1)
      nrlbl=nrlbl+1
      write(plt_label(nrlbl),'(''File:  '',a)') file_id(2)
      nrlbl=nrlbl+1
      write(plt_label(nrlbl),'(''Prgm:  '',a)') prgm_id

c     Add labels for running average if necessary
      if (nravg .gt. 1) then
         nrlbl=nrlbl+1
         if (nravg .lt. 10) then
            write(plt_label(nrlbl),
     &          '(''nAvg:  '',i1)') nravg
         else
            write(plt_label(nrlbl),
     &          '(''nAvg:  '',i2)') nravg
         end if
      end if

      xp=xlabel
      yp=ylabel

      do n=1,nrlbl
         call GRF_STRING (xp,yp,.1,plt_label(n),0.,'LB',retX)
         yp=yp-.15
      end do

c     Return the final label position
      xlabel=xp
      ylabel=yp

      RETURN
      END      ! PLT_LWF_LABEL