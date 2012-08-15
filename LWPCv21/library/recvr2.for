      SUBROUTINE RECVR2
     &          (tlng,tclt,ctclt,stclt,br,gcd,rlng,rclt,crclt,srclt)

c***********************************************************************
c                   subroutine recvr
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     31 Mar 1989

c  Function:
c     Returns coordinates of a point (receiver) which is at a specified
c     great circle distance and bearing from the input transmitter.

c  Parameters passed:
c     tlng          [r] longitude   of transmitter
c     tclt          [r] co-latitude of transmitter
c     ctclt         [r] cosine of tclt
c     stclt         [r] sine   of tclt
c     br            [r] geographic bearing angle of receiver
c     gcd           [r] great circle distance to the receiver

c  Parameters returned:
c     rlng          [r] longitude   of receiver
c     rclt          [r] co-latitude of receiver
c     crclt         [r] cosine of rclt
c     srclt         [r] sine   of rclt

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs
c     acos
c     cos
c     min
c     mod
c     sqrt
c     sin

c  References:

c  Change History:
c     04 Nov 1991   Changed tolerances from 1e-6 to 1e-4; added special
c                   case for receiver at 40000 km.

c  Notes:
c     Sign convention is + for West and North.
c     All coordinates are in radians.

c*******************!***************************************************

      data          pi/3.1415926535898/,twopi/6.2831853071796/

      REDUCE(arg)=SIGN(MIN(ABS(arg),1.),arg)


      xtr=br
      rho=gcd
      if (ABS(xtr) .ge. twopi) xtr=MOD(xtr,twopi)
      if (xtr .lt. 0.) xtr=xtr+twopi
      if (rho .ge. pi) then
         rho=twopi-rho
         xtr=xtr+pi
         if (xtr .ge. twopi) xtr=xtr-twopi
      end if
      if (ABS(rho) .eq. 0.) then
c        Receiver is at the transmitter
         rclt=tclt
         rlng=tlng
         crclt=ctclt
         srclt=stclt
      else
     &if (ABS(tclt) .eq. 0.) then
c        Transmitter is at the North pole, so the receiver is due South
         rlng=tlng+xtr-pi
         if (rho .le. pi) then
            rclt=rho
         else
            rclt=twopi-rho
         end if
         crclt=COS(rclt)
         srclt=SIN(rclt)
      else
     &if (ABS(tclt-pi) .eq. 0.) then
c        Transmitter is at the South pole, so the receiver is due North
         rlng=tlng-xtr
         if (rho .le. pi) then
            rclt=pi-rho
         else
            rclt=rho-pi
         end if
         crclt=COS(rclt)
         srclt=SIN(rclt)
      else
     &if (ABS(xtr) .eq. 0.) then
c        Receiver is due North, possibly on opposite longitude
         rclt=tclt-rho
         if (rclt .ge. 0.) then
            rlng=tlng
         else
            rclt=-rclt
            rlng=tlng+pi
         end if
         crclt=COS(rclt)
         srclt=SIN(rclt)
      else
     &if (ABS(xtr-pi) .eq. 0. .or. ABS(rho-pi) .eq. 0.) then
c        Receiver is due South, possibly on opposite longitude
         rclt=tclt+rho
         if (rclt .lt. pi) then
            rlng=tlng
         else
            rclt=twopi-rclt
            rlng=tlng+pi
         end if
         crclt=COS(rclt)
         srclt=SIN(rclt)
      else
c        Not a special case
         crho=COS(rho)
         srho=SIN(rho)
         crclt=ctclt*crho+stclt*srho*COS(xtr)
         srclt=SQRT(1.-crclt**2)
         if (srclt .eq. 0.) then
            if (ABS(xtr) .lt. .5*pi .or. ABS(xtr) .gt. 1.5*pi) then
c              Receiver is due North, possibly on opposite longitude
               rclt=tclt-rho
               if (rclt .ge. 0.) then
                  rlng=tlng
               else
                  rclt=-rclt
                  rlng=tlng+pi
               end if
            else
c              Receiver is due South, possibly on opposite longitude
               rclt=tclt+rho
               if (rclt .lt. pi) then
                  rlng=tlng
               else
                  rclt=twopi-rclt
                  rlng=tlng+pi
               end if
            end if
         else
            rclt=ACOS(REDUCE(crclt))
            delta=ACOS(REDUCE((crho-ctclt*crclt)/(stclt*srclt)))
            if (xtr .lt. pi) delta=-delta
            rlng=tlng+delta
         end if
      end if

      if (rlng .gt.  pi) then
         rlng=rlng-twopi
      else
     &if (rlng .lt. -pi) then
         rlng=rlng+twopi
      end if

      RETURN
      END      ! RECVR2