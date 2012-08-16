      SUBROUTINE ALMNAC
     &          (year,month,day,UT,colat,xlon)

c***********************************************************************
c                   subroutine almnac
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     Oct 27, 1989

c  Function:
c     Returns the sub-solar point (SSP) for a specific date and UT

c  Parameters passed:
c     year          [i] year
c     month         [i] month number, Jan is 1
c     day           [i] day of the month
c     UT            [i] Universal time; hours

c  Parameters returned:
c     colat         [r] co-latitude    of the SSP; radians
c     xlon          [r] West longitude of the SSP; radians

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     cos
c     mod
c     sin

c  References:

c  Change History:

c*******************!***************************************************

      implicit real*8 (a-h,o-z)

      real     *  8 mdays(12)
      real     *  4 UT,colat,xlon
      integer       year,month,day
      dimension     a(6),b(5),c(5),d(5)
      data          mdays/  0.d0, 31.d0, 59.d0, 90.d0,120.d0,151.d0,
     &                    181.d0,212.d0,243.d0,273.d0,304.d0,334.d0/,
     &              a0/3.798d-1/,
     &              a/-2.30009d1,-3.802d-1,-1.55d-1,-7.6d-3,-2.5d-3,
     &                -4.d-4/,
     &              b/3.5354d0,3.02d-2,7.28d-2,3.2d-3,2.0d-3/,
     &              c/5.965d-1,-2.9502d0,-6.53d-2,-1.248d-1,-1.03d-2/,
     &              d/-7.3435d0,-9.4847d0,-3.083d-1,-1.747d-1,-1.59d-2/


      kyr=MOD(year,1900)
      kk=MOD(kyr,4)
      days=365*kk+.0078d0*(kyr-68)
      if (kk .ne. 0) days=days+1.d0
      date=days+mdays(month)+day+UT/24.d0
c     x=twopi*date/365.25
      x=date*.017202424d0
      cx=COS(x)
      sx=SIN(x)
      twocx=2.d0*cx
      c2x=twocx*cx-1.d0
      s2x=twocx*sx
      c3x=twocx*c2x-cx
      s3x=twocx*s2x-sx
      c4x=twocx*c3x-c2x
      s4x=twocx*s3x-s2x
      c5x=twocx*c4x-c3x
      s5x=twocx*s4x-s3x
      c6x=twocx*c5x-c4x
      dec =a0
     &    +a(1)*cx+a(2)*c2x+a(3)*c3x+a(4)*c4x+a(5)*c5x+a(6)*c6x
     &    +b(1)*sx+b(2)*s2x+b(3)*s3x+b(4)*s4x+b(5)*s5x
      eqnt=c(1)*cx+c(2)*c2x+c(3)*c3x+c(4)*c4x+c(5)*c5x
     &    +d(1)*sx+d(2)*s2x+d(3)*s3x+d(4)*s4x+d(5)*s5x

c     Convert to colat and longitude
      colat=(90.-dec)*1.745329252519949d-2
      xlon=((UT+eqnt/60.d0)*15.d0-180.d0)*1.745329252519949d-2

      RETURN
      END      ! ALMNAC