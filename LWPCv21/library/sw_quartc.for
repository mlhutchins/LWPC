      SUBROUTINE SW_QUARTC
     &          (q,b3,b2,b1,b0)

      implicit real*8 (a-h,o-z)

c***********************************************************************

c  Change History:
c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

      complex  * 16 b3,b2,b1,b0,q,b3sq,h,i,g,hprime,gprime,sqroot,
     &              p1,p2,cbert0,cbert1,cbert2,omega1,omega2,
     &              rootp,rootq,rootr,ctemp

      dimension     diff(4),q(4)

      data          omega1/(-5.d-1, 8.660254038d-1)/,
     &              omega2/(-5.d-1,-8.660254038d-1)/


      b3sq=b3**2
      h=b2-b3sq
      i=b0-(4.d0,0.d0)*b3*b1+(3.d0,0.d0)*b2**2
      g=b1+b3*((-3.d0,0.d0)*b2+(2.d0,0.d0)*b3sq)
      hprime=-i/(12.d0,0.d0)
      gprime=-g**2/(4.d0,0.d0)-h*(h**2+(3.d0,0.d0)*hprime)

      sqroot=SQRT(gprime**2+(4.d0,0.d0)*hprime**3)
      p1=(-.5d0,0.d0)*(gprime-sqroot)
      p2=(-.5d0,0.d0)*(gprime+sqroot)
      if (ABS(p1) .lt. ABS(p2)) p1=p2
      cbert0=EXP(LOG(p1)/(3.d0,0.d0))
      cbert1=omega1*cbert0
      cbert2=omega2*cbert0

      rootp=SQRT(cbert0-hprime/cbert0-h)
      rootq=SQRT(cbert1-hprime/cbert1-h)
      rootr=SQRT(cbert2-hprime/cbert2-h)
      if (ABS(g) .gt. 1.d-30) then
         sign=-rootp*rootq*rootr*(2.d0,0.d0)/g
         if (sign .lt. 0.d0) rootr=-rootr
      end if
      q(1)=+rootp+rootq+rootr-b3
      q(2)=+rootp-rootq-rootr-b3
      q(3)=-rootp+rootq-rootr-b3
      q(4)=-rootp-rootq+rootr-b3

      l=0
      do m=2,4
         do n=m,4
            if (DIMAG(q(n)) .le. 0.d0) then
               l=l+1
               ctemp=q(n)
               q(n)=q(m-1)
               q(m-1)=ctemp
            end if
         end do
      end do
      if (l .ne. 2) then
         do n=1,4
            angq=ATAN2(DIMAG(q(n)),DREAL(q(n)))*57.295779513d0
            if (angq .lt.   0.d0) angq=angq+360.d0
            if (angq .lt. 135.d0) angq=angq+360.d0
            diff(n)=ABS(angq-315.d0)
         end do
         do nm=2,4
            do n=nm,4
               if (diff(n) .le. diff(nm-1)) then
                  temp=diff(n)
                  diff(n)=diff(nm-1)
                  diff(nm-1)=temp
                  ctemp=q(n)
                  q(n)=q(nm-1)
                  q(nm-1)=ctemp
               end if
            end do
         end do
      end if
      RETURN
      END      ! SW_QUARTC