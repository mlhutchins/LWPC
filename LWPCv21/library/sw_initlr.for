      SUBROUTINE SW_INITL R

      implicit real*8 (a-h,o-z)

c***********************************************************************

c  Change History:
c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

      integer       adjflg
      complex  * 16 c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22

      common/sw_mode/
     &              omega,wn,thetar,thetai,c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22,
     &              nriter,adjflg,isotrp

      complex  * 16 m11,m12,m13,m21,m22,m23,m31,m32,m33

      common/sw_mmtx/
     &              m11,m12,m13,m21,m22,m23,m31,m32,m33,ht

      complex  * 16 r11,r22,r12,r21,
     &              logr11,logr22,logr12,logr21,
     &              dl11dh,dl22dh,dl12dh,dl21dh

      common/sw_rmtx/
     &              r11,r22,r12,r21,
     &              logr11,logr22,logr12,logr21,
     &              dl11dh,dl22dh,dl12dh,dl21dh,delh

      complex  * 16 theta,
     &              q,p,t,d11,d13,d31,d33,delta,fnsq,froot,
     &              com1,com3,csqm22,csqm33,b3,b2,b1,b0

      dimension     phs1(8),phs2(8),p(2),t(2),q(4)

      equivalence  (thetar,theta),(logr11,phs1(1))

      data          pi/3.141592653d0/,twopi/6.283185307d0/


      if (isotrp .eq. 0) then

         com1=(1.d0,0.d0)+m11
         com3=(1.d0,0.d0)+m33
         csqm22=csq+m22
         csqm33=csq+m33

         b3=0.25d0*s*(m13+m31)/com3
         b2=(-csqm33*com1+m13*m31-com3*csqm22+m23*m32)/(6.d0*com3)
         b1=s*(m12*m23+m32*m21-csqm22*(m13+m31))/(4.d0*com3)
         b0=(com1*csqm22*csqm33+m12*m23*m31+m32*m21*m13-m13*m31*csqm22
     &      -com1*m23*m32-m12*m21*csqm33)/com3

         call SW_QUARTC (q,b3,b2,b1,b0)

         do n=1,2
            d11=(1.d0,0.d0)+m11-q(n)**2
            d13=m13+s*q(n)
            d31=m31+s*q(n)
            d33=(1.d0,0.d0)+m33-s**2
            delta=d11*d33-d13*d31
            p(n)=(-m12*d33+d13*m32)/delta
            t(n)=q(n)*p(n)-s*(-d11*m32+m12*d31)/delta
         end do
         delta=(t(1)*c+p(1))*(c+q(2))-(t(2)*c+p(2))*(c+q(1))
         r11=((t(1)*c-p(1))*(c+q(2))-(t(2)*c-p(2))*(c+q(1)))/delta
         r22=((t(1)*c+p(1))*(c-q(2))-(t(2)*c+p(2))*(c-q(1)))/delta
         r12=-2.d0*c*(t(1)*p(2)-t(2)*p(1))/delta
         r21=-2.d0*c*(q(1)-q(2))/delta
      else

         if (isotrp .eq. 1) then

            fnsq=(1.d0,0.d0)+m11
            froot=SQRT(csq+m11)
            if (DIMAG(froot) .gt. 0.d0) froot=-froot
            r11=(fnsq*c-froot)/(fnsq*c+froot)
            r22=(c-froot)/(c+froot)
         else

            fnsq=(1.d0,0.d0)+m11
            froot=SQRT(csq+m11+m13**2/fnsq)
            if (DIMAG(froot) .gt. 0.d0) froot=-froot
            com1=(s*froot+m13)/(s*fnsq+m13)
            r11=(c-com1)/(c+com1)
            froot=SQRT(csq+m22)
            if (DIMAG(froot) .gt. 0.d0) froot=-froot
            r22=(c-froot)/(c+froot)
         end if
         r12=(1.d-20,0.d0)
         r21=(1.d-20,0.d0)
      end if

      logr11=LOG(r11)
      logr12=LOG(r12)
      logr21=LOG(r21)
      logr22=LOG(r22)
      if (adjflg .eq. 1) then
         do n=2,8,2
            do while (phs1(n)-phs2(n) .gt. pi)
               phs1(n)=phs1(n)-twopi
            end do
            do while (phs2(n)-phs1(n) .gt. pi)
               phs1(n)=phs1(n)+twopi
            end do
         end do
      end if
      do n=2,8,2
         phs2(n)=phs1(n)
      end do
      RETURN
      END      ! SW_INITL R