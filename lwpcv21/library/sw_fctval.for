      SUBROUTINE SW_FCT VAL

      implicit real*8 (a-h,o-z)

c***********************************************************************

c  Change History:
c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),
     &              thtinc,ftol,maxitr,alpha,prec,rpoly,nrtlst

      integer       adjflg
      complex  * 16 c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22

      common/sw_mode/
     &              omega,wn,thetar,thetai,c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22,
     &              nriter,adjflg,isotrp

      complex  * 16 r11,r22,r12,r21,
     &              logr11,logr22,logr12,logr21,
     &              dl11dh,dl22dh,dl12dh,dl21dh

      common/sw_rmtx/
     &              r11,r22,r12,r21,
     &              logr11,logr22,logr12,logr21,
     &              dl11dh,dl22dh,dl12dh,dl21dh,delh

      complex  * 16 theta

      equivalence  (thetar,theta)


      c=COS(theta*1.745329252d-2)
      csq=c*c
      s=SIN(theta*1.745329252d-2)
      ssq=s*s
      call SW_RBARS
      if (rpoly .eq. 0) then
         call SW_INTEG
      else
         call SW_US POLY
      end if
      f=(rbar11*r11-1.d0)*(rbar22*r22-1.d0)-rbar11*rbar22*r12*r21
      RETURN
      END      ! SW_FCT VAL