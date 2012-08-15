      SUBROUTINE SW_RPLYNM

      implicit real*8 (a-h,o-z)

c***********************************************************************

c  Change History:
c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

      parameter    (mxeigen=50)

      real     *  4 ranger,rangei,atnmax,lub,h
      complex  *  8 eigen

      common/lwpc_mf/
     &              ranger(2),rangei(2),atnmax,lub,h,
     &              eigen(mxeigen),nreigen

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

      complex  * 16 tlist,lgmtrx

      common/sw_rply/
     &              tlist(mxeigen),lgmtrx(4,mxeigen)

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),
     &              thtinc,ftol,maxitr,alpha,prec,rpoly,nrtlst

      character*200 error_msg
      complex  * 16 theta

      equivalence  (thetar,theta)


      if (nreigen .le. 1) then
         write(error_msg,
     &       '(''[SW_RPLYNM]: Insufficient EIGEN list'')')
         call LWPC_ERROR('ERROR',error_msg)
      else
         do m=1,nreigen
            theta=eigen(m)
            c=COS(theta*1.745329252d-2)
            csq=c*c
            s=SIN(theta*1.745329252d-2)
            ssq=s*s
            call SW_INTEG
            tlist (  m)=theta
            lgmtrx(1,m)=logr11
            lgmtrx(2,m)=logr22
            lgmtrx(3,m)=logr12
            lgmtrx(4,m)=logr21
            adjflg=1
         end do
         adjflg=0
      end if
      RETURN
      END      ! SW_RPLYNM