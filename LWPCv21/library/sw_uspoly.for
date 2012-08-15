      SUBROUTINE SW_USPOLY

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

      integer       use(mxeigen)
      real     *  4 dst(mxeigen)
      complex  * 16 theta,prod

      equivalence  (thetar,theta)


c     Distance from THETA to TLIST angles
      do i1=1,nreigen
         use(i1)=i1
         dst(i1)=ABS(theta-tlist(i1))
      end do

c     Order TLIST angles according to distance
      call SORTR (dst,nreigen,use,nreigen,1,nreigen)

c     Use only NRTLST angles
      nruse=MIN(nreigen,nrtlst)

      logr11=0.d0
      logr22=0.d0
      logr12=0.d0
      logr21=0.d0
      do j1=1,nruse
         i1=use(j1)
         prod=1.d0
         do j2=1,nruse
            i2=use(j2)
            if (i1 .ne. i2)
     &      prod=prod*(theta-tlist(i2))/(tlist(i1)-tlist(i2))
         end do
         logr11=logr11+lgmtrx(1,i1)*prod
         logr22=logr22+lgmtrx(2,i1)*prod
         logr12=logr12+lgmtrx(3,i1)*prod
         logr21=logr21+lgmtrx(4,i1)*prod
      end do
      r11=EXP(logr11)
      r22=EXP(logr22)
      r12=EXP(logr12)
      r21=EXP(logr21)
      RETURN
      END      ! SW_USPOLY