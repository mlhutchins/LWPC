      SUBROUTINE SW_INTEG

      implicit real*8 (a-h,o-z)

c***********************************************************************

c  Change History:
c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex

      real     *  4 hten,algen,htnu,algnu,charge,ratiom,
     &              select_hgts,hgts

      common/lwpc_pr/
     &              hten(51),algen(51,3),nrhten,ihten,
     &              htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &              charge(3),ratiom(3),nrspec,lu_prf,
     &              select_hgts(2),hgts(3)

      complex  * 16 m11,m12,m13,m21,m22,m23,m31,m32,m33

      common/sw_mmtx/
     &              m11,m12,m13,m21,m22,m23,m31,m32,m33,ht

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

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),
     &              thtinc,ftol,maxitr,alpha,prec,rpoly,nrtlst

      integer       sflag
      real     *  8 logr(8),logr0(8)
      complex  * 16 theta

      dimension     r(8),dlrdh(8),dlrdh0(8),
     &              dlogr0(8),dlogr1(8),dlogr2(8)

      equivalence  (thetar,theta),
     &             (r11,r),(logr11,logr),(dl11dh,dlrdh)

      data          dlhmin/1.953125d-3/,dlgrmx/1.d15/

      factor=10.d0**(-prec)
      emax=factor*3.d0
      emin=factor*.3d0
      ht=topht
      delh=3.125d-2
      svdelh=delh
      call SW_S MATRX
      call SW_INITL R
      call SW_R DERIV

c     Runge Kutta
10    sflag=0
      if (ht-delh .lt. hten(ihten+1)) then
         sflag=1
         saveht=hten(ihten+1)
         delh=ht-saveht
      end if
      if (ht-delh .lt. botht) then
         sflag=1
         saveht=botht
         delh=ht-saveht
      end if
      do i=1,8
         logr0(i)=logr(i)
         dlrdh0(i)=dlrdh(i)
      end do

c     Try again
40    do i=1,8
         dlogr0(i)=-dlrdh0(i)*delh
         logr(i)=logr0(i)+0.5d0*dlogr0(i)
      end do
      ht=ht-0.5d0*delh
      call SW_S MATRX
      call SW_R DERIV
      do i=1,8
C        Next four lines added to fix Watcom compiler problem...
         temp1 = ABS(dlrdh(i))
         temp2 = MIN(dlgrmx, temp1)
         temp3 = SIGN(temp2,dlrdh(i))
         dlrdh(i)=temp3
C        dlrdh(i)=SIGN(MIN(dlgrmx,ABS(dlrdh(i))),dlrdh(i))
         dlogr1(i)=-dlrdh(i)*delh
         logr(i)=logr0(i)+0.5d0*dlogr1(i)
      end do
      call SW_R DERIV
      do i=1,8
C        Next four lines added to fix Watcom compiler problem...
         temp1 = ABS(dlrdh(i))
         temp2 = MIN(dlgrmx, temp1)
         temp3 = SIGN(temp2,dlrdh(i))
         dlrdh(i)=temp3
C        dlrdh(i)=SIGN(MIN(dlgrmx,ABS(dlrdh(i))),dlrdh(i))
         dlogr2(i)=-dlrdh(i)*delh
         logr(i)=logr0(i)+dlogr2(i)
      end do
      ht=ht-0.5d0*delh
      call SW_S MATRX
      call SW_R DERIV
      error=0.d0
      do i=1,8
C        Next four lines added to fix Watcom compiler problem...
         temp1 = ABS(dlrdh(i))
         temp2 = MIN(dlgrmx, temp1)
         temp3 = SIGN(temp2,dlrdh(i))
         dlrdh(i)=temp3
C        dlrdh(i)=SIGN(MIN(dlgrmx,ABS(dlrdh(i))),dlrdh(i))
         dlogr4=((-dlrdh(i)*delh+dlogr0(i))/2.d0
     &           +dlogr1(i)+dlogr2(i))/3.d0
         logr(i)=logr0(i)+dlogr4
         error=error+MIN(ABS(dlogr2(i)-dlogr4),1.d15)**2
      end do
      error=SQRT(error/8.d0)
      if (error .lt. emax .or. delh .le. dlhmin) go to 100
      sflag=0
      ht=ht+delh
      delh=0.5d0*delh
      if (delh .lt. dlhmin) delh=dlhmin
      go to 40
100   call SW_R DERIV
      if (error .lt. emin) delh=2.*delh
      if (sflag .eq. 1) then
         delh=svdelh
         ht=saveht
      end if
      svdelh=delh
      if (ht .gt. botht) go to 10
      RETURN
      END      ! SW_INTEG