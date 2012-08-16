      SUBROUTINE SW_R DERIV

      implicit real*8 (a-h,o-z)

c***********************************************************************

c  Change History:
c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

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

      complex  * 16 k2i,capd,
     &              t11,t31,t42,t44,t12vrc,t14vrc,t32vrc,t34vrc,ct41,
     &              s11a,d11a,s11b,d11b,c12,c21,
     &              s12,d12,s21,d21,s22,d22,b11,b22,b12,b21


      k2i=DCMPLX(0.d0,-0.5d0*wn)

      if (DREAL(logr11) .gt.  15.d0) then
         logr11= 15.d0
      else
     &if (DREAL(logr11) .lt. -15.d0) then
         logr11=-15.d0
      end if
      r11=EXP(logr11)
      if (DREAL(logr22) .gt.  15.d0) then
         logr22= 15.d0
      else
     &if (DREAL(logr22) .lt. -15.d0) then
         logr22=-15.d0
      end if
      r22=EXP(logr22)
      if (DREAL(logr12) .gt.  15.d0) then
         logr12= 15.d0
      else
     &if (DREAL(logr12) .lt. -15.d0) then
         logr12=-15.d0
      end if
      r12=EXP(logr12)
      if (DREAL(logr21) .gt.  15.d0) then
         logr21= 15.d0
      else
     &if (DREAL(logr21) .lt. -15.d0) then
         logr21=-15.d0
      end if
      r21=EXP(logr21)

      capd=1.d0/(1.d0+m33)

      t11=-s*m31*capd
      t12vrc=s*m32*capd/c
      t14vrc=(csq+m33)*capd/c
      t31=m23*m31*capd-m21
      t32vrc=c+(m22-m23*m32*capd)/c
      t34vrc=s*m23*capd/c
      ct41=(1.d0+m11-m13*m31*capd)*c
      t42=m32*m13*capd-m12
      t44=-s*m13*capd

      s11a=t11+t44
      d11a=t11-t44
      s11b=t14vrc+ct41
      d11b=t14vrc-ct41
      s12=t12vrc+t42
      d12=t12vrc-t42
      s21=t34vrc+t31
      d21=t34vrc-t31
      s22=c+t32vrc
      d22=c-t32vrc

      b11=r11*(d11a-d11b)
      b22=r22*d22
      b12=r12*d21
      b21=r21*s12
      c12=r12*s21
      c21=r21*d12

      dl11dh=k2i*
     &      (b11+b12+b21-s11b-s11b+(r12*r21*d22+c12+c21-d11a-d11b)/r11)
      dl22dh=k2i*
     &      (b12+b21+b22-s22-s22+(r12*r21*(d11a-d11b)+b12+b21+d22)/r22)
      dl12dh=k2i*
     &      (b11+b12+b22+s11a-s11b-s22+(r11*s12+d12)*(1.d0+r22)/r12)
      dl21dh=k2i*
     &      (b11+b21+b22-s11a-s11b-s22+(r11*d21+s21)*(1.d0+r22)/r21)
      RETURN
      END      ! SW_DRV EQU