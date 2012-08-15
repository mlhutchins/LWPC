      SUBROUTINE MF_FSINTG

c***********************************************************************

c     This routine performs an integration of the differential equations
c     for the ionosphere reflection matrix through a free space region
c     over a curved earth.  The integration may be performed in either
c     a positive or negative height direction, but in this program the
c     integration is always upward.  The integration variables are the
c     elements of the matrix (R+1)/C where R is the reflection matrix
c     described by Budden.  The solution is based on Budden, Radio Waves
c     in the Ionosphere, in particular the material on pp.118, 327-329,
c     336-338 and 343-345.

c     If IDERIV is set non-zero, the derivatives of (R+1)/C elements
c     wrt c=COS(THETA) are also integration variables.  Computation at
c     THETA=90 is not excluded.  Also, the equations are formulated in
c     so that a smooth transition is made from the 'curved earth' form
c     to the 'flat earth' form for appropriate values of THETA.  The
c     initial and final values of the integration variables are stored
c     in the common area 'MF RNTG'.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxeigen=50)

      integer        rprnt,xprnt
      real           lub,kvratt,n0sq,nzsq
      complex        eigen,theta,c,s,csq,ssq,
     &               r11,r21,r12,r22,dr11,dr21,dr12,dr22,
     &               drdh,roe,rnd,
     &               k1,k2,k10,k1z,k20,k2z,a0,az,
     &               f,f0,fz,f0z,dq,df,df0,dfz,df0z,
     &               q0,h10,h20,h10p,h20p,e0,
     &               qz,h1z,h2z,h1zp,h2zp,ez,
     &               ep1,ep2,ey1,ey2,dep1,dep2,dey1,dey2,
     &               den,v1,v2,v3,v4,dden,dv1,dv2,dv3,dv4

      common/lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rntg/r11,r21,r12,r22,dr11,dr21,dr12,dr22,drdh(8),
     &               roe(8),rnd(5)

      data           re/6369.427/


      if (ABS(zz-z0) .lt. 0.001) RETURN

      if (xprnt .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_FSINTG:  z0'',f6.2,'' r='',4(2x,2f12.5))')
     &           z0,r11,r21,r12,r22
         if (ideriv .ne. 0) then
            write(lwpcLOG_lun,
     &          '(''MF_FSINTG:      derivs='',4(2x,1p2e12.5))')
     &              dr11,dr21,dr12,dr22
         end if
      end if

      arg=2./(wavenr*re)
      avrkot=EXP(LOG(arg)/3.)
      kvratt=1./avrkot**2
      k1=CMPLX(0.,avrkot)
      k2=CMPLX(0.,.5*arg)

      em0=2.*(z0-h)/re
      n0sq=1.+em0
      k10=k1/n0sq
      k20=k2/n0sq
      emz=2.*(zz-h)/re
      nzsq=1.+emz
      k1z=k1/nzsq
      k2z=k2/nzsq

      c=COS(theta*(.01745329252,0.))
      s=SIN(theta*(.01745329252,0.))
      csq=c**2

      a0=c+k20
      az=c+k2z

      dq=(2.*kvratt)*c

c     Computation of height-gain coefficients for two conditions on the
c     upgoing wave at height=z0; namely, e11=1, ey=0 and e11=0, ey=1.

      q0=kvratt*(csq+em0)

c     Computation of upgoing fields e11 and ey at height=zz for the two
c     conditions described above.

      qz=kvratt*(csq+emz)

      if (REAL(q0) .le. 0.0 .and. REAL(qz) .le. 0.0 .and.
     &    ABS(q0) .gt. 4.2 .and. ABS(qz) .gt. 4.2) then
         ngboth = 1
      else
         ngboth = 0
      end if

      call MF_MDHNKL (q0,h10,h20,h10p,h20p,e0,ngboth)
      call MF_MDHNKL (qz,h1z,h2z,h1zp,h2zp,ez,ngboth)

      if (xprnt .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_FSINTG:  q0'',2(1pe12.3,e11.3)/
     &         ''              '',5(1pe12.3,e11.3))')
     &           q0,e0,h10,h20,h10p,h20p
         write(lwpcLOG_lun,
     &       '(''MF_FSINTG:  qz'',2(1pe12.3,e11.3)/
     &         ''              '',5(1pe12.3,e11.3))')
     &           qz,ez,h1z,h2z,h1zp,h2zp
      end if

      h1z =h1z *EXP(e0-ez)
      h1zp=h1zp*EXP(e0-ez)
      h2z =h2z *EXP(ez-e0)
      h2zp=h2zp*EXP(ez-e0)

      f  =h10 *h2z -h20 *h1z
      f0 =h10p*h2z -h20p*h1z
      fz =h10 *h2zp-h20 *h1zp
      f0z=h10p*h2zp-h20p*h1zp

      df  = (f0+fz)*dq
      df0 = (f0z-q0*f)*dq
      dfz = (f0z-qz*f)*dq
      df0z=-(qz*f0+q0*fz)*dq

      ep1=-.5*r11*(a0*az*f+k1z*a0*fz+k10*az*f0+k10*k1z*f0z)+az*f+k1z*fz
      ep2=-.5*r12*(a0*az*f+k1z*a0*fz+k10*az*f0+k10*k1z*f0z)
      ey1=-.5*r21*( c* c*f+k1 * c*fz+k1 * c*f0+k1 *k1 *f0z)
      ey2=-.5*r22*( c* c*f+k1 * c*fz+k1 * c*f0+k1 *k1 *f0z)+ c*f+k1 *fz
      den=ep1*ey2-ep2*ey1

      if (ideriv .eq. 1) then
         dep1=-.5*dr11*(a0*az*f+k1z*a0*fz+k10*az*f0+k10*k1z*f0z)
     &        -.5*r11*((a0*f+az*f+a0*az*df)+k1z*(fz+a0*dfz)
     &                 +k10*(f0+az*df0)+k10*k1z*df0z)+(f+az*df)+k1z*dfz
         dep2=-.5*dr12*(a0*az*f+k1z*a0*fz+k10*az*f0+k10*k1z*f0z)
     &        -.5*r12*((a0*f+az*f+a0*az*df)+k1z*(fz+a0*dfz)
     &                 +k10*(f0+az*df0)+k10*k1z*df0z)
         dey1=-.5*dr21*( c* c*f+k1 * c*fz+k1 * c*f0+k1 *k1 *f0z)
     &        -.5*r21*(( c*f+ c*f+ c* c*df)+k1 *(fz+ c*dfz)
     &                 +k1 *(f0+ c*df0)+k1 *k1 *df0z)
         dey2=-.5*dr22*( c* c*f+k1 * c*fz+k1 * c*f0+k1 *k1 *f0z)
     &                 +(f+ c*df)+k1 *dfz
     &        -.5*r22*(( c*f+ c*f+ c* c*df)+k1 *(fz+ c*dfz)
     &                 +k1 *(f0+ c*df0)+k1 *k1 *df0z)
         dden=ep1*dey2+dep1*ey2-ep2*dey1-dep2*ey1
      end if

      if (xprnt .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_FSINTG:  ep'',5(1pe12.3,e11.3))')
     &           ep1,ep2,ey1,ey2,den
         if (ideriv .eq. 1) then
            write(lwpcLOG_lun,
     &          '(''MF_FSINTG: dep'',5(1pe12.3,e11.3))')
     &              dep1,dep2,dey1,dey2,dden
         end if
      end if

c     Computation of reflection coefficients at height=zz.
      v1=-r11*(a0*f+k10*f0)+2.*f
      v2=-r12*(a0*f+k10*f0)
      v3=-r21*( c*f+k1 *f0)
      v4=-r22*( c*f+k1 *f0)+2.*f

      if (ideriv .eq. 1) then
         dv1=-dr11*(a0*f+k10*f0)-r11*(f+a0*df+k10*df0)+2.*df
         dv2=-dr12*(a0*f+k10*f0)-r12*(f+a0*df+k10*df0)
         dv3=-dr21*( c*f+k1 *f0)-r21*(f+ c*df+k1 *df0)
         dv4=-dr22*( c*f+k1 *f0)-r22*(f+ c*df+k1 *df0)+2.*df
      end if

      if (xprnt .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_FSINTG:   v'',5(1pe12.3,e11.3))')
     &           v1,v2,v3,v4
         if (ideriv .eq. 1) then
            write(lwpcLOG_lun,
     &          '(''MF_FSINTG:  dv'',5(1pe12.3,e11.3))')
     &              dv1,dv2,dv3,dv4
          end if
      end if

      r11=(v1*ey2-v2*ey1)/den
      r12=(v2*ep1-v1*ep2)/den*n0sq
      r21=(v3*ey2-v4*ey1)/den/n0sq
      r22=(v4*ep1-v3*ep2)/den

      if (ideriv .eq. 1) then
         dr11=(v1*dey2+dv1*ey2-v2*dey1-dv2*ey1)/den     -r11*dden/den
         dr12=(v2*dep1+dv2*ep1-v1*dep2-dv1*ep2)/den*n0sq-r12*dden/den
         dr21=(v3*dey2+dv3*ey2-v4*dey1-dv4*ey1)/den/n0sq-r21*dden/den
         dr22=(v4*dep1+dv4*ep1-v3*dep2-dv3*ep2)/den     -r22*dden/den
      end if

      if (xprnt .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_FSINTG:  zz'',f6.2,'' r='',4(2x,2f12.5))')
     &           zz,r11,r21,r12,r22
         if (ideriv .ne. 0) then
            write(lwpcLOG_lun,
     &          '(''MF_FSINTG:      derivs='',4(2x,1p2e12.5))')
     &              dr11,dr21,dr12,dr22
         end if
      end if
      RETURN
      END      ! MF_FSINTG