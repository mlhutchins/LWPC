      SUBROUTINE WF_T_MTRX
     &          (isotropic)

c     Computes m - the susceptibility tensor and
c              t - the coefficient matrix of the linear system of
c                         dp/dz=-ik*t*p.

      implicit real*8 (a-h,o-z)

c     LWPM
      real     *  4 hten,algen,htnu,algnu,
     &              charge,mratio,select_hgts,hgts

      common/lwpc_pr/
     &              hten(51),algen(51,3),nrhten,ihten,
     &              htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &              charge(3),mratio(3),nrspec,lu_prf,
     &              select_hgts(2),hgts(3)

c     Wave Fields
      real     *  8 freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht,
     &              dtheta,lub,thtinc,alpha,h,prec,
     &              p sav,a norm,b norm,ht,wfht1,wfht2
      complex  * 16 eigen2,
     &              m11,m21,m31,m12,m22,m32,m13,m23,m33,
     &              c1,s1,c2,s2,p1,p2,dp1dh,dp2dh,
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44

      common/wf_inpt/
     &              eigen2,
     &              freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht
     &      /wf_iter/
     &              dtheta(2),lub(2),thtinc,alpha,h,prec,
     &              iter_flag,mxiter,nriter
     &      /wf_mmtx/
     &              m11,m21,m31,m12,m22,m32,m13,m23,m33
     &      /wf_pmtx/
     &              c1,c2,s1,s2,p1(8),p2(8),dp1dh(8),dp2dh(8)
     &      /wf_tmtx/
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44
     &      /wf_save/
     &              p sav(16),
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              a norm,b norm,ht,wfht1,wfht2,
     &              levl

c     Local
      real     *  4 hh,en(3),nu(3)
      complex  * 16 c1sq,s1sq,c2sq,s2sq,
     &              capd,u,usq,dd,iud,ta,tb

      data          dtr/0.01745329252d0/,
     &              coeffx/3.182357d09/,coeffy/1.758796d11/


c     Compute various quantities which do not vary with height.
      omega=6.2831853071796d3*freq2
      drcosl= SIN(codip2*dtr)*COS(azim2*dtr)
      drcosm= SIN(codip2*dtr)*SIN(azim2*dtr)
      drcosn=-COS(codip2*dtr)

      c1=COS(eigen2*dtr)
      s1=SIN(eigen2*dtr)
      c1sq=c1**2
      s1sq=s1**2
      c2=COS((eigen2-DCMPLX(dtheta(1),dtheta(2)))*dtr)
      s2=SIN((eigen2-DCMPLX(dtheta(1),dtheta(2)))*dtr)
      c2sq=c2**2
      s2sq=s2**2

      t11=0.d0
      t31=0.d0
      t12=0.d0
      t42=0.d0
      t34=0.d0
      t44=0.d0

c     Calculate the matrix m.
      m11=0.d0
      m12=0.d0
      m13=0.d0
      m21=0.d0
      m22=0.d0
      m23=0.d0
      m31=0.d0
      m32=0.d0
      m33=0.d0

      hh=ht
      call PRFL_EN NU (hh, en, nu)

c     Add in the contributions to the susceptibility tensor m for each
c     specie in the ionosphere.
      nflag=0
      do k=1,nrspec
         if (en(k) .ge. 1.d-3) then
            nflag=1
            x=coeffx*charge(k)**2/(omega**2*mratio(k))*en(k)
            y=coeffy*charge(k)*magfld2/(omega*mratio(k))
            z=nu(k)/omega
            u=DCMPLX(1.d0,-z)
            usq=u*u
            dd=-x/(u*(usq-y**2))
            iud=(z+(0.d0,1.d0))*dd
            ta=usq*dd
            m11=m11+ta
            m22=m22+ta-(drcosm*y)**2*dd
            m33=m33+ta
            ta=drcosm*y*iud
            tb=drcosl*y*drcosn*y*dd
            m13=m13+ta-tb
            m31=m31-ta-tb
            if (isotropic .eq. 0) then
               m11=m11-(drcosl*y)**2*dd
               m33=m33-(drcosn*y)**2*dd
               ta=drcosn*y*iud
               tb=drcosl*y*drcosm*y*dd
               m12=m12-ta-tb
               m21=m21+ta-tb
               ta=drcosl*y*iud
               tb=drcosm*y*drcosn*y*dd
               m23=m23-ta-tb
               m32=m32+ta-tb
            end if
         end if
      end do

      crvtrm=alpha*(h-ht)
      m11=m11-crvtrm
      m22=m22-crvtrm
      m33=m33-crvtrm

c     Calculate the matrix t.
      capd=1.d0/(1.d0+m33)
      tm41=1.d0+m11
      tm32=m22
      tm14=capd
      if (nflag .eq. 1) then
         tm41=-capd*m13*m31+tm41
         tm11= capd*m31
         tm44= capd*m13
         if (isotropic .eq. 0) then
            tm32=-capd*m32*m23+tm32
            tm31= capd*m23*m31-m21
            tm12= capd*m32
            tm42= capd*m13*m32-m12
            tm34= capd*m23
         end if
      end if

      t41=tm41
      t32=tm32+c1sq
      t14=1.d0-tm14*s1sq
      if (nflag .eq. 1) then
         t11=-tm11*s1
         t44=-tm44*s1
         if (isotropic .eq. 0) then
            t31=tm31
            t12=tm12*s1
            t42=tm42
            t34=tm34*s1
         end if
      else
         t11=0.d0
         t31=0.d0
         t12=0.d0
         t42=0.d0
         t34=0.d0
         t44=0.d0
      end if
      RETURN
      END      ! WF_T_MTRX