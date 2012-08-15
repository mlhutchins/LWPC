      SUBROUTINE MF_TMTRX

c     Computation of M matrix elements as defined by Budden in Proc.
c     Roy. Soc. (London), A227, pp. 516-537 (1955).  Those combinations
c     of M matrix elements used in the T matrix which do not include
c     use of THETA are the final output of this routine.  The dummy
c     indexing on L is used in view of possible storage (as functions
c     of height) and re-use of these combinations for different values
c     of THETA.

      parameter     (mxeigen=50)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      integer        pflag,pindex
      real           lat,lon,lub
      complex        eigen

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex
     &      /lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen
     &      /lwpc_pr/hten(51),algen(51,3),nrhten,ihten,
     &               htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &               charge(3),ratiom(3),nrspec,lu_prf,
     &               select_hgts(2),hgts(3)

      real           nu
      complex        g,q,theta,c,s,csq,ssq,
     &               m11,m21,m31,m12,m22,m32,m13,m23,m33,t11ovs,t44ovs,
     &               t12ovs,t34ovs,t14a,t31,t42,t32mc2,t41m1,
     &               capd,u,usq,usqd,yud,ysqd,ta,tb,
     &               m13d,m31d,m23d,m32d,m2332d,m1331d,m2331d,m3213d

      common/mf_drcs/dcl,dcm,dcn,ec,g,q
     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_mmtx/m11,m21,m31,m12,m22,m32,m13,m23,m33,ht
     &      /mf_tmtx/t11ovs,t44ovs,t12ovs,t34ovs,t14a,t31,t42,t32mc2,
     &               t41m1

      dimension      en(3),nu(3)

      data           cx/3.182357e09/,cy/1.758796e7/,re/6369.427/


      dclsq=dcl**2
      dcmsq=dcm**2
      dcnsq=dcn**2

      call PRFL_EN NU (ht,en,nu)

      yud= (0.,0.)
      ysqd=(0.,0.)
      usqd=(0.,0.)
      do n=1,nrspec

         coeffx=cx*(charge(n)/omega)**2/ratiom(n)
         y=cy*bfield/omega*charge(n)/ratiom(n)
         ysq=y**2

         x=coeffx*en(n)
         z=nu(n)/omega
         u=CMPLX(1.,-z)
         usq=u*u
         capd=-x/(u*(usq-ysq))
         usqd=usqd+usq*capd
         yud =yud +y*u*capd
         ysqd=ysqd+ysq*capd
      end do

      crvtrm=2./re*(ht-h)

      m11=usqd-dclsq*ysqd+crvtrm
      m22=usqd-dcmsq*ysqd+crvtrm
      m33=usqd-dcnsq*ysqd+crvtrm
      ta=CMPLX(0.,dcn)*yud
      tb=dcl*dcm*ysqd
      m21=+ta-tb
      m12=-ta-tb
      ta=CMPLX(0.,dcm)*yud
      tb=dcl*dcn*ysqd
      m13=+ta-tb
      m31=-ta-tb
      ta=CMPLX(0.,dcl)*yud
      tb=dcm*dcn*ysqd
      m32=+ta-tb
      m23=-ta-tb

      capd=(1.,0.)/((1.,0.)+m33)
      m13d=m13*capd
      m31d=m31*capd
      m23d=m23*capd
      m32d=m32*capd
      m2332d=m32*m23d
      m1331d=m31*m13d
      m2331d=m31*m23d
      m3213d=m32*m13d

      t11ovs=-m31d
      t44ovs=-m13d
      t12ovs=m32d
      t34ovs=m23d
      t14a=m33*capd
      t31=m2331d-m21
      t42=m3213d-m12
      t32mc2=m22-m2332d
      t41m1=m11-m1331d
      RETURN
      END      ! MF_T MTRX