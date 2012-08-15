      SUBROUTINE SW_SMATRX

      implicit real*8 (a-h,o-z)

c***********************************************************************

c  Change History:
c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

      parameter    (mxeigen=50)

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

      real     *  4 ranger,rangei,atnmax,lub,h
      complex  *  8 eigen

      common/lwpc_mf/
     &              ranger(2),rangei(2),atnmax,lub,h,
     &              eigen(mxeigen),nreigen

      real     *  4 hten,algen,htnu,algnu,charge,ratiom,
     &              select_hgts,hgts

      common/lwpc_pr/
     &              hten(51),algen(51,3),nrhten,ihten,
     &              htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &              charge(3),ratiom(3),nrspec,lu_prf,
     &              select_hgts(2),hgts(3)

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

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),
     &              thtinc,ftol,maxitr,alpha,prec,rpoly,nrtlst

      real     *  4 hh,en(3),nu(3)
      real     *  8 lsq,msq,nsq,lm,ln,mn
      complex  * 16 il,im,in,capd,usqd,yud,ysqd,u,usq

      data          dtr/1.745329252d-2/,
     &              coeffy/1.758796d7/,coeffx/3.182357d09/


      drcosl= COS(dip*dtr)*COS(azim*dtr)
      drcosm= COS(dip*dtr)*SIN(azim*dtr)
      drcosn=-SIN(dip*dtr)
      il=DCMPLX(0.d0,drcosl)
      im=DCMPLX(0.d0,drcosm)
      in=DCMPLX(0.d0,drcosn)
      lsq=drcosl**2
      msq=drcosm**2
      nsq=drcosn**2
      lm=drcosl*drcosm
      ln=drcosl*drcosn
      mn=drcosm*drcosn

      cx=coeffx/omega**2
      cy=coeffy*bfield/omega

      hh=ht
      call PRFL_EN NU (hh,en,nu)

      usqd=0.d0
      yud =0.d0
      ysqd=0.d0
      do n=1,nrspec
         capx=cx*charge(n)**2/ratiom(n)*en(n)
         capy=cy*charge(n)/ratiom(n)
         capz=nu(n)/omega
         u=DCMPLX(1.d0,-capz)
         usq=u*u
         capd=-capx/(u*(usq-capy**2))
         if (ABS(capd) .gt. 1.d-30) then
            usqd=usqd+usq*capd
            yud =yud +capy*u*capd
            ysqd=ysqd+capy**2*capd
         end if
      end do
      crvtrm=alpha*(h-ht)
      m11=usqd-lsq*ysqd-crvtrm
      m22=usqd-msq*ysqd-crvtrm
      m33=usqd-nsq*ysqd-crvtrm
      m12=-in*yud-lm*ysqd
      m21= in*yud-lm*ysqd
      m13= im*yud-ln*ysqd
      m31=-im*yud-ln*ysqd
      m23=-il*yud-mn*ysqd
      m32= il*yud-mn*ysqd
      RETURN
      END      ! SW_SMATRX