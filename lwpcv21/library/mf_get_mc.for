      SUBROUTINE MF_GET_MC

c     Routine for computing mode parameters.

      parameter    (mxeigen=50,mxboxes=200)

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

      common/mf_boxs/
     &              adjmsh,wdchng,
     &              unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &              uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &              nrboxs,kb

      common/mf_flag/
     &              iexact,iovflo,kexact,lowg,nodivd,noevct,
     &              nofinal,nointg,nomesh,notlin

      common/mf_hgts/
     &              rtol,xintrp,htntrp,d,z0,zz

      common/mf_list/
     &              listno(mxboxes),listbx(mxeigen),listke(mxeigen)

      complex       theta,c,s,csq,ssq

      common/mf_mode/
     &              theta,c,s,csq,ssq,omega,wavenr,ideriv

      complex       num11,num22,den11,den22,
     &              dnum11,dnum22,dden11,dden22,
     &              f1,f2,hg

      common/mf_rbrs/
     &              num11,num22,den11,den22,
     &              dnum11,dnum22,dden11,dden22,
     &              f1,f2,hg

      complex       x11,x21,x12,x22,dx11dc,dx21dc,dx12dc,dx22dc,
     &              dxdh,roe,rnd

      common/mf_rntg/
     &              x11,x21,x12,x22,dx11dc,dx21dc,dx12dc,dx22dc,
     &              dxdh(8),roe(8),rnd(5)

      complex       tp,capt,fofr

      common/sw_wgou/
     &              tp(mxeigen),capt(4,mxeigen),fofr(mxeigen),lu_mds

      complex       stp,sqrts,
     &              r11,r21,r12,r22,rbar11,rbar22,
     &              d11,d22,rb11p1,rb22p1,
     &              fm,dfmdt,dfdt,factor

      data          re/6369.427/


      d=botht
      ideriv=1
      do neigen=1,nreigen
         kb=listbx(neigen)
         noevct=listno(kb)
         kexact=listke(neigen)

c        Set up heights for FSINTG
         if (kexact .eq. 0) then
            z0=htntrp
            zz=d
         else
            z0=botht
            zz=d
         end if
         theta=eigen(neigen)
         c=COS(theta*0.01745329252)
         s=SIN(theta*0.01745329252)
         csq=c**2
         ssq=s**2

         call MF_FDFDT (theta,fm,dfmdt)

         r11=c*x11-1.
         r21=c*x21
         r12=c*x12
         r22=c*x22-1.
         d11=c*num11-den11
         d22=c*num22-den22
         rbar11=den11/d11
         rbar22=den22/d22
         rb11p1=c*num11/d11
         rb22p1=c*num22/d22

         dfdt=dfmdt*csq/(d11*d22)*57.296
         sqrts=SQRT(s)
         factor=SQRT(s)/dfdt

c        Eigen angle referred to ground level.
         capk=SQRT(1.-2.*h/re)
         stp=s/capk
         tp(neigen)=(0.,-57.296)
     &             *LOG(SQRT((1.,0.)-stp*stp)+(0.,1.)*stp)

         num11=(1.,0.)-rbar11*r11
         num22=(1.,0.)-rbar22*r22
         if (ABS(num11) .lt. ABS(num22)) then
            iterm=1
            num11=r12*r21*rbar11*rbar22/num22
         else
            iterm=2
            num22=r12*r21*rbar11*rbar22/num11
         end if

c        VLF/LF excitation terms
         capt(1,neigen)=rb11p1**2*num22*factor/(rbar11*f1*f1)
         capt(2,neigen)=rb22p1**2*num11*factor/(rbar22*f2*f2)
         capt(3,neigen)=rb11p1*rb22p1*factor*r21/(f1*f2)
         capt(4,neigen)=r12/r21
         if (iterm .eq. 1) then
            fofr(neigen)=capt(3,neigen)
     &                  /capt(1,neigen)
         else
            fofr(neigen)=capt(2,neigen)
     &                 /(capt(3,neigen)*capt(4,neigen))
         end if
      end do

      RETURN
      END      ! MF_GET_MC