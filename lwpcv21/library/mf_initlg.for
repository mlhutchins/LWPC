      SUBROUTINE MF_INITLG

c***********************************************************************

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.
c 3/5/10 MLH Added temporary lprnt storage to bypass memory error at 31
c 3/10/10 MLH Removed above change, used -fno-automatic during compile
c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxboxes=200,mxcorners=400)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      character*200  error_msg
      integer        pflag,pindex,rprnt,xprnt
      real           freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,
     &               adjmsh,wdchng,unitr,uniti,
     &               boxlft,boxrht,boxtop,boxbot,
     &               rtol,xintrp,htntrp,d,z0,zz,
     &               omega,wavenr,tlft,trht,ttop,tbot,
     &               htinc,diffsq,atol,delp,delm,size,
     &               amp(4),phs(4),delmax,difpls,difmns,dif0,etol,
     &               tlengt,ht0,htmrgn

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex

     &      /mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_crnr/tcornr(mxcorners),rcornr(4,mxcorners),nrcorner
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rntg/r(4),dr(4),drdc(4),drdcdh(4),roe(8),rnd(5)
     &      /mf_rseq/r0(4),drdt(4),d2rdt2(4),d3rdt3(4),
     &               zetap(2),gammap(2),theta0,extra
     &      /mf_side/tlft,trht,ttop,tbot

      dimension      thetag(4),rgiven(4,4),rlow(4,4),
     &               drlsq(4),rcomp(4)

      data           htinc/1./,htmrgn/10./

c     Coefficients for interpolating values of reflection coefficients
c     are found. If testing of truncation error indicates the search
c     rectangle is too large for interpolation, NOTLIN is set to 1.

c     Define the corners and mid-point of the search rectangle.
      thetag(1)=CMPLX(tlft,ttop)
      thetag(2)=CMPLX(trht,ttop)
      thetag(3)=CMPLX(tlft,tbot)
      thetag(4)=CMPLX(trht,tbot)

      theta0=CMPLX(.5*(tlft+trht),.5*(tbot+ttop))

c     At each corner of the rectangle, values of elements of the
c     reflection matrix found by using the full-wave solution are
c     retrieved from values already stored for adjacent rectangle.
c     If a full-wave integration cannot be done at TCORNR(lc), then
c     REAL(RCORNR(1,lc)) is set to a large value as a flag. RLOW is
c     the reflection matrix corresponding to the bottom of the
c     ionosphere.

      ideriv=0
      nointg=0

      atol=0.01*adjmsh
      do l=1,4
         last=0
         if (nrcorner .gt. 0) then
            lc=1
            do while (last .eq. 0 .and. lc .le. nrcorner)
               delt=thetag(l)-tcornr(lc)
               diffsq=REAL(delt)**2+AIMAG(delt)**2
               if (diffsq .le. atol**2) then
                  call MF_XFER (rcornr(1,lc),r,4)
                  if (REAL(r(1)) .lt. 22000.) then ! EXP(RLGMAX)
                     last=1
                  else
                     nointg=1
                     RETURN
                  end if
               end if
               lc=lc+1
            end do
         end if
         if (last .eq. 0) then
            theta=thetag(l)
            call MF_INTEG
            if (nointg .ne. 0) r(1)=60000. ! EXP(RLGMAX+1)
            if (nrcorner .lt. mxcorners) then
               nrcorner=nrcorner+1
               tcornr(nrcorner)=theta
               call MF_XFER (r,rcornr(1,nrcorner),4)
            end if
            if (nointg .ne. 0) RETURN
         end if
         call MF_XFER (r,rlow(1,l),4)
      end do

c     Geometry parameters, G and W, of the search rectangle are found.
c     TLENGT is used for scaling but does not affect the final values
c     of the coefficients.

      tlengt=ABS(thetag(1)-theta0)
      d1=(thetag(1)-theta0)/tlengt
      d2=(thetag(2)-theta0)/tlengt

      conjd1=conjg(d1)
      conjd2=conjg(d2)

      d1sq=d1**2
      d1cu=d1**3
      d2sq=d2**2
      d2cu=d2**3

      twdet1=2.*(d2sq-d1sq)
      twdet2=2.*d1*d2*(d2sq-d1sq)

      g11=d2sq/twdet1
      g12=-d1sq/twdet1
      g21=-1./twdet1
      g22=1./twdet1

      w11=d2cu/twdet2
      w12=-d1cu/twdet2
      w21=-d2/twdet2
      w22=d1/twdet2

c     Set up the search for the optimum reference height D.

      iprint=1
      irefht=0
      ihtpls=0
      iht0=0
      ihtmns=0
      ht0=(topht+htntrp)*.5
      nrtm=0



30    ihtflg=0
      if (ihtpls .eq. 0) ihtflg=1
      if (ihtmns .eq. 0 .and. ihtflg .eq. 0) ihtflg=-1
      ht=ht0+ihtflg*htinc

c     Set up heights for FSINTG
31    z0=botht
      zz=ht

      do l=1,4
         theta=thetag(l)
         call MF_XFER (rlow(1,l),r,4)
         call MF_FSINTG
         rgiven(1,l)=r(1)
         rgiven(2,l)=r(2)
         rgiven(3,l)=r(3)
         rgiven(4,l)=r(4)
      end do



c     The interpolation coefficients are found. A least-squares value of
c     the first-order coefficient is also found that corresponds to the
c     situation of omitting the third-order coefficients. It is used
c     only for testing truncation error.

      do i=1,4
         v1=rgiven(i,1)-rgiven(i,4)
         v2=rgiven(i,2)-rgiven(i,3)

         a1=w11*v1+w12*v2
         a3=w21*v1+w22*v2

         a1lsq=(conjd1*v1+conjd2*v2)/(conjd1*d1+conjd2*d2)*.5

         v1=rgiven(i,1)+rgiven(i,4)
         v2=rgiven(i,2)+rgiven(i,3)

         a0=g11*v1+g12*v2
         a2=g21*v1+g22*v2

         r0(i)=a0
         drdt(i)=a1/tlengt
         d2rdt2(i)=2.*a2/tlengt**2
         d3rdt3(i)=6.*a3/tlengt**3

         drlsq(i)=a1lsq/tlengt
      end do

      if (mrprnt .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_INITLG: theta0='',2f7.3,2i3)')
     &            theta0,iprint,ihtflg
         do i=1,4
            alnr=LOG(r(i))
            amp(i)=EXP(REAL(alnr))
            phs(i)=   AIMAG(alnr)*57.296
         end do
         write(lwpcLOG_lun,
     &       '(''MF_INITLG: Amplitude and phase of r'')')
         write(lwpcLOG_lun,
     &       '(10x,2f12.5,15x,2f12.5)')
     &         amp(1),amp(3),phs(1),phs(3)
         write(lwpcLOG_lun,
     &       '(10x,2f12.5,15x,2f12.5)')
     &         amp(2),amp(4),phs(2),phs(4)
      end if

      etol=30.*rtol
      delmax=0.

c     Eigen values of the reflection matrix corresponding to a third
c     order interpolation series are compared to eigen values of
c     elements using a second order least-squares seies. The largest
c     difference in either eigen value at the corners os the rectangle
c     is taken to be the truncation error for that height.

      do l=1,4
         r(1)=rgiven(1,l)
         r(2)=rgiven(2,l)
         r(3)=rgiven(3,l)
         r(4)=rgiven(4,l)

         arg=(r(1)-r(4))**2+4.*r(2)*r(3)
         rt=SQRT(arg)
         sm=r(1)+r(4)
         if (ABS(sm+rt) .lt. ABS(sm-rt)) rt=-rt
         eigpls=sm+rt
         eigmns=sm-rt

         rtgivn=rt
         delt=thetag(l)-theta0

         rcomp(1)=((d2rdt2(1)*.5)*delt+drlsq(1))*delt+r0(1)
         rcomp(2)=((d2rdt2(2)*.5)*delt+drlsq(2))*delt+r0(2)
         rcomp(3)=((d2rdt2(3)*.5)*delt+drlsq(3))*delt+r0(3)
         rcomp(4)=((d2rdt2(4)*.5)*delt+drlsq(4))*delt+r0(4)

         r(1)=rcomp(1)
         r(2)=rcomp(2)
         r(3)=rcomp(3)
         r(4)=rcomp(4)

         arg=(r(1)-r(4))**2+4.*r(2)*r(3)
         rt=SQRT(arg)
         if (ABS(rt+rtgivn) .lt. ABS(rt-rtgivn)) rt=-rt
         sm=r(1)+r(4)
         epcomp=sm+rt
         emcomp=sm-rt

         size=(ABS(eigpls)+ABS(epcomp))*.5
         delp=ABS(eigpls-epcomp)/size
         delm=ABS(eigmns-emcomp)/size

         if (delmax .lt. delp   ) delmax=delp
         if (delmax .lt. delm/3.) delmax=delm/3.

         if (mrprnt .ne. 0) then
            write(lwpcLOG_lun,
     &          '(''MF_INITLG: theta ='',2f7.3,2i3)')
     &              thetag(l),iprint,ihtflg
            write(lwpcLOG_lun,
     &          '(''MF_INITLG: rgiven='',8f12.4)')
     &             (rgiven(i,l),i=1,4)
            write(lwpcLOG_lun,
     &          '(''MF_INITLG: rcomp ='',8f12.4)')
     &             (rcomp(i),i=1,4)
            write(lwpcLOG_lun,
     &          '(''MF_INITLG: eig   ='',4f12.5)')
     &              eigpls,eigmns
            write(lwpcLOG_lun,
     &          '(''MF_INITLG: ecomp ='',4f12.5)')
     &              epcomp,emcomp
            write(lwpcLOG_lun,
     &          '(''MF_INITLG: diff  ='',2f12.5)')
     &              delp,delm
         end if
      end do

      if (irefht .eq. 1) then
         difpls=delmax
         go to 88
      end if

c     Testing of whether truncation error values have been found for
c     three successive heights, the middle one of which should
c     correspond to the minimum value of such error.

      if (ihtflg .eq. 1) then
         difpls=delmax
         ihtpls=1
      else
         if (ihtflg .eq. -1) then
            difmns=delmax
            ihtmns=1
         else
     &   if (ihtflg .eq. 0) then
            dif0=delmax
            iht0=1
         end if
      end if

      if (mrprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_INITLG: ht, delmax'',f7.2,3x,f8.5)')
     &           ht,delmax

      nrtm=nrtm+1
      if (nrtm .gt. 300) then
         write(error_msg,
     &       '(''[MF_INITLG]: '',
     &         ''Too many steps in finding reflection ht'')')
         call LWPC_ERROR ('ERROR',error_msg)
      end if

      if (ihtpls .eq. 0 .or. iht0 .eq. 0 .or. ihtmns .eq. 0)
     &   go to 30

c     Test of whether height of minimum in value of truncation error
c     has been found.
      if (dif0 .le. difpls .and. dif0 .le. difmns) go to 87
      iprint=0

      if (dif0 .le. difpls) go to 85
      ht0=ht0+htinc
      if (ht0 .ge. topht+htmrgn) go to 86
      ihtpls=0
      difmns=dif0
      dif0=difpls
      go to 30
85    ht0=ht0-htinc
      if (ht0 .le. botht) go to 86
      ihtmns=0
      difpls=dif0
      dif0=difmns
      go to 30

c     If no height is found at which truncation error is a minimum, then
c     D is set to HTNTRP.

86    irefht=1
      ht0=htntrp
      ht=ht0
c      write(lwpcLOG_lun,
c     &    '(''MF_INITLG: '',
c     &      ''WARNING:  Reflection ht set to HTNTRP='',f7.2,1x,
c     &      ''for interpolation'')')
c     &        ht0
c MLH commented out to give a consistant log file
      go to 31

87    if (iprint .eq. 0) then
         iprint=1
         go to 30
      end if

c     If truncation error is greater than 30 times RTOL, then NOTLIN is
c     set to 1.

88    notlin=0
      if (dif0 .gt. etol) notlin=1

      if (mrprnt .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_INITLG: difmax='',f8.5)')
     &           dif0
         write(lwpcLOG_lun,
     &       '(''MF_INITLG: etol  ='',f8.5,'' notlin='',i3)')
     &           etol,notlin
      end if
c   MLH commented out for Matlab processing of log file
c      if (lprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_INITLG: At theta='',2f7.2,1x,
c     &         ''the reflection ht='',f7.2)')
c     &           theta0,ht0

c     D is set to the height of minimum truncation error.
      d=ht0

      RETURN
      END      ! MF_INITLG
