      SUBROUTINE LW_SUM_MODES
     &          (print_lwf,print_mc,print_wf,
     &           power,incl,headng,talt,ralt,
     &           ijkl,nrcmp,dst,amp,phs)

c***********************************************************************

c     Calculates the mode sum at DST

c***********************************************************************

c     INPUT PARAMETERS:

c     print_lwf     Indicates levels of print from ModeSum
c                   =0: minimum print
c                   =1: adds fields vs. distance
c                   =2: adds mode parameters vs. distance

c     print_mc      Indicates levels of print from ModeConversion
c                   =0: minimum print
c                   =1: adds conversion coefficients
c                   =2: adds integrals

c     print_wf      Indicates levels of print from WaveFields
c                   =0: minimum print
c                   =1: adds iterations
c                   =2: adds fields vs. height

c     power         power in kW
c     incl          antenna  inclination in degrees
c     headng        antenna  heading in degrees
c     talt          antenna  altitude in km
c     ralt          receiver altitude in km
c     ijkl          index of the call to this routine
c     nrcmp         index of the component;
c     dst           distance in km;

c     OUTPUT PARAMETERS:

c     amp           amplitude in dB above 1uv/m;
c     phs           relative phase in degrees;

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     12 Nov 96     Changed to allow paths longer than 20000.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxeigen=50,mxsgmnt=201)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,range,rlat,rlon,rrho,bearng,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht
      complex  *  8 thetap,capt,fofr,
     &              a,tp,stp,xtra,eyhy

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,range,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex
     &      /mf_sw_1/
     &              rng(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt
     &      /mf_sw_2/
     &              thetap(mxsgmnt,  mxeigen),
     &              capt  (mxsgmnt,4,mxeigen),
     &              fofr  (mxsgmnt,  mxeigen)
     &      /mc_slab/
     &              a(mxeigen,mxeigen),tp(mxeigen),stp(mxeigen),
     &              xtra(3,mxeigen),eyhy(mxeigen),nreigen2,nsgmnt

      integer       print_lwf,print_mc,print_wf
      real     *  4 power,incl,headng,talt,ralt,
     &              dst,amp(3),phs(3),phs1(3),cycle(3),wm(3),wa(3),
     &              dst0/99999./
      complex  *  8 mik,mikx,
     &              ta,soln a(mxeigen),temp(mxeigen),
     &              tb,soln b(3,mxeigen),
     &              hgt(3,mxeigen),hgr(3,mxeigen)

      dimension     t(3)

      data          alpha/3.14e-4/

      if (ijkl .eq. 1) then

         omega=6.283185308e3*freq
         waveno=20.958445e-3*freq
         mik=CMPLX(0.,-waveno)
         aconst=-8686.*waveno
         econst=20.*LOG10(35.*waveno)
         sum0=682.2408*SQRT(freq*power)

c        Antenna orientation factors
         theta=headng-bearng
         t(1)=COS(incl*.01745329252)
         t(2)=SIN(incl*.01745329252)*SIN(theta*.01745329252)
         t(3)=SIN(incl*.01745329252)*COS(theta*.01745329252)

c        Radiation resistance correction
         if (talt .eq. 0.) then
            const=sum0
         else
            x=2.*waveno*talt
            xt1=3.*(SIN(x)-x*COS(x))/x**3
            xt2=.5*(xt1-3.*SIN(x)/x)
            xt3=SQRT(2./(1.+xt2+(xt1-xt2)*t(1)**2))
            const=sum0*xt3
         end if

         if (ijkl .eq. 1 .and. print_lwf .gt. 1)
     &      write(lwpcLOG_lun,
     &          '(/a
     &            /''freq='',f5.1,'' tlat='',f5.1,'' tlon='',f6.1,1x,
     &             ''bearng='',f5.1,''  power='',f5.0/
     &             ''incl='',f3.0,'' headng='',f4.0,1x,
     &             ''talt='',f4.1,'' ralt='',f4.1)')
     &               case_id,freq,tlat,tlon,bearng,power,
     &               incl,headng,talt,ralt
      end if
      
      if (dst .lt. dst0) then

         phs1(1)=0.
         cycle(1)=0.
         phs1(2)=0.
         cycle(2)=0.
         phs1(3)=0.
         cycle(3)=0.

c        Get data for first slab
         nsgmnt=1
         lat     =xla(1)
         lon     =xlo(1)
         rho     =rng(1)
         azim    =azm(1)
         dip     =xdp(1)
         bfield  =fld(1)
         sigma   =sgm(1)
         epsr    =eps(1)
         beta    =bta(1)
         hprime  =hpr(1)
         pindex  =npr(1)
         nreigen2=num(1)

c        xone is distance to beginning of slab from the transmitter
c        xtwo is distance of the end   of slab

         xone=0.
         if (nrsgmnt .eq. 1) then

c           Horizontally homogeneous path
            xtwo=40000.
         else

c           Multi-segment path
            xtwo=rng(2)
         end if

         if (nreigen2 .le. 0) then

c           No modes; terminate the mode summing
            amp(1)=-99.
            phs(1)=0.
            amp(2)=-99.
            phs(2)=0.
            amp(3)=-99.
            phs(3)=0.
            dst0=0.

            RETURN
         end if

         do neigen=1,nreigen2

            tp(neigen)=thetap(1,neigen)

            stp(neigen)=SIN(tp(neigen)*(.01745329252,0.))

c           Transfer Ey/Hy
            eyhy(neigen)=fofr(1,neigen)

c           Get Hy excitation factor at the transmitter
            xtra(1,neigen)=-capt(1,1,neigen)*stp(neigen)
            xtra(2,neigen)= capt(1,3,neigen)*capt(1,4,neigen)
            xtra(3,neigen)= capt(1,1,neigen)
         end do

c        Get conversion coefficients
         call LW_STEP (print_mc,print_wf)

         call LW_HTGAIN (1,freq,sigma,epsr,alpha,
     &                     mxeigen,nreigen2,tp,talt,hgt)
         call LW_HTGAIN (1,freq,sigma,epsr,alpha,
     &                     mxeigen,nreigen2,tp,ralt,hgr)

         do m2=1,nreigen2

c           Hy excitation factor
            ta=xtra(1,m2)*hgt(1,m2)*t(1)+
     &         xtra(2,m2)*hgt(2,m2)*t(2)+
     &         xtra(3,m2)*hgt(3,m2)*t(3)
            soln a(m2)=ta

c           Ez excitation factor
            tb=-stp(m2)*ta
            soln b(1,m2)=tb*hgr(1,m2)

c           Ey excitation factor
            tb=eyhy(m2)*ta
            soln b(2,m2)=tb*hgr(2,m2)

c           Ex excitation factor
            tb=-ta
            soln b(3,m2)=tb*hgr(3,m2)
         end do

         if (print_lwf .gt. 1) then

c           Print mode constants table
            write(lwpcLOG_lun,'('' '')')
            if (beta .eq. 99.) then
               write(lwpcLOG_lun,
     &             '(''  rho   lat    lon  azim   dip mgfld'',
     &               ''  sigma eps  ndx  h''''  mds'')')
               write(lwpcLOG_lun,
     &             '(i5,f6.1,f7.1,f6.1,f6.1,f6.3,1pe7.0,0pf4.0,
     &               i5.3,f5.1,i4)')
     &               int(rho),lat,lon,azim,dip,bfield,sigma,epsr,
     &               pindex,hprime,nreigen2
            else
               write(lwpcLOG_lun,
     &             '(''  rho   lat    lon  azim   dip mgfld'',
     &               ''  sigma eps beta  h''''  mds'')')
               write(lwpcLOG_lun,
     &             '(i5,f6.1,f7.1,f6.1,f6.1,f6.3,1pe7.0,0pf4.0,
     &               f5.2,f5.1,i4)')
     &               int(rho),lat,lon,azim,dip,bfield,sigma,epsr,
     &               beta,hprime,nreigen2
            end if
            write(lwpcLOG_lun,
     &          '('' mode  atten   v/c         rel exc 1'')')

            do m2=1,nreigen2
               sr= REAL(stp(m2))
               si=AIMAG(stp(m2))
               atten=aconst*si
               voverc=1./sr
               do nc=1,3
                  wr= REAL(soln b(nc,m2))
                  wi=AIMAG(soln b(nc,m2))
                  wm(nc)=econst+10.*LOG10(MAX(wr**2+wi**2,1.e-20))
                  wa(nc)=ATAN2(wi,wr)-1.5707963267949
               end do
               write(lwpcLOG_lun,
     &             '(i4,f8.3,f9.5,3(f10.3,f7.3))')
     &               m2,atten,voverc,(wm(nc),wa(nc),nc=1,3)
            end do
         end if

         if (dst .eq. 0.) then
            amp(1)=10.*LOG10(const*80.)
            phs(1)=0.
            amp(2)=10.*LOG10(const*80.)
            phs(2)=0.
            amp(3)=10.*LOG10(const*80.)
            phs(3)=0.
            dst0=0.

            RETURN
         end if
      end if
      
720   if (nreigen2 .le. 0 .and. nsgmnt .ne. nrsgmnt) then

c        No modes; terminate the mode summing
         amp(1)=-99.
         phs(1)=0.
         amp(2)=-99.
         phs(2)=0.
         amp(3)=-99.
         phs(3)=0.

         RETURN
      end if

      if (dst .gt. xtwo) then

c        End of current slab
         mikx=mik*(xtwo-xone)
         nreigen1=nreigen2
         do m1=1,nreigen1

c           Get Hy excitation factors at end of slab
            soln a(m1)=soln a(m1)*EXP(mikx*(stp(m1)-(1.,0.)))
            temp  (m1)=soln a(m1)
         end do
         xone=xtwo

c        Get data for next slab
         nsgmnt=nsgmnt+1
         lat     =xla(nsgmnt)
         lon     =xlo(nsgmnt)
         rho     =rng(nsgmnt)
         azim    =azm(nsgmnt)
         dip     =xdp(nsgmnt)
         bfield  =fld(nsgmnt)
         sigma   =sgm(nsgmnt)
         epsr    =eps(nsgmnt)
         beta    =bta(nsgmnt)
         hprime  =hpr(nsgmnt)
         pindex  =npr(nsgmnt)
         nreigen2=num(nsgmnt)

         if (nsgmnt .lt. nrsgmnt) then
            xtwo=rng(nsgmnt+1)
         else
            xtwo=40000.
         end if
         
         if (nreigen2 .le. 0 .and. nsgmnt .ne. nrsgmnt) then

c           No modes; terminate the mode summing
            amp(1)=-99.
            phs(1)=0.
            amp(2)=-99.
            phs(2)=0.
            amp(3)=-99.
            phs(3)=0.

            RETURN
         end if
         
         do neigen=1,nreigen2
            tp(neigen)=thetap(nsgmnt,neigen)
            stp(neigen)=SIN(tp(neigen)*(.01745329252,0.))

c           Transfer Ey/Hy
            eyhy(neigen)=fofr(nsgmnt,neigen)
         end do
                  
c        Get conversion coefficients
         call LW_STEP (print_mc,print_wf)
         
         call LW_HTGAIN (1,freq,sigma,epsr,alpha,
     &                   mxeigen,nreigen2,tp,ralt,hgr)
         
         do m2=1,nreigen2

c           Hy excitation factor
            soln a(m2)=(0.d0,0.d0)
            do m1=1,nreigen1
               soln a(m2)=soln a(m2)+temp(m1)*a(m1,m2)
            end do
            ta=soln a(m2)

c           Ez excitation factor
            tb=-stp(m2)*ta
            soln b(1,m2)=tb*hgr(1,m2)

c           Ey excitation factor
            tb=eyhy(m2)*ta
            soln b(2,m2)=tb*hgr(2,m2)

c           Ex excitation factor
            tb=-ta
            soln b(3,m2)=tb*hgr(3,m2)
         end do
         
         if (print_lwf .gt. 1) then

c           Print mode constants table
            write(lwpcLOG_lun,'('' '')')
            if (beta .eq. 99.) then
               write(lwpcLOG_lun,
     &             '(''  rho   lat    lon  azim   dip mgfld'',
     &               ''  sigma eps  ndx  h''''  mds'')')
               write(lwpcLOG_lun,
     &             '(i5,f6.1,f7.1,f6.1,f6.1,f6.3,1pe7.0,0pf4.0,
     &               i5.3,f5.1,i4)')
     &               int(rho),lat,lon,azim,dip,bfield,sigma,epsr,
     &               pindex,hprime,nreigen2
            else
               write(lwpcLOG_lun,
     &             '(''  rho   lat    lon  azim   dip mgfld'',
     &               ''  sigma eps beta  h''''  mds'')')
               write(lwpcLOG_lun,
     &             '(i5,f6.1,f7.1,f6.1,f6.1,f6.3,1pe7.0,0pf4.0,
     &               f5.2,f5.1,i4)')
     &               int(rho),lat,lon,azim,dip,bfield,sigma,epsr,
     &               beta,hprime,nreigen2
            end if
            write(lwpcLOG_lun,
     &          '('' mode  atten   v/c         rel exc 1'')')
            do m2=1,nreigen2
               sr= REAL(stp(m2))
               si=AIMAG(stp(m2))
               atten=aconst*si
               voverc=1./sr
               do nc=1,3
                  wr= REAL(soln b(nc,m2))
                  wi=AIMAG(soln b(nc,m2))
                  wm(nc)=econst+10.*LOG10(MAX(wr**2+wi**2,1.e-20))
                  wa(nc)=ATAN2(wi,wr)-1.5707963267949
               end do
               write(lwpcLOG_lun,
     &             '(i4,f8.3,f9.5,3(f10.3,f7.3))')
     &               m2,atten,voverc,(wm(nc),wa(nc),nc=1,3)
            end do
         end if
         go to 720
      else

c        Get sum of modes
         mikx=mik*(dst-xone)
         factor=const/SQRT(ABS(SIN(dst/6366.)))

         do nc=1,nrcmp
            tb=(0.,0.)
            do m2=1,nreigen2
               tb=tb+soln b(nc,m2)*EXP(mikx*(stp(m2)-(1.,0.)))*factor
            end do
            tbr= REAL(tb)
            tbi=AIMAG(tb)
            phs2=ATAN2(tbi,tbr)*57.2957795
            if (ABS(phs1(nc)-phs2) .ge. 180.) then
               if (phs1(nc) .le. phs2) then
                  cycle(nc)=cycle(nc)-360.
               else
                  cycle(nc)=cycle(nc)+360.
               end if
            end if
            phs1(nc)=phs2
            amp(nc)=10.*LOG10(MAX(1.e-30,tbr**2+tbi**2))
            phs(nc)=phs2+cycle(nc)
         end do
         dst0=dst

         RETURN
      end if

      END      ! LW_SUM_MODES