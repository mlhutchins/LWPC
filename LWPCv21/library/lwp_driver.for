      SUBROUTINE LWP_DRIVER
     &          (begin_file)

c***********************************************************************

c     Routine to processed presegmented path parameters

c***********************************************************************

c  Change history:

c     10 Nov 93     Do the restart of MF using IEXACT=1.

c     30 Jun 94     Drop END_FILE from argument list.

c     14 Nov 94     Added subroutine MF_GET_MC to calculate mode
c                   constants when SW fails at the starting segment.

c     13 Dec 94     Added specification for PRFL_FILE.

c     26 Sep 95     Changed all PRINTs to WRITE(6)s.

c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     08 Feb 99     When we want to continue using the original
c                   solutions from MF_DRIVER, we must recalculate
c                   them because the list may have been modified
c                   by SW_STEP.

c*******************!***************************************************

c     Sign convention: + for West and North, - for East and South

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxeigen=50,mxsgmnt=201)

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

      real     *  4 hten,algen,htnu,algnu,
     &              charge,ratiom,select_hgts,hgts

      common/lwpc_pr/
     &              hten(51),algen(51,3),nrhten,ihten,
     &              htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &              charge(3),ratiom(3),nrspec,lu_prf,
     &              select_hgts(2),hgts(3)

      common/mf_flag/
     &              iexact,iovflo,kexact,lowg,nodivd,noevct,
     &              nofinl,nointg,nomesh,notlin

      common/mf_sw_1/
     &              dst(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt

      complex  *  8 save_tp,save_capt,save_fofr

      common/mf_sw_2/
     &              save_tp  (mxsgmnt,  mxeigen),
     &              save_capt(mxsgmnt,4,mxeigen),
     &              save_fofr(mxsgmnt,  mxeigen)

      integer       print_swg
      real     *  4 drmin,drmax

      common/sw_path/
     &              drmin,drmax,mdir,lostm,lx,print_swg

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),
     &              thtinc,ftol,maxitr,alpha,prec,rpoly,nrtlst

      complex  *  8 tp,capt,fofr

      common/sw_wgou/
     &              tp(mxeigen),capt(4,mxeigen),fofr(mxeigen),lu_mds

      character*120 prfl_file
      logical       begin_file
      integer       month,day,year,UT,prev_index

c intrp=0 --- normal (no interpolation)
c      =1 --- interpolating between points on the path
c      =2 --- final interpolation interval

c lostm=0 --- no trouble with modes
c      =1 --- all modes found but one or more changed significantly
c             from the extrapolated values
c      =2 --- dropped a mode in SW_WVGD or SW_EXTRAP; try to back up
c      =3 --- dropped a mode in SW_WVGD or SW_EXTRAP; skip segment
c      =4 --- dropped a mode in SW_WVGD or SW_EXTRAP; restart with MF
c      =5 --- distance change is too large;           restart with MF

c pflag=0 --- non-exponential profile on whole path
c      =1 ---     exponential profile on whole path
c      =2 ---     exponential profiles for all day
c      =3 ---     exponential profiles for all night
c      =4 ---     exponential profiles for a specific date and time
c      =5,7,9 non-exponential profiles at each point
c      =6,8,10    exponential profiles at each point


Change: 10 Nov 93: Save current value of flags
      iexact_sav=iexact
      irpoly_sav=rpoly
      nsgmnt0=1
      prev_index=0

      do while (nsgmnt0 .le. nrsgmnt)

         nsgmnt=nsgmnt0
         do while (nsgmnt .le. nrsgmnt)

            rho   =dst(nsgmnt)
            lat   =xla(nsgmnt)
            lon   =xlo(nsgmnt)
            azim  =azm(nsgmnt)
            dip   =xdp(nsgmnt)
            bfield=fld(nsgmnt)
            sigma =sgm(nsgmnt)
            epsr  =eps(nsgmnt)
            beta  =bta(nsgmnt)
            hprime=hpr(nsgmnt)
            pindex=npr(nsgmnt)

            if (pflag .eq. 5 .or. pflag .eq. 7 .or. pflag .eq. 9) then
c              Get position dependent tablular profile
               call PRFL_SPECIFICATION
     &             (.false.,print_swg,
     &              prfl_file,prfl_id,pflag,
     &              freq,month,day,year,UT,
     &              lat,lon,rho,dip,zenith,
     &              hpr_mid,beta,hprime,pindex)
            else
     &      if (pindex .ne. prev_index) then
c              Get new profile            
               call PRFL_EXP (beta,hprime)
               call PRFL_HGTS (print_swg)
               prev_index=pindex
            end if
            if (mdir .eq. 1) then

c              Reverse azim if for xmtr deployment
               azim=azim-180.
               if (azim .lt.   0.) then
                  azim=azim+360.
               else
     &         if (azim .gt. 360.) then
                  azim=azim-360.
               end if
            end if
            if (print_swg .gt. 0)            
     &         write(lwpcLOG_lun,
     &             '(/''Propagation path parameters:''/
     &                ''  rho    lat     lon'',
     &                ''   azim    dip  bfield'',
     &                ''   sigma  epsr  beta   h''''''/
     &                f6.0,f7.2,f8.2,2f7.2,f7.4,
     &                1pe9.1,0pf5.1,2f6.2)')
     &                rho,lat,lon,azim,dip,bfield,
     &                sigma,epsr,beta,hprime

            if (nsgmnt .eq. nsgmnt0) then
c              Initialize extrapolation
               lx=0
               rho0=rho

c              Get starting solutions
               call MF_DRIVER

               if (print_swg .gt. 0)

     &            write(lwpcLOG_lun,
     &                '(''Output from MF_DRIVER:''/(8f9.4))')
     &                 (eigen(j),j=1,nreigen)

               if (nreigen .lt. mxeigen) eigen(nreigen+1)=(0.,0.)

cxx               tol(1)=lub
cxx               tol(2)=lub
cxx               dtheta(1)=lub*2.
cxx               dtheta(2)=lub*2.
cxx               thtinc=lub*8.
cxx
cxxChange: 06 Feb 94
cxxc       Ensure that SW uses the same tolerance for duplicate EIGENs
cxxc       as used by MF.
cxx               deigen(1)=1.
cxx               deigen(2)=1.
cxx               do j=1,nreigen-1
cxx                  eigenjr= REAL(eigen(j))
cxx                  eigenri=AIMAG(eigen(j))
cxx                  do k=j+1,nreigen
cxx                     eigenkr= REAL(eigen(k))
cxx                     eigenki=AIMAG(eigen(k))
cxx                     deigen(1)=MIN(deigen(1),ABS(eigenjr-eigenkr))
cxx                     deigen(2)=MIN(deigen(2),ABS(eigenji-eigenki))
cxx                  end do
cxx               end do
cxxChange: 26 Apr 95
cxxc       Ensure a non-zero minimum separation
cxx               deigen(1)=MAX(deigen(1)*.5,.000001)
cxx               deigen(2)=MAX(deigen(2)*.5,.000001)
Change: 26 Apr 95
c       Use LUB (haven't we been here before?)
               thtinc=lub*4.
               tol(1)=lub
               tol(2)=lub/5.
               dtheta(1)=tol(1)*2.
               dtheta(2)=tol(2)*2.
               deigen(1)=tol(1)/4.
               deigen(2)=tol(2)/4.

               if (print_swg .gt. 0)

     &            write(lwpcLOG_lun,
     &                '(''Iteration parameters:''/
     &                  ''    lub    thtinc    tolr    toli'',
     &                  ''    dthtar  dthtai    deignr    deigni''/
     &                  3(1x,2f8.4),1x,2f10.6)')
     &                  lub,thtinc,tol,dtheta,deigen
            end if
            if (nreigen .eq. 0) then

c              NO MODES FOUND: SKIP SEGMENT
               if (print_swg .gt. 0)
     &            write(lwpcLOG_lun,
     &                '(/''[LWP_DRIVER] WARNING: '',
     &                   ''MF_DRIVER found no modes; SKIP SEGMENT'')')
               call LWPC_ERROR ('ERROR','MF_DRIVER found no modes')
               num(nsgmnt)=-1
               nsgmnt=nrsgmnt+1
            else

               call SW_STEP (rho0,rho,azim,dip,bfield)
               if (lostm .lt. 2) then

Change: 10 Nov 93: Successful; make sure that flags are reset
                  iexact=iexact_sav
                  rpoly =irpoly_sav

c                 Store data for this segment
                  num(nsgmnt)=nreigen
                  do neigen=1,nreigen
                     save_tp  (nsgmnt,  neigen)=tp  (  neigen)
                     save_fofr(nsgmnt,  neigen)=fofr(  neigen)
                     save_capt(nsgmnt,1,neigen)=capt(1,neigen)
                     save_capt(nsgmnt,2,neigen)=capt(2,neigen)
                     save_capt(nsgmnt,3,neigen)=capt(3,neigen)
                     save_capt(nsgmnt,4,neigen)=capt(4,neigen)
                  end do

c                 Get next segment
                  call SW_NEXT (pflag,nsgmnt0,nsgmnt)
               else

                  if (lostm .eq. 3 .and. iexact .eq. 1) then

Change: 14 Nov 94: Use mode parameters as computed by MF
                     if (print_swg .gt. 0)

     &                  write(lwpcLOG_lun,
     &                      '(/''Continuing with mode parameters '',
     &                         ''from MF_DRIVER'')')
c                    Refresh the list of starting solutions
                     call MF_DRIVER

c                    Get mode parameters
                     call MF_GET_MC

c                    Reset flags
                     iexact=iexact_sav
                     rpoly =irpoly_sav

c                    Store data for this segment
                     num(nsgmnt)=nreigen
                     do neigen=1,nreigen
                        save_tp  (nsgmnt,  neigen)=tp  (  neigen)
                        save_fofr(nsgmnt,  neigen)=fofr(  neigen)
                        save_capt(nsgmnt,1,neigen)=capt(1,neigen)
                        save_capt(nsgmnt,2,neigen)=capt(2,neigen)
                        save_capt(nsgmnt,3,neigen)=capt(3,neigen)
                        save_capt(nsgmnt,4,neigen)=capt(4,neigen)
                     end do

c                    Get next segment
                     call SW_NEXT (pflag,nsgmnt0,nsgmnt)
                  else

c                    RESTART WITH MF
                     nsgmnt0=nsgmnt

Change: 10 Nov 93: Do the restart using IEXACT=1 and RPOLY=0
                     iexact=1
                     rpoly =0
                  end if
               end if
            end if
         end do
         
         if (nsgmnt .gt. nrsgmnt) then
            nsgmnt0=1
            do while (nsgmnt0 .le. nrsgmnt .and. num(nsgmnt0) .ne. 0)
               nsgmnt0=nsgmnt0+1
            end do
         end if
      end do
      call LWP_SAVE_MC (begin_file)
      RETURN
      END      ! LWP_DRIVER
