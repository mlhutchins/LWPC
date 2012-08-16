      SUBROUTINE LW_STEP
     &          (print_mc,print_wf)

c***********************************************************************

c     Gets mode conversion coefficients.

c***********************************************************************

c     INPUT PARAMETERS:

c     print_mc      Indicates levels of print from ModeConversion
c                   =0: minimum print
c                   =1: adds conversion coefficients
c                   =2: adds integrals

c     print_wf      Indicates levels of print from WaveFields
c                   =0: minimum print
c                   =1: adds iterations
c                   =2: adds fields vs. height

c     mc_step       mode conversion flag
c                   =0: use rigorous formulation only
c                   =1: use rigorous formulation only if the
c                       approximate formulation fails
c                   =2: use approximate formulation only
c                   =3: get conversion coefficients from a file

c     mc_test       test to determine if LW_FULL_MC should be called
c                   Currently, if h' .gt. MC_TEST, then do full wave.

c     stp           sine(theta')
c     nreigen       number of STP
c     nsgmnt        path segment number

c     OUTPUT PARAMETERS:

c     a             conversion coefficients

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxeigen=50,mxsgmnt=201)

c     LWPC
      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,

     &              rng,xla,xlo,azm,xdp,fld,sgm,eps,bta,hpr

      complex  *  8 thetap,capt,fofr

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
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

c     Mode Conversion
      character*120 prfl_file
      integer       lu_wfd,lu_wfa,lu_wfp,mc_step,arb_azim
      real     *  8 max_wf_ht,mc_test
      complex  *  8 a,tp,stp,xtra,eyhy

      common/mc_inpt/
     &              prfl_file,max_wf_ht,mc_test,
     &              lu_wfd,lu_wfa,lu_wfp,mc_step,arb_azim
     &      /mc_slab/
     &              a(mxeigen,mxeigen),tp(mxeigen),stp(mxeigen),
     &              xtra(3,mxeigen),eyhy(mxeigen),nreigen,nsgmnt

c     Local
      character*120 save_file
      integer       print_mc,print_wf,pnmode,pnsgmnt

c     Set 9th byte to 0 (LW_FAST_MC only)
      mc_flag0(n)=(n-(n/65536)*65536)

c     Set 9th byte to 1 (LW_FAST_MC used when MC_STEP=1)
      mc_flag1(n)=(n-(n/65536)*65536)+65536

c     Set 9th byte to 2 (LW_FULL_MC only or when MC_STEP=1)
      mc_flag2(n)=(n-(n/65536)*65536)+131072

      if (mc_step .lt. 2) then

         if (prfl_file .ne. save_file) then

c           Initialize profile handling
            call PRFL_SPECIFICATION
     &          (.true.,0,
     &           prfl_file,prfl_id,pflag,
     &           freq,month,day,year,UT,
     &           lat,lon,rho,dip,zenith,
     &           hpr_mid,beta,hprime,pindex)

            save_file=prfl_file
         end if

c        Get ionospheric profile
         if (pflag .eq. 5 .or. pflag .eq. 7) then

c           Get range dependent tabular profile
            call PRFL_SPECIFICATION
     &          (.false.,print_wf,
     &           prfl_file,prfl_id,pflag,
     &           freq,month,day,year,UT,
     &           lat,lon,rho,dip,zenith,
     &           hpr_mid,beta,hprime,pindex)
         else

c           Get exponential profile
            call PRFL_EXP (beta,hprime)
            call PRFL_HGTS (print_wf)
         end if
      end if
      
      if (mc_step .eq. 0) then

c        Full mode conversion
         if (nsgmnt .eq. 1) then
            jflag=0
         else
            jflag=1
         end if
         call LW_FULL_MC (print_mc,print_wf,jflag)

c        Set the mode conversion flag to be 2
         npr(nsgmnt)=MC_FLAG2(npr(nsgmnt))
      else
     &if (mc_step .eq. 1) then

         if (nsgmnt .gt. 1 .and.
     &      (hofwr .gt. mc_test .or. phofwr .gt. mc_test)) then

c           The approximate conversion coefficients probably will not
c           be good enough; the rigorous procedure must be used.

            if (nsgmnt .gt. pnsgmnt+1) then

c              LW_FULL_MC has not been run for the data of the
c              previous segment, so it needs to be run for that
c              segment first.

c              Set up for the previous segment
               nsgmnt =nsgmnt-1
               lat    =xla(nsgmnt)
               lon    =xlo(nsgmnt)
               rho    =rng(nsgmnt)
               azim   =azm(nsgmnt)
               dip    =xdp(nsgmnt)
               bfield =fld(nsgmnt)
               sigma  =sgm(nsgmnt)
               epsr   =eps(nsgmnt)
               beta   =bta(nsgmnt)
               hprime =hpr(nsgmnt)
               pindex =npr(nsgmnt)
               nreigen=num(nsgmnt)
               do neigen=1,nreigen
                  tp(neigen)=thetap(nsgmnt,neigen)
               end do

c              Get ionospheric profile
               if (pflag .eq. 5 .or. pflag .eq. 7) then

c                 Get range dependent tabular profile
                  call PRFL_SPECIFICATION
     &                (.false.,print_wf,
     &                 prfl_file,prfl_id,pflag,
     &                 freq,month,day,year,UT,
     &                 lat,lon,rho,dip,zenith,
     &                 hpr_mid,beta,hprime,pindex)
               else

c                 Get exponential profile
                  call PRFL_EXP (beta,hprime)
                  call PRFL_HGTS (print_wf)
               end if

               call LW_FULL_MC (print_mc,print_wf,0)

c              Set the mode conversion flag to be 2
               npr(nsgmnt)=MC_FLAG2(npr(nsgmnt))

c              Re-store the current segment
               nsgmnt =nsgmnt+1
               lat    =xla(nsgmnt)
               lon    =xlo(nsgmnt)
               rho    =rng(nsgmnt)
               azim   =azm(nsgmnt)
               dip    =xdp(nsgmnt)
               bfield =fld(nsgmnt)
               sigma  =sgm(nsgmnt)
               epsr   =eps(nsgmnt)
               beta   =bta(nsgmnt)
               hprime =hpr(nsgmnt)
               pindex =npr(nsgmnt)
               nreigen=num(nsgmnt)
               do neigen=1,nreigen
                  tp(neigen)=thetap(nsgmnt,neigen)
               end do

c              Get ionospheric profile
               if (pflag .eq. 5 .or. pflag .eq. 7) then

c                 Get range dependent tabular profile
                  call PRFL_SPECIFICATION
     &                (.false.,print_wf,
     &                 prfl_file,prfl_id,pflag,
     &                 freq,month,day,year,UT,
     &                 lat,lon,rho,dip,zenith,
     &                 hpr_mid,beta,hprime,pindex)
               else

c                 Get exponential profile
                  call PRFL_EXP (beta,hprime)
                  call PRFL_HGTS (print_wf)
               end if
            end if

            call LW_FULL_MC (print_mc,print_wf,1)

c           Set the mode conversion flag to be 2
            npr(nsgmnt)=MC_FLAG2(npr(nsgmnt))

c           Save the segment number
            pnsgmnt=nsgmnt
         else
         
c           Fast mode conversion ought to be good enough
            call LW_FAST_MC (print_mc)

c           Set the mode conversion flag to be 1
            npr(nsgmnt)=MC_FLAG1(npr(nsgmnt))
         end if

c        Save profile data for this segment
         phofwr=hofwr
      else
     &if (mc_step .eq. 2) then

c        Fast mode conversion
         call LW_FAST_MC (print_mc)

c        Set the mode conversion flag to be 0
         npr(nsgmnt)=MC_FLAG0(npr(nsgmnt))
      else
     &if (mc_step .eq. 3) then

cxx      Get conversion coefficients from a file
cxx      call LW_READ_MC (print_mc)

         write(lwpcLOG_lun,
     &       '(''ERROR [LW_STEP]: '',
     &         ''Option to read conversion coefficients not '',
     &         ''available'')')
         STOP
      end if
      
      if (print_mc .gt. 0 .and. nsgmnt .gt. 1) then

         write(lwpcLOG_lun,
     &       '(/''LW_STEP: '',
     &          ''Conversion coefficients for segment '',i2)')
     &          nsgmnt

         do j=1,pnmode
            write(lwpcLOG_lun,'('' '')')
            do k=1,nreigen
               ar= REAL(a(j,k))
               ai=AIMAG(a(j,k))
               db=10.*LOG10(ar*ar+ai*ai)
               ang=ATAN2(ai,ar)*57.2957795
               write(lwpcLOG_lun,
     &             '(''A('',i2,'','',i2,'')='',1p2e15.5,5x,
     &               ''20*log10(T)='',0p2f10.3)')
     &               k,j,ar,ai,db,ang
            end do
         end do
         write(lwpcLOG_lun,'('' '')')
      end if
      pnmode=nreigen

      RETURN
      END      ! LW_STEP