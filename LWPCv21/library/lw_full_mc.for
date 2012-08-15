      SUBROUTINE LW_FULL_MC
     &          (print_mc,print_wf,jflag)

c***********************************************************************

c     Compute conversion coefficients using rigorous formulation.

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

c     stp           sine(theta')
c     nreigen       number of theta'
c     nsgmnt        path segment number

c     OUTPUT PARAMETERS:

c     a             conversion coefficients

c     Note that CAPI and NORM are used differently here than in
c     LW_FAST_MC, but it's OK because the parameters are not passed
c     between this routine and LW_FAST_MC.

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     06 Sep 98     Added test to delete modes near (90,0).

c*******************!***************************************************

      IMPLICIT REAL*8 (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxeigen=50,nrht=65)

c     LWPC
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

c     Mode Conversion
      character*120 prfl_file
      integer       lu_wfd,lu_wfa,lu_wfp,mc_step,arb_azim
      real     *  8 max_wf_ht,mc_test
      complex  *  8 a,tp,stp,xtra,eyhy
      complex  * 16 norm,capi

      common/mc_inpt/
     &              prfl_file,max_wf_ht,mc_test,
     &              lu_wfd,lu_wfa,lu_wfp,mc_step,arb_azim
     &      /mc_slab/
     &              a(mxeigen,mxeigen),tp(mxeigen),stp(mxeigen),
     &              xtra(3,mxeigen),eyhy(mxeigen),nreigen,nsgmnt
     &      /mc_stor/
     &              norm(mxeigen),capi(mxeigen,mxeigen)

c     Wave Fields
      real     *  8 freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max_ht,del_ht,top_ht,bot_ht,
     &              dtheta,lub,thtinc,alpha,h,prec
      complex  * 16 eigen2

      common/wf_inpt/
     &              eigen2,
     &              freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max_ht,del_ht,top_ht,bot_ht
     &      /wf_iter/
     &              dtheta(2),lub(2),thtinc,alpha,h,prec,
     &              iter_flag,mxiter,nriter

c     Local
      logical       first/.true./
      integer       print_mc,print_wf,pnmode
      complex  * 16 MC_P_INTEG,
     &              eigen,sine,exc(3),
     &              eyk,ezk,hyk,hzk,eym,ezm,hym,hzm,prod

      common        eyk(nrht),ezk(nrht),hyk(nrht),hzk(nrht),
     &              eym(nrht),ezm(nrht),hym(nrht),hzm(nrht),prod(nrht)

      data          dtr/1.745329252d-2/


      if (first) then

c        Open scratch files
c        NOTE: On a VMS machine, use 1060 instead of 4240.

c        Wave fields: direct
         OPEN (lu_wfd,access='direct',status='scratch',recl=4240)

c        Wave fields: adjoint
         OPEN (lu_wfa,access='direct',status='scratch',recl=4240)

c        Wave fields: direct; from previous slab
         OPEN (lu_wfp,access='direct',status='scratch',recl=4240)

         first=.false.
      end if

c*****
c     Transfer variables from LWPC to WF commons
      freq2  =freq
      azim2  =azim
      codip2 =90.-dip
      magfld2=bfield*1.e-4
      sigma2 =sigma
      epsr2  =epsr

c     Set height intervals for WF_DRIVER to give NRHT points

c     MAX_HT defines a uniform top of the ionosphere for calculating the
c     wave fields across slab boundaries while TOP_HT and BOT_HT can
c     change from one slab to the next.
      max_ht=max_wf_ht
      del_ht=DINT(max_ht+.99)/(nrht-1)

      top_ht=DINT(top ht/del_ht+.99)*del_ht
      bot_ht=DINT(bot ht/del_ht    )*del_ht
      if (print_wf .gt. 0) then
         write(lwpcLOG_lun,
     &       '(/''LW_FULL_MC:''/
     &          ''   rho    lat     lon    azim   dip  bfield'',
     &          ''   sigma  epsr  beta   h''''''/
     &          f7.0,f7.2,f8.2,2f7.2,f7.4,1pe9.1,0pf5.1,2f6.2)')
     &          rho,lat,lon,azim,dip,bfield,sigma,epsr,beta,hprime
         write(lwpcLOG_lun,
     &       '(/''   bot   ='',f6.1,5x,'' top   ='',f6.1,5x,
     &          '' max_ht='',f6.1/
     &          ''   bot_ht='',f11.6,  '' top_ht='',f11.6,
     &          '' del_ht='',f11.6/)')
     &          bot ht,top ht,max_ht,bot_ht,top_ht,del_ht
      end if
c*****

c     Loop over WF_DRIVER to get fields vs. height

c     Temporary (?) fix for failure to converge to a solution:
c        Drop the offending mode

c     do neigen=1,nreigen
c        eigen2=tp(neigen)
c        call WF_DRIVER
c    &       (lwpcLOG_lun,lu_wfd,lu_wfa,print_wf,nsgmnt,neigen,arb_azim)
c     end do

      neigen=1
      do while (neigen .le. nreigen)
         eigen2=tp(neigen)
         call WF_DRIVER
     &       (lwpcLOG_lun,lu_wfd,lu_wfa,print_wf,nsgmnt,neigen,arb_azim)

         if ((nriter .gt. mxiter) .or.
     &       (ABS(DREAL(eigen2)-90.d0) .lt. lub(1) .and.
     &        ABS(DIMAG(eigen2)      ) .lt. lub(2)) .or.
     &       (-182.d0*freq2*DIMAG(SIN(eigen2*dtr)) .lt. .01)) then

c           Drop mode if there were too many iterations or
c                        the mode is invalid (close to 90,0)
            nreigen=nreigen-1
            do n=neigen,nreigen
               tp  (n)=tp  (n+1)
               stp (n)=stp (n+1)
               eyhy(n)=eyhy(n+1)
               do j=1,3
                  xtra(j,n)=xtra(j,n+1)
               end do
            end do
         else

c           Keep the current mode
            neigen=neigen+1
         end if
      end do

c     Calculate normalization terms
      err max=0.d0
      do m=1,nreigen

c        Get direct fields
         read (lu_wfd,rec=m) eigen,sine,exc,eym,ezm,hym,hzm

         if (iter_flag .eq. 1) then

c           Store revised eigen angles (from WF_ITRATE)
            tp(m)=eigen
            stp(m)=sine

c           Get revised ratio Ey/Hy at the ground
            eyhy(m)=eym(1)

            if (nsgmnt .eq. 1) then

c              Get revised excitation factors (from WF_RBARS)
               xtra(1,m)=exc(1)
               xtra(2,m)=exc(2)
               xtra(3,m)=exc(3)
            end if
         end if

         if (arb_azim .eq. 0) then

c           Get direct fields
            read (lu_wfd,rec=m) eigen,sine,exc,eyk,ezk,hyk,hzk
         else

c           Get adjoint fields
            read (lu_wfa,rec=m) eigen,sine,exc,eyk,ezk,hyk,hzk
         end if
         do j=1,nrht
            prod(j)=hzm(j)*eyk(j)-hym(j)*ezk(j)
     &             -ezm(j)*hyk(j)+eym(j)*hzk(j)
         end do
         norm(m)=MC_P_INTEG (del_ht,nrht,prod,err)
         if (err max .lt. err) err max=err

         if (print_mc .gt. 1)
     &      write(lwpcLOG_lun,
     &          '(''LW_FULL_MC: '',
     &            ''NORM('',i2,'')='',1p2e15.5,1x,
     &            ''err='',1pe12.2)')
     &              m,norm(m),err
      end do

      if (print_mc .gt. 1)
     &   write(lwpcLOG_lun,
     &       '(''LW_FULL_MC: '',
     &         ''NORM: Max. error ='',1pe10.2)')
     &           err max

      if (nsgmnt .eq. 1) then

         do k=1,nreigen
            do m=1,nreigen
               if (k .eq. m) then
                  a(k,m)=(1.d0,0.d0)
               else
                  a(k,m)=(0.d0,0.d0)
               end if
            end do
         end do
      else

         if (jflag .eq. 1) then

            err max=0.d0
            do k=1,pnmode

c              Get direct fields from previous slab

               read (lu_wfp,rec=k) eigen,sine,exc,eyk,ezk,hyk,hzk

               do m=1,nreigen
                  if (arb_azim .eq. 0) then

c                    Get direct fields
                     read (lu_wfd,rec=m) eigen,sine,exc,eym,ezm,hym,hzm
                  else

c                    Get adjoint fields
                     read (lu_wfa,rec=m) eigen,sine,exc,eym,ezm,hym,hzm
                  end if
                  do j=1,nrht
                     prod(j)=hzm(j)*eyk(j)-hym(j)*ezk(j)
     &                      -ezm(j)*hyk(j)+eym(j)*hzk(j)
                  end do
                  capi(m,k)=MC_P_INTEG (del_ht,nrht,prod,err)
                  if (err max .lt. err) err max=err

                  if (print_mc .gt. 1)
     &               write(lwpcLOG_lun,
     &                   '(''LW_FULL_MC: '',
     &                     ''CAPI('',i2,'','',i2,'')='',1p2e15.5,1x,
     &                     ''err='',1pe12.2)')
     &                       k,m,capi(m,k),err
               end do
            end do
            if (print_mc .gt. 1)
     &         write(lwpcLOG_lun,
     &             '(''LW_FULL_MC: '',
     &               ''CAPI: Max. error ='',1pe10.2)')
     &                 err max

c           Compute total conversion
            do k=1,pnmode
               do m=1,nreigen
                  a(k,m)=capi(m,k)/norm(m)
               end do
            end do
         end if
      end if

      pnmode=nreigen
      do k=1,nreigen
         read (lu_wfd,rec=k) eigen,sine,exc,eyk,ezk,hyk,hzk
         write(lu_wfp,rec=k) eigen,sine,exc,eyk,ezk,hyk,hzk
      end do

      RETURN
      END      ! LW_FULL_MC