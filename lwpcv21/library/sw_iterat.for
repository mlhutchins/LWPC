      SUBROUTINE SW_ITERAT

      implicit real*8 (a-h,o-z)

c***********************************************************************

c     This routine drives the iteration to obtain solutions to the
c     modal equation.

c  Change History:
c     21 Oct 94     Added SW_PATH so we could use PRINT_SWG during
c                   debugging operations; modified iteration loop
c                   and tests to accept a mode with a poor value
c                   of the F function when the correction to the
c                   solution is small.

c     11 Sep 95     Added test for very large F functions to stop
c                   when it's outrageous.

c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     18 Nov 95     Added coding to find the value of BOTHT which
c                   gives the best value of the F function; adjust
c                   BOTHT if possible when the imaginary part of the
c                   eigen angle becomes positive or when the maximum
c                   number of iterations is exceeded.

c     26 Apr 96     Changed to generic functions.

c     29 Oct 96     Set increment for adjusting BOTHT to be consistent
c                   with LW_FULL_MC (WF_DRIVER).

c     06 Sep 98     Modifed test for adjusting BOTHT to abort the
c                   routine if the initial value of F does not satisfy
c                   the tolerance, which has been changed to 0.5 from
c                   the previous value of 0.1.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

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

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),
     &              thtinc,ftol,maxitr,alpha,prec,rpoly,nrtlst

      complex  * 16 c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22

      common/sw_mode/
     &              omega,wn,thetar,thetai,c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22,
     &              nriter,adjflg,isotrp

      integer       print_swg
      real     *  4 drmin,drmax

      common/sw_path/
     &              drmin,drmax,mdir,lostm,lx,print_swg

      character*120 prfl_file
      integer       lu_wfd,lu_wfa,lu_wfp,mc_step,arb_azim
      real     *  8 max_wf_ht,mc_test

      common/mc_inpt/
     &              prfl_file,max_wf_ht,mc_test,
     &              lu_wfd,lu_wfa,lu_wfp,mc_step,arb_azim

      logical       loop
      real     *  4 botht0,botht1,
     &              absr,absi
      complex  *  8 dthta
      complex  * 16 theta,theta0,f0,dlthta

      equivalence  (thetar,theta),(dtheta,dthta)

      if (print_swg .gt. 2) then
         if (rpoly .eq. 0) then
            write(lwpcLOG_lun,
     &          '(/''Iterations:    exact'')')
         else
            write(lwpcLOG_lun,
     &          '(/''Iterations:  inexact'')')
         end if
      end if

c     Store starting values
      theta0=theta
      botht0=botht

c     Set increment for adjusting D to be consistent with LW_FULL_MC
      delht=DINT(max_wf_ht+.99)/64

      if (rpoly .eq. 0) then

c        Find the bottom height which gives
c        the best chance of finding a solution
         botht=INT(botht/delht)*delht
         if (botht .gt. 0.) then

            botht1=botht
            fmag1=1000.
            loop=.true.
            do while (loop)

               call SW_FCT VAL
               fmag=ABS(f)
cxxcDEBUG
cxx               write(lwpcLOG_lun,
cxx     &             '(''Adjust D:'',f7.4,1pe12.3)') botht,fmag
               if (fmag .lt. fmag1) then

c                 Store the bottom height which gives
c                 the smallest F function
                  botht1=botht
                  fmag1=fmag
               end if
               if (botht .eq. 0. .or. fmag .le. .5) then
                  loop=.false.
               else
                  botht=botht-delht
               end if
            end do
            if (botht .ne. botht1) then

c              Use the bottom height which gives
c              the smallest F function
               botht=botht1
               fmag=fmag1
               if (botht .eq. 0. .and. fmag .gt. .5) then

c                 We aren't going to be able to find a solution
                  f=fmag
                  botht=botht0
                  nriter=maxitr+1
cxxcDEBUG
cxx                  write(lwpcLOG_lun,
cxx     &                '(''Adjust D:  Failed'')')
                  RETURN
               end if
            end if
cxxcDEBUG
cxx               write(lwpcLOG_lun,
cxx     &             '(''Adjust D:'',f7.4,1pe12.3)') botht,fmag
         end if
      end if

      if (print_swg .gt. 2)
     &   write(lwpcLOG_lun,
     &       '(8x,''real    imag     f mag       d real     d imag'',
     &         5x,''dfdt real  dfdt imag'')')

c     Store the magnitude of the F function for the starting angle
      call SW_FCT VAL
      fmag0=ABS(f)

      nriter=0
      mxiter=maxitr
      loop=.true.
      do while (loop)

         theta=theta-dthta
         call SW_FCT VAL
         f0=f
         theta=theta+dthta
         call SW_FCT VAL

         fmag=ABS(f)

         nriter=nriter+1
         dfdtht=(f-f0)/dthta
         dlthta=-f/dfdtht
         if (print_swg .gt. 2)
     &      write(lwpcLOG_lun,
     &          '(5x,2f8.4,1pe12.3,2(1x,2e11.3))')
     &            theta,fmag,dlthta,dfdtht

         absmag=ABS(dlthta)
         if (absmag .gt. thtinc) dlthta=dlthta*(thtinc/absmag)

         theta=theta+dlthta

         absr=ABS(DREAL(dlthta))
         absi=ABS(DIMAG(dlthta))

         if (absr .le. tol(1) .and. absi .le. tol(2)) then

            loop=.false.
         else
     &   if (nriter .eq. mxiter .or. DIMAG(theta) .ge. 0.) then

            loop=.false.
         end if
      end do

      if (absr .le. tol(1) .and. absi .le. tol(2) .and.
     &    fmag .gt. ftol) then

c        Set F function so that SW_WVGD will accept this mode;
c        a small correction to the solution means we're close to a
c        a zero even though the calculated F function is large.
         f=ftol

cxx      else
cxx     &if (ABS(dlthta) .gt. 1.e-10) then
cxx
cxxc        Calculate the derivative using the last correction to the
cxxc        eigen angle.
cxx         nriter=nriter+1
cxx         f0=f
cxx         call SW_FCT VAL
cxx         dfdtht=(f-f0)/dlthta
cxx
cxx         fmag=ABS(f)
cxx         dlthta=0.
cxx         if (print_swg .gt. 2)
cxx     &      write(lwpcLOG_lun,
cxx     &          '(5x,2f8.4,1pe12.3,2(1x,2e11.3))')
cxx     &            theta,fmag,dlthta,dfdtht
cxx
cxx         if (rpoly .eq. 1) then
cxx
cxxc           Test the magnitude of the F function of the final angle
cxx            if (fmag .gt. fmag0) then
cxx
cxxc              Set eigen angle back to the original value
cxx               theta=theta0
cxx               f=fmag0
cxx               if (print_swg .gt. 1)
cxx     &            write(lwpcLOG_lun,
cxx     &                '(''[SW_ITERAT] WARNING: '',
cxx     &                  ''During RPOLY=1, '',
cxx     &                  ''initial fmag ('',1pe10.4,'') '',
cxx     &                  ''is smaller than final fmag '',
cxx     &                  ''('',1pe10.4,'')'')')
cxx     &                    fmag0,fmag
cxx            end if
cxx         end if
      end if

c     Restore the original value of bottom height
      botht=botht0
      RETURN
      END      ! SW_ITERAT