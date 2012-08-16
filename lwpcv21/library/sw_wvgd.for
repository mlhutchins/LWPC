      SUBROUTINE SW_WVGD

c***********************************************************************

c     This routine drives the generation of mode parameters using the
c     input EIGEN list.
c     If RPOLY is 0, then all calculations are made exactly.
c     If RPOLY is 2, then all calculations are made approximately using
c     the routine SW_RPLYNM.
c     If RPOLY is 1, then the initial calculations are approximate to
c     refine the initial solutions and the final solutions are obtained
c     using the exact formulation.

c  Change History:
c     28 Dec 93     Changed mode counting algorithm to allow
c                   MXEIGEN inputs; dropped test on attenuation
c                   rate when the iterations produce a bad result.

c     11 Sep 95     Made FMAG double precision so that this routine
c                   doesn't die when SW_ITERAT returns a bad mode.

c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     26 Apr 96     Changed to generic functions.

c     06 Sep 98     Added test to delete modes near (90,0).

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

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

      real     *  8 omega,wn,thetar,thetai
      complex  * 16 c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22

      common/sw_mode/
     &              omega,wn,thetar,thetai,c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22,
     &              nriter,adjflg,isotrp

      real     *  8 delh
      complex  * 16 r11,r22,r12,r21,
     &              logr11,logr22,logr12,logr21,
     &              dl11dh,dl22dh,dl12dh,dl21dh

      common/sw_rmtx/
     &              r11,r22,r12,r21,
     &              logr11,logr22,logr12,logr21,
     &              dl11dh,dl22dh,dl12dh,dl21dh,delh

      character* 20 reason,blank
      logical       loop
      integer       psave
      real     *  8 capk,fmag,reflht
      complex  * 16 theta,theta0,stp,
     &              ratio,store1,store2,store3,term1,term2,
     &              wterm,cxlog,mik,
     &              zmplxi,zone,zdtr

      equivalence  (thetar,theta)

      data          blank/'                    '/,
     &              reflht/70.d0/
     &              zmplxi/(0.d0,1.d0)/,zone/(1.d0,0.d0)/,
     &              zdtr/(1.745329252d-2,0.d0)/


      psave=rpoly

c     Count the number of modes
Change: 28 Dec 93
      neigen=0
      loop=.true.
      do while (loop)
         if (neigen .eq. mxeigen) then
            loop=.false.
         else
            if (REAL(eigen(neigen+1)) .eq. 0.) then
               loop=.false.
            else
               neigen=neigen+1
            end if
         end if
      end do
Change: END
      if (neigen .eq. 0) then
         if (print_swg .gt. 1)
     &      write(lwpcLOG_lun,
     &          '(''[SW_WVGD]: No modes input'')')
         lostm=2
         RETURN
      else
     &if (neigen .eq. 1) then
         rpoly=0
      end if

Change: 09 Oct 92
c     capk=1./(1.-.5*alpha*h
      capk=SQRT(1.-alpha*h)
Change: END
      omega=6.283185306d3*freq
      wn=2.0958426d-2*freq
      wterm=DCMPLX(0.d0,-.5d0*wn*reflht)
      mik=DCMPLX(0.d0,-1.d3*wn)
      adjflg=0
      if (bfield .le. 1.e-10) then
         isotrp=1
      else
         if (dip.eq.0. .and. (azim.eq.90. .or. azim.eq.270.)) then
            isotrp=2
         else
            isotrp=0
         end if
      end if

      if (rpoly .gt. 0) call SW_RPLYNM

      if (print_swg .gt. 1)
     &   write(lwpcLOG_lun,
     &       '(/''   initial   iter    final'',6x,''mag f   mag p'',
     &          ''   atten  v/c'',6x,''Wait''''s exc'')')

Change: 28 Dec 93
10    kn=0
      km=neigen
      loop=.true.
      do while (loop)
         if (kn .eq. km) then
            loop=.false.
         else
            kn=kn+1
            theta0=eigen(kn)
            theta=theta0

            call SW_ITERAT
            thtr=thetar
            thti=thetai

c           Calculate magnitude of the F function and polarization
            fmag=ABS(f)
            term1=rbar22*r21
            term2=zone-rbar11*r11
            if (ABS(term1) .gt. ABS(term2)) then
               pmag=ABS((zone-rbar22*r22)/term1)
            else
               pmag=ABS(rbar11*r12/term2)
            end if

c           Check validity of solution
            reason=blank
            if (nriter .ge. maxitr .and. fmag .gt. ftol) then
c              If not maximum iterations,
c              then the iteration stopped on TOL;
c              in that case, ignore FMAG;
c              otherwise, drop the mode
               write(reason,'(''F mag='',1pe8.2)') fmag
            else
     &      if (thti .ge. 0.) then
c              If imaginary part of the solution is greater than 0,
c              then the mode is invalid
               write(reason,'(''THETAi .gt. 0'')')
            else
     &      if (ABS(thtr-90.) .lt. tol(1) .and.
     &          ABS(thti    ) .lt. tol(2)) then
c              If the mode is close to 90,0,
c              then it is invalid
               write(reason,'(''THETA .eq. 90,0'')')
            else
     &      if (-182.*freq*DIMAG(SIN(theta*zdtr)) .lt. .01) then
c              If the attenuation rate is close to zero
c              then the mode is close to 90,0, which is invalid
               write(reason,'(''THETA .eq. 90,0'')')
            else
     &      if (kn .gt. 1) then
c              Look for duplicate modes
               kd=1
               do while (reason .eq. blank .and. kd .lt. kn)
                  if (ABS(thtr- REAL(eigen(kd))) .le. deigen(1) .and.
     &                ABS(thti-AIMAG(eigen(kd))) .le. deigen(2))
     &               write(reason,'(''It matches mode'',i3)') kd
                  kd=kd+1
               end do
            end if

            if (reason .eq. blank) then

c              Store solution
               eigen(kn)=theta
               if (rpoly .ne. 1) then

                  if (nriter .gt. maxitr/2) then
                     if (print_swg .gt. 1)
     &                  write(lwpcLOG_lun,
     &                      '(''[SW_WVGD] WARNING: '',
     &                        ''Excessive iterations.'')')
                     lostm=1
                  end if
                  s=SIN(theta*zdtr)
Change: 09 Oct 92
c                 stp=s*capk
                  stp=s/capk
Change: END
                  at=-8.6858896d3*wn*DIMAG(stp)
                  vc=1.d0/DREAL(stp)
                  tp(kn)=-zmplxi
     &                  *LOG(SQRT(zone-stp*stp)+zmplxi*stp)/zdtr
                  ratio=SQRT(s)/(dfdtht/zdtr)
                  term1=zone-rbar11*r11
                  term2=zone-rbar22*r22
                  if (ABS(term1) .lt. ABS(term2)) then
                     term1=r12*r21*rbar11*rbar22/term2
                  else
                     term2=r12*r21*rbar11*rbar22/term1
                  end if
                  store1=(zone+rbar11)**2*term2*ratio/rbar11
                  store2=(zone+rbar11)*(zone+rbar22)*ratio
                  store3=(zone+rbar22)**2*term1*ratio/rbar22
                  cxlog=LOG(wterm*store1*(s*hg)**2)
                  wm=DREAL(cxlog)*8.685889638d0
                  wa=DIMAG(cxlog)
                  if (wa .lt. 0.d0) wa=wa+6.283185d0
                  if (print_swg .gt. 1)
     &               write(lwpcLOG_lun,
     &                   '(f6.3,f7.3,i3,2f7.3,1p2e8.1,
     &                     1x,0pf6.2,1x,f7.5,1x,f7.2,1x,f4.2)')
     &                     theta0,nriter,eigen(kn),fmag,pmag,
     &                     at,vc,wm,wa

                  capt(1,kn)=store1/norm11
                  capt(2,kn)=store3/norm22
                  capt(3,kn)=store2/norm12*r21
                  capt(4,kn)=r12/r21
                  if (ABS(zone-r11*rbar11) .ge.
     &                ABS(zone-r22*rbar22)) then
                     fofr(kn)=capt(2,kn)/(capt(3,kn)*capt(4,kn))
                  else
                     fofr(kn)=capt(3,kn)/capt(1,kn)
                  end if
               end if
            else

c              Error handling
               if (rpoly .eq. 1) then ! switch to exacts

                  loop=.false.
               else

                  if (print_swg .gt. 1)
     &               write(lwpcLOG_lun,
     &                   '(f6.3,f7.3,i3,2f7.3,1p2e8.1)')
     &                     theta0,nriter,theta,fmag,pmag
                  if (print_swg .gt. 1)
     &               write(lwpcLOG_lun,
     &                   '(''[SW_WVGD]: Lost mode'',i3,'': '',a)')
     &                     kn,reason

                  if (rho .eq. 0.) then

c                    OK to drop a mode at the transmitter
                     do m=kn,km-1
                        eigen(m)=eigen(m+1)
                     end do
                     if (km .eq. mxeigen) eigen(km)=0.
                     km=km-1
                     kn=kn-1
                  else

c                    Check atenuation rate
                     atten=-8.686d3*wn/capk*DIMAG(SIN(theta0*zdtr))
                     if (atten .lt. atnmax) then
                        rpoly=psave
                        lostm=2
                        RETURN
                     else
                        eigen(kn)=(50.,-90.)
                        tp(kn)=(50.,-90.)
                        lostm=1
                     end if
                  end if
               end if
            end if
         end if
      end do
Change: END

      if (rpoly .eq. 1) then
         rpoly=0
         go to 10
      end if

      nreigen=kn
      if (nreigen .eq. 0) then
         if (print_swg .gt. 1)
     &      write(lwpcLOG_lun,
     &          '(''[SW_WVGD]: Lost all modes'')')
         lostm=2
      end if

      rpoly=psave
      RETURN
      END      ! SW_WVGD