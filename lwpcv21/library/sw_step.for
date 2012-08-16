      SUBROUTINE SW_STEP
     &          (rho0,rho,azim,dip,bfield)

c***********************************************************************

c     rho0          Initial distance
c     rho           Current distance
c     azim          Magnetic field parameters
c     dip
c     bfield

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     14 Feb 98     Test for too large a change in extrapolated modes.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxeigen=50)

      real     *  4 ranger,rangei,atnmax,lub,h
      complex  *  8 eigen

      common/lwpc_mf/
     &              ranger(2),rangei(2),atnmax,lub,h,
     &              eigen(mxeigen),nreigen

      integer       print_swg
      real     *  4 drmin,drmax

      common/sw_path/
     &              drmin,drmax,mdir,lostm,lx,print_swg

      logical       end loop

      data          r1/0./,rhop/0./

      intrp=0
      drho=rho-r1
      end loop=.false.
      do while (.not.end loop)

         lostm=0
         if (rho .gt. rho0) call SW_EXTRAP
         if (lostm .lt. 2) then ! no error in SW_EXTRAP
            call SW_WVGD
            if (print_swg .gt. 0)
     &         write(lwpcLOG_lun,
     &             '(''Output from SW_WVGD: '',
     &               ''LOSTM='',i1/(8f9.4))')
     &                 lostm,(eigen(j),j=1,nreigen)
            if (lostm .lt. 2) then ! no error in SW_WVGD
               call SW_XSAVE
               if (lostm .lt. 2) then ! no error in SW_XSAVE
                  rhop=rho
                  if (intrp .ne. 1) then ! not currently interpolating
                     r1=rho
                     a1=azim
                     d1=dip
                     b1=bfield
                     RETURN ! reached end of segment
                  end if
               else
                  if (print_swg .gt. 0)
     &               write(lwpcLOG_lun,
     &                   '(/''[SW_STEP] WARNING: '',
     &                      ''SW_SAVE returned LOSTM='',i1)') lostm
               end if
            else
               if (print_swg .gt. 0)
     &            write(lwpcLOG_lun,
     &                '(/''[SW_STEP] WARNING: '',
     &                   ''SW_WVGD returned LOSTM='',i1)') lostm
            end if
         else
            if (print_swg .gt. 0)
     &         write(lwpcLOG_lun,
     &             '(/''[SW_STEP] WARNING: '',
     &                ''SW_EXTRAP returned LOSTM='',i1)') lostm
            if (lostm .eq. 5) RETURN
         end if

c        Interpolation:

         if (intrp .eq. 0) then ! set up interpolation
            if (rho .eq. rho0) then
               if (print_swg .gt. 0)
     &            write(lwpcLOG_lun,
     &                '(''[SW_STEP] WARNING: '',
     &                  ''Failure at starting rho'')')
               lostm=3
               RETURN
            end if
            if (rho-r1 .lt. 50.) then
               if (print_swg .gt. 0)
     &            write(lwpcLOG_lun,
     &                '(/''[SW_STEP] WARNING: '',
     &                   ''Backup less than 50 km '',
     &                   ''is not allowed'')')
               lostm=4
               RETURN
            end if
            if (rho-r1 .gt. 1000.) then
               if (print_swg .gt. 0)
     &            write(lwpcLOG_lun,
     &                '(/''[SW_STEP] WARNING: '',
     &                   ''Distance interval is too large for '',
     &                   ''efficient extrapolation'')')
               lostm=5
               RETURN
            end if

c           Begin interpolation
            intrp=1
            drho=AINT(.5*(rho-r1)+.5)
            r2=rho
            a2=azim
            if (a2-a1 .gt.  180.) then
               a2=a2-360.
            else
     &      if (a2-a1 .lt. -180.) then
               a2=a2+360.
            end if
            d2=dip
            b2=bfield
            rho=r1+drho
         else

c           Continuation of interpolation
            if (lostm .eq. 0) then
c              Try to get to end of segment
               intrp=2
               drho=r2-rhop
               rho=r2
            else
               if (lostm .eq. 1) then
c                 Move half way to end of the segment
                  drho=AINT(.5*(r2-rhop)+.5)
               else
c                 Attempt to back up
                  if (intrp .eq. 2) then
c                    Last interval was too large
                     intrp=1
                     drho=r2-rhop
                  end if
c                 Reduce the step size
                  drho=AINT(.5*drho+.5)
               end if
               if (drho .lt. 50.) then
                  if (print_swg .gt. 0)
     &               write(lwpcLOG_lun,
     &                   '(/''[SW_STEP] WARNING: '',
     &                      ''Backup less than 50 km '',
     &                      ''is not allowed'')')
                  lostm=4
                  RETURN
               end if
               rho=rhop+drho
            end if
         end if
         slope=(rho-r1)/(r2-r1)
         azim=a1+slope*(a2-a1)
         dip=d1+slope*(d2-d1)
         bfield=b1+slope*(b2-b1)
         if (azim .lt.   0.) then
            azim=azim+360.
         else
     &   if (azim .gt. 360.) then
            azim=azim-360.
         end if

         if (print_swg .gt. 0)
     &      write(lwpcLOG_lun,
     &          '(/''Interpolated path parameters:''/
     &             ''  rho   azim    dip  bfield''/
     &             f6.0,2f7.2,f7.4)') rho,azim,dip,bfield
      end do

      RETURN
      END      ! SW_STEP