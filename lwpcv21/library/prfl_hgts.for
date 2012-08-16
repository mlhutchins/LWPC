      SUBROUTINE PRFL_HGTS
     &          (print_hgts)

c***********************************************************************

c     This routine determines the limits of the ionospheric profile.
c     It uses Wait's omega sub r to determine these limits.

c***********************************************************************

c Change history:
c     27 Oct 95     Changed to get the logical unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      integer        pflag,pindex
      real           freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,

     &               hten,algen,htnu,algnu,charge,ratiom,
     &               select_hgts,hgts

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex

     &      /lwpc_pr/hten(51),algen(51,3),nrhten,ihten,
     &               htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &               charge(3),ratiom(3),nrspec,lu_prf,
     &               select_hgts(2),hgts(3)

c     Local
      logical        flag1,flag2,flag3
      integer        print_hgts
      real           en(3),nu(3)

      equivalence   (factor_top,select_hgts(1)),
     &              (factor_bot,select_hgts(2))

      data           cx/3.182357e9/

c     write(lwpcLOG_lun,'(''PRFL_HGTS: nrspec ='',i2)') nrspec
c     write(lwpcLOG_lun,'(''PRFL_HGTS: nrhten ='',i2)') nrhten
c     write(lwpcLOG_lun,'(''PRFL_HGTS: nrhtnu ='',i2)') nrhtnu
c     write(lwpcLOG_lun,'(''PRFL_HGTS: charge ='',3f5.1)') charge
c     write(lwpcLOG_lun,'(''PRFL_HGTS: ratiom ='',1p3e11.3)') ratiom
c     write(lwpcLOG_lun,'(''PRFL_HGTS: Ne'')')
c     do n=1,nrhten
c        write(lwpcLOG_lun,'(f6.1,1p3e11.3)')
c    &         hten(n),(EXP(algen(n,l)),l=1,nrspec)
c     end do
c     write(lwpcLOG_lun,'(''PRFL_HGTS: nu'')')
c     do n=1,nrhtnu
c        write(lwpcLOG_lun,'(f6.1,1p3e11.3)')
c              htnu(n),(EXP(algnu(n,l)),l=1,nrspec)
c     end do

      omega=6.283185308e3*freq

      wr_top=2.5e5*factor_top
      wr_hpr=2.5e5
      wr_bot=2.5e5*factor_bot

c     Start at the bottom of the profile and work up.

      if (wr_bot .eq. 0.) then
         flag1=.true.
         botht=0.
      else
         flag1=.false.
      end if
      flag2=.false.
      if (wr_top .eq. 0.) then
         flag3=.true.
         topht=hten(1)
      else
         flag3=.false.
      end if
c     write(lwpcLOG_lun,
c    &    '(''PRFL_HGTS: flag   ='',3l5)')
c    &      flag1,flag2,flag3

      hh=hten(nrhten)
      do while (.not.flag1 .or. .not.flag2 .or. .not.flag3)

         call PRFL_EN NU (hh,en,nu)
         wr=cx*en(1)/nu(1)/ratiom(1)
         if (nrspec .gt. 1) then
            wr=wr+cx*en(2)/nu(2)/ratiom(2)
     &           +cx*en(3)/nu(3)/ratiom(3)
         end if

         if (.not.flag1 .and. wr .gt. wr_bot) then
            htop=hh
            hbot=hh-5.
            do while (ABS(htop-hbot) .ge. .1)
               hh=(htop+hbot)/2.
               call PRFL_EN NU (hh,en,nu)
               wr=cx*en(1)/nu(1)/ratiom(1)
               if (nrspec .gt. 1) then
                  wr=wr+cx*en(2)/nu(2)/ratiom(2)
     &                 +cx*en(3)/nu(3)/ratiom(3)
               end if
               if (wr .gt. wr_bot) then
                  htop=hh
               else
                  hbot=hh
               end if
            end do
            botht=hh
            flag1=.true.
         end if
         if (.not.flag2 .and. wr .gt. wr_hpr) then
            htop=hh
            hbot=hh-5.
            do while (ABS(htop-hbot) .ge. .1)
               hh=(htop+hbot)/2.
               call PRFL_EN NU (hh,en,nu)
               wr=cx*en(1)/nu(1)/ratiom(1)
               if (nrspec .gt. 1) then
                  wr=wr+cx*en(2)/nu(2)/ratiom(2)
     &                 +cx*en(3)/nu(3)/ratiom(3)
               end if
               if (wr .gt. wr_hpr) then
                  htop=hh
               else
                  hbot=hh
               end if
            end do
            hofwr=hh
            flag2=.true.
         end if
         if (.not.flag3 .and. wr .gt. wr_top) then
            htop=hh
            hbot=hh-5.
            do while (ABS(htop-hbot) .ge. .1)
               hh=(htop+hbot)/2.
               call PRFL_EN NU (hh,en,nu)
               wr=cx*en(1)/nu(1)/ratiom(1)
               if (nrspec .gt. 1) then
                  wr=wr+cx*en(2)/nu(2)/ratiom(2)
     &                 +cx*en(3)/nu(3)/ratiom(3)
               end if
               if (wr .gt. wr_top) then
                  htop=hh
               else
                  hbot=hh
               end if
            end do
            topht=hh
            flag3=.true.
         end if
         if (hh .ge. hten(1)) then
            topht=hten(1)
            flag3=.true.
         end if
         hh=hh+5.
      end do

      hgts(1)=topht
      hgts(2)=hofwr
      hgts(3)=botht

      if (print_hgts .gt. 0) then

         write(lwpcLOG_lun,
     &       '(''PRFL_HGTS:''/''Electron parameters:''/
     &         ''  ht     ne       nu        x         z         '',
     &         '' w'')')
         do n=1,3
            hh=hgts(n)
            call PRFL_EN NU (hh,en,nu)
            capx=en(1)*cx
            capz=nu(1)/omega
            wr=cx*en(1)/nu(1)
            write(lwpcLOG_lun,
     &          '(1x,f5.1,1p6e10.2)')
     &            hh,en(1),nu(1),capx,capz,wr
         end do
      end if

c     Make sure that TOP HT is greater than or equal to H of Wr
      topht=MAX(topht,hofwr)
            
      RETURN
      END      ! PRFL_HGTS