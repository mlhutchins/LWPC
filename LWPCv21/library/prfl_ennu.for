      SUBROUTINE PRFL_EN NU
     &          (ht,en,nu)

c     This routine provides for output of electron density and collision
c     frequency parameters

      real           hten,algen,htnu,algnu,charge,ratiom,
     &               select_hgts,hgts

      common/lwpc_pr/hten(51),algen(51,3),nrhten,ihten,
     &               htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &               charge(3),ratiom(3),nrspec,lu_prf,
     &               select_hgts(2),hgts(3)
c      common/lwpc_in/topht,botht

c     Local
      logical        first/.true./
      real           en(3),nu(3)

      if (first) then

c        Initialize the array counters
         ihten=1
         ihtnu=1
         first=.false.
      end if
      if (ht .gt. hten(ihten) .or. ht .le. hten(ihten+1)) then
         if (ht .gt. hten(ihten)) then
            do while (ihten .gt. 1 .and. ht .ge. hten(ihten-1))
               ihten=ihten-1
            end do
            if (ihten .eq. 1) then

c              Extrapolation above top of profile
               j1=1
               j2=2
            else
               j1=ihten-1
               j2=ihten
            end if
         else
            do while (ihten .lt. nrhten .and. ht .le. hten(ihten+1))
               ihten=ihten+1
            end do
            if (ihten .eq. nrhten) then

c              Extrapolate below the profile
               j1=nrhten-1
               j2=nrhten
            else
               j1=ihten
               j2=ihten+1
            end if
         end if
      else
         j1=ihten
         j2=ihten+1

      end if
      slope=(ht-hten(j1))/(hten(j2)-hten(j1))
      do n=1,nrspec
         en(n)=EXP(algen(j1,n)+(algen(j2,n)-algen(j1,n))*slope)
      end do

      if (ht .gt. htnu(ihtnu) .or. ht .le. htnu(ihtnu+1)) then
         if (ht .gt. htnu(ihtnu)) then
            do while (ihtnu .gt. 1 .and. ht .ge. htnu(ihtnu-1))
               ihtnu=ihtnu-1
            end do
            if (ihtnu .eq. 1) then

c              Extrapolation above top of profile
               j1=1
               j2=2
            else
               j1=ihtnu-1
               j2=ihtnu
            end if
         else
            do while (ihtnu .lt. nrhtnu .and. ht .le. htnu(ihtnu+1))
               ihtnu=ihtnu+1
            end do
            if (ihtnu .eq. nrhtnu) then

c              Extrapolate below the profile
               j1=nrhtnu-1
               j2=nrhtnu
            else
               j1=ihtnu
               j2=ihtnu+1
            end if
         end if
      else
         j1=ihtnu
         j2=ihtnu+1
      end if
      slope=(ht-htnu(j1))/(htnu(j2)-htnu(j1))
      do n=1,nrspec
         nu(n)=EXP(algnu(j1,n)+(algnu(j2,n)-algnu(j1,n))*slope)
      end do

      RETURN
      END      ! PRFL_EN NU