      SUBROUTINE PRFL_EXP
     &          (beta,hprime)

c     Routine to calculate exponential profiles given BETA and HPRIME.

      real           hten,algen,htnu,algnu,charge,ratiom,
     &               select_hgts,hgts

      common/lwpc_pr/hten(51),algen(51,3),nrhten,ihten,
     &               htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &               charge(3),ratiom(3),nrspec,lu_prf,
     &               select_hgts(2),hgts(3)

c     Local
      character*200 error_msg

      if (beta*hprime .eq. 0.) then
         write(error_msg,
     &       '(''[PRFL_EXP]: BETA or HPRIME not input'')')
         call LWPC_ERROR('ERROR', error_msg)
      endif

c     Initialize species information
      nrspec=1
      charge(1)=-1.
      ratiom(1)= 1.

c     First, make sure that the collision frequency is consistent
      nrhtnu=2
      htnu(2)=0.
      htnu(1)=200.
      algnu(2,1)=25.925
      algnu(1,1)=-4.075

c     Now, set up electron density
      nrhten=2
      hten(2)=0.
      hten(1)=200.
      algen(2,1)=algnu(2,1)-beta*hprime-9.4517306
      algen(1,1)=algnu(1,1)-beta*hprime-9.4517306+beta*200.
      RETURN
      END      ! PRFL_EXP