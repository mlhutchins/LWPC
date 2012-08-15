      SUBROUTINE MF_WHICH

      complex      bpt

      common/mf_bpts/bpt(6),nobpts
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_side/tlft,trht,ttop,tbot

      data tmargn/2.0/


      noevct=0
      if (lowg   .ne. 0) noevct=1
      if (iexact .ne. 0) noevct=1
      if (nobpts .ne. 0) noevct=1
      if (noevct .ne. 0) return

      do i=1,6
         bpr= real(bpt(i))
         bpi=aimag(bpt(i))
         if (bpr .gt. tlft-tmargn .and. bpr .lt. trht+tmargn .and.
     &       bpi .gt. tbot-tmargn .and. bpi .lt. ttop+tmargn)
     &     noevct=1
      end do
      return
      END      ! MF_WHICH