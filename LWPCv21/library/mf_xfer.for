      SUBROUTINE MF_XFER (a,b,n)

c     Routine for transferring the values of one complex array into
c     another.

      complex a(1),b(1)


      do i=1,n
         b(i)=a(i)
      end do
      return

      END      ! MF_XFER