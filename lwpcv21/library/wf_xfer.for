      SUBROUTINE WF_XFER
     &          (a,b,n)

c     Transfer array A into array B.

      real     *  8 a(1),b(1)

      do j=1,n
         b(j)=a(j)
      end do
      RETURN
      END      ! WF_XFER