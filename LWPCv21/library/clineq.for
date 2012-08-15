      SUBROUTINE CLIN EQ
     &          (a,b,x,n,n dim,iflag,err)

c***********************************************************************
c                         subroutine clineq
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Solves simultaneous linear equations of the form:
c         A*X=L*U*X=B
c     for COMPLEX*16 variables

c     Uses L-U decomposition to find the triangular matrices L and U
c     such that L*U=A.  The matrices L and U are stored in A. This form
c     is used with back substitution to find the solution X of:
c         A*X=L*U*X=B.

c  Parameters passed:
c     a                [c,nxn] matrix A
c     b                [c,n]   matrix B
c     n                [i]     number of elements in the matrices
c     n dim            [i]     dimension of the matrices in the calling
c                              routine
c     iflag            [i]  =0 indicates that the matrix A has not been
c                              decomposed into L and U; after it has
c                              been decomposed, it is replaced by the
c                              matrices L and U;
c                           =1 indicates that the matrix A has already
c                              been decomposed into L and U

c  Parameters returned:
c     x                [c,n]   matrix X; the solution vector
c     err              [r]     relative error of the solution vector

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     cdabs

c  References:

c  Change History:

c*******************!***************************************************

      implicit      real*8 (a-h,o-z)

      parameter    (max dim=51)

      complex *  16 a(n dim,n dim),b(n dim),x(n dim),t

      dimension     irow(max dim), q(max dim)

      character*200 error_msg


      if (n .gt. max dim) then
         write(error_msg,
     &       '(''[CLIN EQ]: '',
     &         ''matrix size greater than '',i2)') max dim
         call LWPC_ERROR ('ERROR',error_msg)
      end if

      if (n .eq. 1) then
         x(1)=b(1)/a(1,1)
         RETURN
      end if

      if (iflag .eq. 0) then
         do i=1,n
            q(i)=0.d0
            do j=1,n
               qq=CDABS(a(i,j))
               if (q(i) .lt. qq)  q(i)=qq
            end do
            if (q(i) .eq. 0.d0) then
               loop=40
               write(error_msg,
     &             '(''[CLIN EQ]: '',
     &               ''matrix is singular at loop '',i3)') loop
               call LWPC_ERROR ('ERROR',error_msg)
            end if
         end do
         err=1.d-15
         ppiv=0.d0
         do i=1,n
            irow(i)=i
         end do

         do l=1,n
            pivot=0.d0
            k=l-1
            do i=l,n
               if (k .ge. 1) then
                  do j=1,k
                     a(i,l)=a(i,l)-a(j,l)*a(i,j)
                  end do
               end if
               f=CDABS(a(i,l))/q(i)
               if (pivot .le. f) then
                  pivot=f
                  npivot=i
               end if
            end do
            if (pivot .eq. 0.d0) then
               loop=240
               write(error_msg,
     &             '(''[CLIN EQ]: '',
     &               ''matrix is singular at loop '',i3)') loop
               call LWPC_ERROR ('ERROR',error_msg)
            end if
            if (ppiv .gt. pivot) err=err*ppiv/pivot
            ppiv=pivot
            if (npivot .ne. l) then
               q(npivot)=q(l)
               j=irow(l)
               irow(l)=irow(npivot)
               irow(npivot)=j
               do i=1,n
                  t=a(l,i)
                  a(l,i)=a(npivot,i)
                  a(npivot,i)=t
               end do
            end if
            if (l .ne. n) then
               t=1.d0/a(l,l)
               k=l+1
               m=l-1
               do i=k,n
                  if (m .ge. 1) then
                     do j=1,m
                        a(l,i)=a(l,i)-a(l,j)*a(j,i)
                     end do
                  end if
                  a(l,i)=t*a(l,i)
               end do
            end if
         end do
         if (err .gt. 1.d-5)then
            write(error_msg,
     &          '(''[CLIN EQ]: '',
     &            ''Has decomposed an ill-conditioned matrix. '',
     &            ''The relative error is '',1pe12.2)') err
               call LWPC_ERROR ('WARNING',error_msg)
          endif
      end if

      do i=2,n
         x(i)=0.d0
      end do
      j=irow(1)
      x(1)=b(j)/a(1,1)
      do i=2,n
         j=irow(i)
         k=i-1
         do l=1,k
            x(i)=x(i)+a(i,l)*x(l)
         end do
         x(i)=(b(j)-x(i))/a(i,i)
      end do
      k=n-1
      do i=1,k
         j=n-i
         m=j+1
         do l=m,n
            x(j)=x(j)-x(l)*a(j,l)
         end do
      end do

      RETURN
      END      ! CLIN EQ