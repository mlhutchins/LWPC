      SUBROUTINE XLIN EQ
     &          (a,ae,b,be,x,xe,n,n dim,iflag,err)

c***********************************************************************
c                         subroutine xlineq
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     04 Jan 1996

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
c     ae               [r,nxn] matrix A exponents
c     b                [c,n]   matrix B
c     be               [r,n]   matrix B exponents
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
c     xe               [r,n]   matrix X; the solution vector exponent
c     err              [r]     relative error of the solution vector

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs

c     xc_add
c     xc_norme

c  References:

c  Change History:

c*******************!***************************************************

      implicit      real*8 (a-h,o-z)

      parameter    (max dim=51)

      complex *  16 a (n dim,n dim),b (n dim),x (n dim),t
      real    *   8 ae(n dim,n dim),be(n dim),xe(n dim),te

      dimension     irow(max dim),q(max dim),qe(max dim)

      character*200 error_msg


      if (n .gt. max dim) then
         write(error_msg,
     &       '(''[XLIN EQ]: '',
     &         ''matrix size greater than '',i2)') max dim
         call LWPC_ERROR ('ERROR',error_msg)
      end if

      if (n .eq. 1) then
         x (1)=b (1)/a (1,1)
         xe(1)=be(1)-ae(1,1)
         call XC_NORME (x(1),xe(1))
         RETURN
      end if

      if (iflag .eq. 0) then
         do i=1,n
            q (i)=0.d0
            qe(i)=0.d0
            do j=1,n
               qq =ABS(a (i,j))
               qqe=    ae(i,j)
               if (q(i)/qq*DEXP(qe(i)-qqe) .lt. 1.d0) then
                  q (i)=qq
                  qe(i)=qqe
               end if
            end do
            if (q(i) .eq. 0.d0) then
               loop=40
               write(error_msg,
     &             '(''[XLIN EQ]: '',
     &               ''matrix is singular at loop '',i3)') loop
               call LWPC_ERROR ('ERROR',error_msg)
            end if
         end do
         err  =1.d-15
         ppiv =0.d0
         ppive=0.d0
         do i=1,n
            irow(i)=i
         end do

         do l=1,n
            pivot =0.d0
            pivote=0.d0
            k=l-1
            do i=l,n
               if (k .ge. 1) then
                  do j=1,k
cxx                  a(i,l)=a(i,l)-a(i,j)*a(j,l)
                     call XC_ADD
     &                   (a(i,l),ae(i,l),a(i,l),ae(i,l),
     &                   -a(i,j)*a(j,l),ae(i,j)+ae(j,l))
                  end do
               end if
               t =a (i,l)/q (i)
               te=ae(i,l)-qe(i)
               call XC_NORME (t,te)
               f =ABS(t )
               fe=    te
               if (pivot/f*DEXP(pivote-fe) .le. 1.d0) then
                  pivot =f
                  pivote=fe
                  npivot=i
               end if
            end do
            if (pivot .eq. 0.d0) then
               loop=240
               write(error_msg,
     &             '(''[XLIN EQ]: '',
     &               ''matrix is singular at loop '',i3)') loop
               call LWPC_ERROR ('ERROR',error_msg)
            end if
            if (ppiv/pivot*DEXP(ppive-pivote) .gt. 1.d0)
     &         err=err*ppiv/pivot*DEXP(ppive-pivote)
            ppiv =pivot
            ppive=pivote
            if (npivot .ne. l) then
               q (npivot)=q (l)
               qe(npivot)=qe(l)
               j=irow(l)
               irow(l)=irow(npivot)
               irow(npivot)=j
               do i=1,n
                  t =a (l,i)
                  te=ae(l,i)
                  a (l,i)=a (npivot,i)
                  ae(l,i)=ae(npivot,i)
                  a (npivot,i)=t
                  ae(npivot,i)=te
               end do
            end if
            if (l .ne. n) then
               t =1.d0/a (l,l)
               te=    -ae(l,l)
               k=l+1
               m=l-1
               do i=k,n
                  if (m .ge. 1) then
                     do j=1,m
cxx                     a(l,i)=a(l,i)-a(l,j)*a(j,i)
                        call XC_ADD
     &                      (a(l,i),ae(l,i),a(l,i),ae(l,i),
     &                      -a(l,j)*a(j,i),ae(l,j)+ae(j,i))
                     end do
                  end if
                  a (l,i)=t *a (l,i)
                  ae(l,i)=te+ae(l,i)
                  call XC_NORME (a(l,i),ae(l,i))
               end do
            end if
         end do
         if (err .gt. 1.d-5)then
            write(error_msg,
     &          '(''[XLIN EQ]: '',
     &            ''Has decomposed an ill-conditioned matrix. '',
     &            ''The relative error is '',1pe12.2)') err
               call LWPC_ERROR ('WARNING',error_msg)
          endif
      end if

      j=irow(1)
      x (1)=b (j)/a (1,1)
      xe(1)=be(j)-ae(1,1)
      do i=2,n
         t =0.d0
         te=0.d0
         j=irow(i)
         k=i-1
         do l=1,k
cxx         x(i)=x(i)+a(i,l)*x(l)
            call XC_ADD
     &          (t,te,t,te,a(i,l)*x(l),ae(i,l)+xe(l))
         end do
cxx      x(i)=(b(j)-x(i))/a(i,i)
         call XC_ADD
     &       (t,te,b(j),be(j),-t,te)
         x (i)=t /a (i,i)
         xe(i)=te-ae(i,i)
      end do
      k=n-1
      do i=1,k
         j=n-i
         m=j+1
         t =x (j)
         te=xe(j)
         do l=m,n
cxx         x(j)=x(j)-a(j,l)*x(l)
            call XC_ADD
     &          (t,te,t,te,-a(j,l)*x(l),ae(j,l)+xe(l))
         end do
         x (j)=t
         xe(j)=te
      end do

      RETURN
      END      ! XLIN EQ