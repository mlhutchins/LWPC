      SUBROUTINE SHIFT_ARRAY2
     &          (array,mxi,mxj,nxi,l,m,nm1)

c***********************************************************************
c                   subroutine shift_array2
c***********************************************************************
c
c  Program Source:  Naval Ocean Systems Center - Code 542
c
c  Date:
c     21 Mar 1995
c
c  Function:
c     Shifts rows of a 2 dimensional array from one location to another.
c     The section to be moved is between indices M and N-1 and it is to
c     be moved to a section beginning at index L.  For example:
c                     L                 M                    N
c         1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
c     gives
c         1  2  3  4 11 12 13 14 15 16 17  5  6  7  8  9 10 18 19 20 21
c
c  Parameters passed:
c     ARRAY          [r,*] array of real values to be processed
c     MXI            [i  ] dimension of ARRAY
c     MXJ            [i  ] dimension of ARRAY
c     NXI            [i  ] number of columns used in ARRAY
c     L              [i  ] target index
c     M              [i  ] starting index of section to be moved
c     NM1            [i  ] ending   index of section to be moved
c
c  Parameters returned:
c     ARRAY          [r,*] rearranged array
c
c  Common blocks referenced:
c
c  Functions and subroutines referenced:
c     mod
c
c  References:
c
c  Change History:
c
c*******************!***************************************************

      integer      col
      integer      delta
      integer      i
      integer      j
      integer      k
      integer      l
      integer      m
      integer      ml
      integer      mxi
      integer      mxj
      integer      n
      integer      nm
      integer      nm1
      integer      n0
      integer      np
      integer      nrp
      integer      nxi

      real         array(mxi,mxj)
      real         temp


      n=nm1+1
      ml=m-l
      nm=n-m
      if (ml .lt. nm) then
         delta=ml
      else
         delta=-nm
      end if
      nrp=n-l
c     Loop over columns
      do col=1,nxi
         np=0
         n0=0
         do while (np .lt. nrp)
            temp=array(col,l+n0)
            i=m+n0
            array(col,l+n0)=array(col,i)
            np=np+1
            j=n
            do while (j .ne. l+n0)
               j=i+delta
               if (j .ge. n) then
                  j=j+l-n
               else
     &         if (j .lt. l) then
                  j=j-l+n
               end if
               array(col,i)=array(col,j)
               np=np+1
               k=i
               i=j
            end do
            array(col,k)=temp
            n0=n0+1
         end do
      end do
      RETURN
      END      ! SHIFT_ARRAY2