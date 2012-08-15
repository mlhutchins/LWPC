      SUBROUTINE SHIFT_ARRAY
     &          (array,mxa,l,m,nm1)

c***********************************************************************
c                   subroutine shift_array
c***********************************************************************
c
c  Program Source:  Naval Ocean Systems Center - Code 542
c
c  Date:
c     02 Dec 1994
c
c  Function:
c     Shifts a section of an array from one location to another.  The
c     section to be moved is between indices M and N-1 and it is to be
c     moved to a section beginning at index L.  For example:
c                     L                 M                    N
c         1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
c     gives
c         1  2  3  4 11 12 13 14 15 16 17  5  6  7  8  9 10 18 19 20 21
c
c  Parameters passed:
c     ARRAY          [r,*] array of real values to be processed
c     MXA            [i  ] dimension of ARRAY
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

      integer      delta
      integer      i
      integer      j
      integer      k
      integer      l
      integer      m
      integer      ml
      integer      mxa
      integer      n
      integer      nm
      integer      nm1
      integer      n0
      integer      np
      integer      nrp

      real         array(mxa)
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
      np=0
      n0=0
      do while (np .lt. nrp)
         temp=array(l+n0)
         i=m+n0
         array(l+n0)=array(i)
         np=np+1
         j=n
         do while (j .ne. l+n0)
            j=i+delta
            if (j .ge. n) then
               j=j+l-n
            else
     &      if (j .lt. l) then
               j=j-l+n
            end if
            array(i)=array(j)
            np=np+1
            k=i
            i=j
         end do
         array(k)=temp
         n0=n0+1
      end do
      RETURN
      END      ! SHIFT_ARRAY