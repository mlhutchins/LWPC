      SUBROUTINE SORTR
     &          (array,nra,index,nri,ii,jj)

c***********************************************************************
c                         subroutine sortr
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Sorts a single precision array into order of increasing value;
c     the original order of the values in the array will be saved in
c     the index array if it is dimensioned greater than 1. This
c     allows sorting of associated arrays within the calling routine.
c     For instance, suppose one has three arrays, one is range and the
c     others are the corresponding latitude and longitude. In the
c     calling routine, set INDEX(k)=k as the arrays RANGE, LATITUDE and
c     LONGITUDE are filled. Call this routine to sort according to
c     range; the coordinates of the sorted position RANGE(k) are found
c     in LATITUDE(INDEX(k)) and LONGITUDE(INDEX(k)).

c  Parameters passed:
c     array            [r,*] array to be sorted
c     nra              [i]   the dimension of the array
c     index            [i]   array of indices of the values of ARRAY
c     nri              [i]   the dimension of INDEX;
c                            if not 1, it is assummed that NRI=NRA
c     ii               [i]   the index at which to start the sort
c     jj               [i]   the index at which to end   the sort

c  Parameters returned:
c     array            [r,*] the sorted array
c     index            [i]   array of indices of the values of ARRAY

c  Common blocks referenced:

c  Functions and subroutines referenced:

c  References:
c     Algorithm 347,R.C.Singleton,Communications of the ACM,V12,N3,Mar69

c     The only arithmetic operation on ARRAY is subtraction. The user
c     should consider the possibility of integer overflow. Arrays IU(K)
c     and IL(K) permit sorting up to 2**(k+1)-1 elements.

c  Change History:

c*******************!***************************************************

      integer       index(*)
      real          array(*)

      character*200 error_msg

      dimension     iu(36),il(36)


      if (jj .gt. nra) then
         write(error_msg,
     &       '(''[SORTR]: jj gt nra'')')
         call LWPC_ERROR ('WARNING',error_msg)
      end if

      m=1
      i=ii
      j=jj
5     if (i .ge. j) go to 70
10    k=i
      ij=(i+j)/2
      t=array(ij)
      if (nri .le. 1) go to 15
      n=index(ij)
15    if (array(i) .le. t) go to 20
      array(ij)=array(i)
      array(i)=t
      t=array(ij)
      if (nri .le. 1) go to 20
      index(ij)=index(i)
      index(i)=n
      n=index(ij)
20    l=j
      if (array(j) .ge. t) go to 40
      array(ij)=array(j)
      array(j)=t
      t=array(ij)
      if (nri .le. 1) go to 25
      index(ij)=index(j)
      index(j)=n
      n=index(ij)
25    if (array(i) .le. t) go to 40
      array(ij)=array(i)
      array(i)=t
      t=array(ij)
      if (nri .le. 1) go to 40
      index(ij)=index(i)
      index(i)=n
      n=index(ij)
      go to 40
30    array(l)=array(k)
      array(k)=tt
      if (nri .le. 1) go to 40
      index(l)=index(k)
      index(k)=nn
40    l=l-1
      if (array(l) .gt. t) go to 40
      tt=array(l)
      if (nri .le. 1) go to 50
      nn=index(l)
50    k=k+1
      if (array(k) .lt. t) go to 50
      if (k .le. l) go to 30
      if (l-i .le. j-k) go to 60
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 80
60    il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 80
70    m=m-1
      if (m .eq. 0) go to 999
      i=il(m)
      j=iu(m)
80    if (j-i .ge. 11) go to 10
      if (i .eq. ii) go to 5
      i=i-1
90    i=i+1
      if (i .eq. j) go to 70
      t=array(i+1)
      if (nri .le. 1) go to 95
      n=index(i+1)
95    if (array(i) .le. t) go to 90
      k=i
100   array(k+1)=array(k)
      if (nri .le. 1) go to 105
      index(k+1)=index(k)
105   k=k-1
      if (t .lt. array(k)) go to 100
      array(k+1)=t
      if (nri .le. 1) go to 90
      index(k+1)=n
      go to 90
999   RETURN
      END      ! SORTR