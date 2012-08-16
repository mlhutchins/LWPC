      SUBROUTINE GET_TIME
     &          (ihr,imn,isc)

c***********************************************************************
c                         subroutine get_time
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     04 Dec 1996

c  Function:
c     Returns current time

c  Parameters passed:
c     none

c  Parameters returned:
c     ihr              [i] current hour
c     imn              [i] current minute
c     isc              [i] current second

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     gettim
c     itime



c  References:

c  Change History:

c*******************!***************************************************

      integer       ihr,imn,isc



c     WATCOM specific code

c      integer  *  2 ih,im,is,i100th



c      call GETTIM (ih,im,is,i100th)
c      ihr=ih

c      imn=im

c      isc=is

c     WATCOM specific code


c     SUN SOLARIS specific code

      integer       iarray(3)



      call ITIME (iarray)
      ihr=iarray(1)
      imn=iarray(2)
      isc=iarray(3)
c     SUN SOLARIS specific code


      RETURN
      END      ! GET_TIME