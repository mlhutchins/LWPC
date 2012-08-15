      SUBROUTINE GET_DATE
     &          (iyear,imonth,iday)

c***********************************************************************
c                         subroutine get_date
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     04 Dec 1996

c  Function:
c     Returns current date

c  Parameters passed:
c     none

c  Parameters returned:
c     iyear            [i] current year

c     imonth           [i] current month
c     iday             [i] current day of the month


c  Common blocks referenced:

c  Functions and subroutines referenced:
c     getdat
c     itime

c  References:

c  Change History:

c*******************!***************************************************

      integer       iyear,imonth,iday



c     WATCOM specific code

c      integer  *  2 iy,im,id



c      call GETDAT (iy,im,id)
c      iyear =iy

c      imonth=im

c      iday  =id

c     WATCOM specific code


c     SUN SOLARIS specific code

      integer       iarray(3)



      call IDATE (iarray)
      iyear =iarray(3)

      imonth=iarray(2)
      iday  =iarray(1)
 
c      SUN SOLARIS specific code


      RETURN
      END      ! GET_DATE