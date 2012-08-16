      FUNCTION LEAP_YEAR (year)

c***********************************************************************
c                         function   leap_year
c***********************************************************************

c  Program Source:   Naval Ocean Systems Center - Code 542

c  Date:
c     22 Mar 1997

c  Function:
c     Determines if given year is a leap year.

c  Parameters passed:
c        year       [i  ] year

c  Parameters returned:
c        leap_year  [l  ] TRUE if a leap year; FALSE otherwise

c  Common blocks referenced:

c  Functions and subroutines referenced:

c  References:

c  Change History:

c*******************!***************************************************

      logical       leap_year
      integer       year

c     Determine if this is a leap_year year
      if (MOD(year,4) .eq. 0) then
         if (MOD(year,100) .eq. 0) then
            if (MOD(year,400) .eq. 0) then
               leap_year=.true.
            else
               leap_year=.false.
            end if
         else
            leap_year=.true.
         end if
      else
         leap_year=.false.
      end if

      RETURN
      END    ! LEAP_YEAR