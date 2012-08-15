      FUNCTION HRS_TO_HHMM
     &        (hours)

c***********************************************************************
c                         function hrs_to_hhmm
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 882

c  Date:
c     11 Apr 1999

c  Function:
c     Converts time in hours to HHMM format

c  Parameters passed:
c     hours            [r] time in hours

c  Parameters returned:
c     hhmm             [r] time in HHMM format

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     int

c  References:

c  Change History:

c*******************!***************************************************

c     Convert from hours to HHMM
      hrs_to_hhmm=INT(hours)*40.+hours*60.+.5

      RETURN
      END      ! HRS_TO_HHMM