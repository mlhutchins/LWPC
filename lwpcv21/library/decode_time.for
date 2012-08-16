      SUBROUTINE DECODE_TIME
     &          (time,hour,minute)

c***********************************************************************
c                         subroutine decode_time
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 Apr 1990

c  Function:
c     Decodes time from a string of the form:
c        HOUR:MINUTE

c  Parameters passed:
c     time             [s] time string

c  Parameters returned:
c     hour             [i] hour
c     minute           [i] minute

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     ichar
c     index

c     str_length

c  References:

c  Change History:

c*******************!***************************************************

      character*(*) time
      character*  4 frmt(2)
      character*256 dummy
      integer       str_length,hour,minute

      data          frmt/'(i1)','(i2)'/


c     Transfer TIME to DUMMY and delete embedded blanks;
c     I3 is the number of characters in the resulting string.
      i3=0
      do i1=1,STR_LENGTH(time)
         if (time(i1:i1) .ne. ' ') then
            i3=i3+1
            dummy(i3:i3)=time(i1:i1)
         end if
      end do

c     I1 is location of punctuation between hours and minutes
      i1=INDEX(dummy,':')
      if (i1 .eq. 0) then

c        Only the HOUR
         read (dummy,frmt(i3)) hour
      else

c        The format is HOUR:MINUTE
         if (i1 .gt. 1) read (dummy,frmt(i1-1)) hour
         if (i1 .lt. i3) read (dummy(i1+1:),frmt(i3-i1)) minute
      end if

      RETURN
      END      ! DECODE_TIME