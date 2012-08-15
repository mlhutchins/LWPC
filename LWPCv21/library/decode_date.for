      SUBROUTINE DECODE_DATE
     &          (date,month,day,year)

c***********************************************************************
c                         subroutine decode_date
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 Apr 1990

c  Function:
c     Decodes date from a string of the form:
c         MONTH/DAY/YEAR

c  Parameters passed:
c     date             [s] date string

c  Parameters returned:
c     month            [s] month name
c     day              [i] day number
c     year             [i] year

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     ichar
c     index

c     str_length

c  References:

c  Change History:

c*******************!***************************************************

      character*(*) date,month
      character*  4 frmt(4)
      character*256 dummy
      integer       str_length,day,year

      data          frmt/'(i1)','(i2)','(i3)','(i4)'/


c     Transfer DATE to DUMMY and delete embedded blanks;
c     I3 is the number of characters in the resulting string.
      i3=0
      do i1=1,STR_LENGTH(date)
         if (date(i1:i1) .ne. ' ') then
            i3=i3+1
            dummy(i3:i3)=date(i1:i1)
         end if
      end do

c     I1 is the location of the first punctuation mark
      i1=INDEX(dummy,'/')
      if (i1 .eq. 0) then

c        Only the MONTH
         month=dummy

      else

c        I2 is the location of the second punctuation mark
         i2=i1+INDEX(dummy(i1+1:),'/')
         if (i2 .eq. i1) then

c           Only MONTH/DAY
            if (i1 .gt. 1) month=dummy(:i1-1)
            if (i1 .lt. i3) read (dummy(i1+1:),frmt(i3-i1)) day

         else

c           The format is MONTH/DAY/YEAR
            if (i1 .gt. 1) month=dummy(:i1-1)
            if (i1 .lt. i2-1) read (dummy(i1+1:),frmt(i2-i1-1)) day
            if (i2 .lt. i3) read (dummy(i2+1:),frmt(i3-i2)) year
         end if
      end if

      RETURN
      END      ! DECODE_DATE