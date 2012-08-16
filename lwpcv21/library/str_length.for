      FUNCTION   STR_LENGTH
     &          (string)

c***********************************************************************
c                         subroutine str_length
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Returns the location of the last right-most non-blank character of
c     a string.  Leading blanks are included in the length.

c  Parameters passed:
c     string           [s] character string

c  Parameters returned:

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     len

c  References:

c  Change History:
c     08 May 96     Added test for TAB character.
c 2/16/10 MLH removed test for TAB character 

c*******************!***************************************************

      character     string*(*)
c      character*  1 TAB
      integer       str_length
c      data          TAB/z9/


c     Check if string is defined
      if (ichar(string(1:1)) .eq. 0) then
         str_length=0
      else
c        Determine the dimension of the string
         str_length=LEN(string)
c        Count the number of characters in the string
10       if (ichar(string(str_length:str_length)) .gt. 0) then
            if (string(str_length:str_length) .ne. ' ') RETURN ! .and.
c     &          string(str_length:str_length) .ne. TAB) RETURN
         end if
         str_length=str_length-1
         if (str_length .gt. 0) go to 10
      end if

      RETURN
      END      ! STR_LENGTH