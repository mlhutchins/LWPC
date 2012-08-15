      SUBROUTINE STR_COUNT_LIST
     &          (string,first_character,last_character,nl)

c***********************************************************************
c                         subroutine str_count_list
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Returns the number of values which are encoded in a character
c     string; the values are separated by commas and spaces

c  Parameters passed:
c     string           [s] character string
c     first_character  [s] character at which to start the search
c     last_character   [s] character at which to end   the search

c  Parameters returned:
c     nl               [i] number of values in the list

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     str_length

c  References:

c  Change History:
c     08 May 96     Added test for TAB character.
c 2/16/10 MLH removed test for TAB character 

c*******************!***************************************************

      character*(*) string
c      character*  9 TAB
      integer       str_length,first_character,last_character
c      parameter (TAB='09')
      
      if (first_character .eq. 0) then
         jf=1
      else
         jf=first_character
      end if
      if (last_character .eq. 0) then
         jl=STR_LENGTH(string)
      else
         jl=last_character
      end if

      nl=0
      j=jf
      do while (j .le. jl)
c        Find the starting location of the value
         do while (string(j:j) .eq. ' ')!.or.
c     &             string(j:j) .eq. TAB)
            if (j .eq. jl) RETURN
            j=j+1
         end do
         nl=nl+1
c        Find the ending location of the value
         do while (string(j:j) .ne. ' ' .and.
c     &             string(j:j) .ne. TAB .and.
     &             string(j:j) .ne. ',')
            if (j .eq. jl) RETURN
            j=j+1
         end do
         if (string(j:j) .eq. ',') j=j+1
      end do

      RETURN
      END      ! STR_COUNT_LIST