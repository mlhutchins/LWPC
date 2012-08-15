      SUBROUTINE STR_LOWER
     &          (string,first_character,last_character)

c***********************************************************************
c                         subroutine str_lower
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Changes the case of a substring to lower case

c  Parameters passed:
c     string         [s] character string
c     first_character[i] location of the first character
c     last_character [i] location of the last  character

c  Parameters returned:
c     string         [s] converted character string

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     char
c     len
c     ichar
c     max
c     min

c     str_length

c  References:

c  Change History:

c*******************!***************************************************

      character     string *(*)
      integer       str_length,first_character,last_character


      if (last_character .eq. 0) then
         jmax=STR_LENGTH(string)
      else
         jmax=MIN(last_character,LEN(string))
      end if

      if (jmax .gt. 0) then
         j=MAX(first_character,1)
         do while (j .le. jmax)
            nn=ICHAR(string(j:j))
            if (nn .ge. 65 .and. nn .le. 90) then
               string(j:j)=CHAR(nn+32)
            end if
            j=j+1
         end do
      end if

      RETURN
      END      ! STR_LOWER