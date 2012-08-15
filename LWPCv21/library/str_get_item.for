      SUBROUTINE STR_GET_ITEM
     &          (item_number,string,item,first_character,last_character)

c***********************************************************************
c                         subroutine str_get_item
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Returns the specified item encoded in a character string;
c     the values are separated by commas and spaces

c  Parameters passed:
c     item_number      [i] number of the item to be returned
c     string           [s] data string

c  Parameters returned:
c     item             [s] string containing the item
c     first_character  [i] character at which the item starts
c     last_character   [i] character at which the item ends

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     len
c     min

c     str_length

c  References:

c  Change History:
c     08 May 96     Added test for TAB character.
c 2/16/10 MLH removed test for TAB character 


c*******************!***************************************************

      character*(*) string,item
c      character*  1 TAB
      character*256 temporary
      integer       str_length,first_character,last_character
c      data          TAB/z9/


c     Get dimension of the strings
      len_item     =LEN(item)
      len_string   =LEN(string)
      length_string=STR_LENGTH(string)

      if (length_string .eq. 0 .or. item_number .eq. 0) RETURN

      jf=1
      item_count=0
      do while (jf .le. length_string .and.
     &          item_count .lt. item_number)
c        Find the starting location of the value
         do while (string(jf:jf) .eq. ' ')! .or.
c     &             string(jf:jf) .eq. TAB)
            jf=jf+1
         end do
c        Check for null items
         do while (string(jf:jf) .eq. ',')
            item_count=item_count+1
            if (item_count .eq. item_number) then
               first_character=jf
               last_character=jf
               RETURN
            else
     &      if (jf .eq. length_string) then
               first_character=jf
               last_character=jf
               RETURN
            end if
            jf=jf+1
         end do
c        Find the last character of the item
         jl=jf+1
         do while (jl .le. length_string  .and.
     &             string(jl:jl) .ne. ' ' .and.
c     &             string(jl:jl) .ne. TAB .and.
     &             string(jl:jl) .ne. ',')
            jl=jl+1
         end do
         item_count=item_count+1
         temporary=string(jf:MIN(jl,jf+len_item)-1)
         first_character=jf
         last_character=jl-1
         jf=jl+1
      end do
      if (item_count .eq. item_number) item=temporary

      RETURN
      END      ! STR_GET_ITEM