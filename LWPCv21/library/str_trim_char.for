      SUBROUTINE STR_TRIM_CHAR
     &          (string, trim_type, nrchar, trim_option)

c***********************************************************************
c                         subroutine str_trim_char
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     11 Feb 1993

c  Function:
c     Removes requested character type from a string

c  Parameters passed:
c     trim_type      [c] character - character to remove
c     string         [s] character string
c     nrchar         [i]
c     trim_option    [s] 'L' to remove leading trim_type chars only
c                        'A' to remove all instances of trim_type char

c  Parameters returned:
c     string         [s] character string with leading blanks removed
c     nrchar         [i] length of modified string

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     str_length
c     str_upper

c  References:

c  Change History:

c*******************!***************************************************

      character* 1  trim_type
      character*(*) string,trim_option
      integer       str_length,nrchar,crnt_pos,next_pos


c     First, remove all leading trim_type characters from string
      nrchar=STR_LENGTH(string)
      do while (string(1:1) .eq. trim_type)
         string(1:nrchar)=string(2:nrchar)//' '
         nrchar=nrchar-1
      end do

c     If trim_option is ALL remove all further instances of trim_type
      call STR_UPPER (trim_option,1,1)
      if (trim_option(1:1) .eq. 'A') then

         crnt_pos=1
         do while (crnt_pos .le. nrchar)
            next_pos=crnt_pos+1
            if (string(crnt_pos:crnt_pos) .eq. trim_type) then
               next_last=nrchar-1
               string(crnt_pos:nrchar)=string(next_pos:nrchar)//' '
               nrchar=next_last
            else
               crnt_pos=next_pos
            end if
         end do
      end if

      RETURN
      END        ! STR_TRIM_CHAR