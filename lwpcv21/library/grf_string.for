c$pragma aux Sys_string "*_" parm (value)

      SUBROUTINE GRF_STRING
     &          (xx,yy,height,string,angle,justify,retX)

c***********************************************************************

c  Function:
c     Plots a character string

c  Parameters passed:
c     xx            [r] x position of first character
c     yy            [r] y position of first character
c     height        [r] height of the characters in inches
c     string        [s] character string to be plotted
c     angle         [r] angle of the string in degrees relative to
c                       the horizontal
c     justify       [s] 2 character justification string
c                                       string  action
c                                       CC                      center about X; center about Y
c                                       CT                      center about X; top of chars at Y
c                                       CB                      center about X; bottom of chars at Y
c                                       RC                      right justify;          center about Y
c                                       LC                      left justify;           center about Y
c                                       RT                      right justify;          top of chars at Y
c                                       RB                      right justify;          bottom of chars at Y
c                                       LT                      left justify;           top of chars at Y
c                                       LB                      left justify;           bottom of chars at Y

c  Parameters returned:
c      retX         [r] length of output string (in inches)

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     ichar

c     str_length

c     hpgl_string
c     sys_string

c  Entry:

c  References:

c  Change History:
c 2/16/10 MLH changed null/z0/ to null and added data null/'0'/

c*******************!***************************************************

      include      'graphics.cmn'

      character* 2  justify
      character*(*) string

      character*  1 null/' '/
      character*121 xstring
      character*200 error_msg
      integer       str_length
      real          height
      real          retX
c     Initialize retX
      retX=0.

c     Count the number of characters in the string
      xstring=string
      nr_characters=STR_LENGTH(xstring)
      if (nr_characters .eq. 0) RETURN

c     Check if xstring is defined
      if (ICHAR(xstring(1:1)) .eq. 0) RETURN

c     Replace undefined characters with blanks
      do n=1,nr_characters
         if (ICHAR(xstring(n:n)) .eq. 0) xstring(n:n)=' '
      end do

c     Re-count the number of characters in the string
      nr_characters=STR_LENGTH(xstring)
      if (nr_characters .eq. 0) RETURN

      if (nr_characters .gt. 245) then
         write(error_msg,
     &      '(''[GRF_STRING]: '',
     &        ''Input string is too long ('',i3,'' characters)'')')
     &          nr_characters
         call GRF_DONE
         call LWPC_ERROR ('ERROR',error_msg)
      end if

c     Move to the beginning of the string
      call GRF_MOVE (xx,yy)

      if (graphics_device(1:3) .eq. 'sys') then

c        Make sure the string ends with a null
         nl=STR_LENGTH(xstring)+1
         xstring(nl:nl)=null

         retX=SYS_STRING
     &       (xx,yy,height,xstring,angle,justify)
      else

         call HPGL_STRING
     &       (xx,yy,height,xstring,angle,justify,retX)
      end if

c     Update position array
      graphics_position(1)=xx
      graphics_position(2)=yy

c     Pen status
      graphics_pen='up'

      RETURN
      END      ! GRF_STRING