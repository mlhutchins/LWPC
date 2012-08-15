      SUBROUTINE HPGL_COLOR
     &          (color_index)

c***********************************************************************

c  Function:
c     Sets color on HPGL devices

c  Parameters passed:
c     color_index   [i] index of the color to be set

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     write

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      integer       str_length,
     &              color_index

c     Select pen
      npen=color_index

      if (graphics_color(1) .eq. 'rgb') then

c        HPGL device; change the RGB value of selected pen
         write(graphics_LogicalUnit,'('' pc'',i3,'','',a,'';'')')
     &         npen,
     &         graphics_color(2)(:STR_LENGTH(graphics_color(2)))
      end if

      write(graphics_LogicalUnit,'('' sp'',i3,'';'')') npen

c     Ensure that HPGL_DRAW inserts a Pen Down command
c     before drawing the next line segemnt.
      graphics_pen='up'

      RETURN
      END      ! HPGL_COLOR