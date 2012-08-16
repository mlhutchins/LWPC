      SUBROUTINE HPGL_DRAW
     &          (xx,yy)

c***********************************************************************

c  Function:
c     Moves pen/cursor to new position while leaving a line

c  Parameters passed:
c     xx            [r] x position
c     yy            [r] y position

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

      if (graphics_pen .eq. 'up') then

c        Move to the first position and draw to the second position
         x1a=graphics_origin(1)+graphics_units(1)*graphics_position(1)
         y1a=graphics_origin(2)+graphics_units(2)*graphics_position(2)
         x2a=graphics_origin(1)+graphics_units(1)*xx
         y2a=graphics_origin(2)+graphics_units(2)*yy
         write(graphics_LogicalUnit,'(2(1x,a,2i6))')
     &        'pu',INT(x1a+.5),INT(y1a+.5),
     &        'pd',INT(x2a+.5),INT(y2a+.5)
      else

c        Draw from the first position to the second position
         x2a=graphics_origin(1)+graphics_units(1)*xx
         y2a=graphics_origin(2)+graphics_units(2)*yy
         write(graphics_LogicalUnit,'(2(1x,a,2i6))')
     &        '  ',INT(x2a+.5),INT(y2a+.5)
      end if

      RETURN
      END      ! HPGL_DRAW