      SUBROUTINE GRF_ORIGIN
     &          (xx,yy)

c***********************************************************************

c  Function:
c     Moves pen/cursor to new position and establishes a new origin

c  Parameters passed:
c     xx            [r] value of x position in inches
c     yy            [r] value of y position in inches

c  Parameters returned:
c     origin        [r] coordinates of current  logical origin
c                       relative to physical origin

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      call GRF_MOVE (xx,yy)

      graphics_origin(1)=graphics_origin(1)+graphics_units(1)*xx
      graphics_origin(2)=graphics_origin(2)+graphics_units(2)*yy

c     Set pen position back to 0,0
      call GRF_MOVE (0.,0.)

      RETURN
      END      ! GRF_ORIGIN