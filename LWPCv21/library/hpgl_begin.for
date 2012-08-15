      SUBROUTINE HPGL_BEGIN

c***********************************************************************

c  Function:
c     Initialize HPGL

c  Parameters passed:

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

c     Set plotter units per inch
      graphics_units(1)=1016.
      graphics_units(2)=1016.

c     Set portrait or landscape
      if (graphics_landscape) then

c        Set P1 and P2
         graphics_extent(1)=10000.
         graphics_extent(2)=7200.
      else

c        Rotate the plot to get portrait
         write(graphics_LogicalUnit,'(1x,a)') 'ro90;'

c        Set P1 and P2
         graphics_extent(1)=7200.
         graphics_extent(2)=10000.
      end if

c     Set pen width to 0.3 mm
      write(graphics_LogicalUnit,'('' pw0.3;'')')

      RETURN
      END      ! HPGL_BEGIN