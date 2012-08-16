      SUBROUTINE HPGL_FILL_RECT
     &          (x1a,y1a,x2a,y2a,index)

c***********************************************************************

c  Function:
c     Draws a rectangle with lower left corner at (x1a,y1a) and
c     upper right corner at (x2a,y2a); fills it with the specified
c     pattern.

c  Parameters passed:
c     x1a           [r] value of absolute x position: p1
c     y1a           [r] value of absolute y position: p1
c     x2a           [r] value of absolute x position: p2
c     y2a           [r] value of absolute y position: p2
c     index         [i] index of fill pattern:
c                   Index  Name           Pattern
c                      0   100%           100  (solid)
c                      1    99%            99
c                      2    80%            80
c                      3    55%            55
c                      4    35%            35
c                      5    20%            20
c                      6    10%            10
c                      7     2%             2
c                      8     0%             0  (blank)
c                      9   vertical       lines
c                     10   horizontal     lines
c                     11   left diagonal  lines
c                     12   right diagonal lines

c                          Aliases
c                      0   solid
c                      8   blank

c  Parameters returned:

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     write

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      integer       type(13),shading(13)

      data          type/1,8*10,4*21/
      data          shading/100,99,80,55,35,20,10,2,0,2,1,3,4/

      if (index .ne. 8) then

         if (index .ne. graphics_FillIndx) then

c           Define fill pattern
            if (index .eq. 0) then

               write(graphics_LogicalUnit,'('' ft1;'')')
            else

               write(graphics_LogicalUnit,'(1x,a,i2,i4,'';'')')
     &              'ft',type(index+1),shading(index+1)
            end if
         end if

c        Move to the first position and draw a rectangle to the second
         write(graphics_LogicalUnit,'(1x,2(a,2i6,'';''))')
     &        'pu',INT(x1a+.5),INT(y1a+.5),
     &        'ra',INT(x2a+.5),INT(y2a+.5)
      end if

      RETURN
      END      ! HPGL_FILL_RECT