      SUBROUTINE HPGL_POLY_FILL
     &          (nrpts,xarray,yarray,xmin,ymin,xscale,yscale,index)

c***********************************************************************

c  Function:
c     Draws a polygon and fills it with the specified fill type using
c     HPGL/2 conventions.

c  Parameters passed:
c     nrpts         [i] number of points in arrays defining the polygon
c     xarray        [r] x coordinates of the polygon [user units]
c     yarray        [r] y coordinates of the polygon [user units]
c     xmin          [r] graphical origin [user units]
c     ymin          [r] graphical origin [user units]
c     xscale        [r] x scale [user units per inch]
c     yscale        [r] y scale [user units per inch]
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
c     abs
c     write

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

c     Argument list
      real          xarray(1),yarray(1),xmin,ymin,xscale,yscale

c     Local
      character* 40 frmt
      integer       type(13),shading(13),
     &              ix(8),iy(8)

      data          type/1,8*10,4*21/
      data          shading/100,99,80,55,35,20,10,2,0,2,1,3,4/

      if (index .ne. 8) then

         if (index .ne. graphics_fillindx) then

c           Define fill pattern
            if (index .eq. 0) then
               write(graphics_LogicalUnit,'('' ft1;'')')

            else
               write(graphics_LogicalUnit,'(1x,a,i2,i4,'';'')')
     &              'ft',type(index+1),shading(index+1)
            end if
         end if

c        Move to the first position and enter polygon mode
         xx=(xarray(1)-xmin)/xscale
         yy=(yarray(1)-ymin)/yscale

         ix1=graphics_origin(1)
     &      +graphics_units (1)*xx+.5
         iy1=graphics_origin(2)
     &      +graphics_units (2)*yy+.5

         write(graphics_LogicalUnit,'('' pu'',2i6,'';pm0;'')')
     &         ix1,iy1

c        Define the rest of the positions
         nptmax=((nrpts-1)/8)*8+1
         do npt=2,nptmax,8

            nxy=0
            nmin=npt
            nmax=npt+7
            do n=nmin,nmax
               xx=(xarray(n)-xmin)/xscale
               yy=(yarray(n)-ymin)/yscale

               nxy=nxy+1
               ix(nxy)=graphics_origin(1)
     &                +graphics_units (1)*xx+.5
               iy(nxy)=graphics_origin(2)
     &                +graphics_units (2)*yy+.5
            end do
            write(frmt,
     &          '(''('''' pd'''','',i2,''i6,'''';'''')'')')
     &            nxy*2
            write(graphics_LogicalUnit,frmt)
     &           (ix(i),iy(i),i=1,nxy)
         end do

         if (nptmax .eq. nrpts) then

c           Make sure the polygon is closed
            if (xarray(1) .ne. xarray(nrpts) .or.
     &          yarray(1) .ne. yarray(nrpts)) then

c              Close the polygon;
c              exit polygon mode and fill the polygon
               write(graphics_LogicalUnit,'('' pd'',2i6,'';pm2;fp;'')')
     &               ix1,iy1
            else

c              Exit polygon mode and fill the polygon
               write(graphics_LogicalUnit,'('' pm2;fp;'')')
            end if
         else

c           Write the last line of points
            nxy=0
            nmin=nptmax+1
            nmax=nrpts
            do n=nmin,nmax
               xx=(xarray(n)-xmin)/xscale
               yy=(yarray(n)-ymin)/yscale

               nxy=nxy+1
               ix(nxy)=graphics_origin(1)
     &                +graphics_units (1)*xx+.5
               iy(nxy)=graphics_origin(2)
     &                +graphics_units (2)*yy+.5
            end do

c           Make sure the polygon is closed
            if (xarray(1) .ne. xarray(nrpts) .or.
     &          yarray(1) .ne. yarray(nrpts)) then

c              Return to the first position
               nxy=nxy+1
               ix(nxy)=ix1
               iy(nxy)=iy1
            end if
            write(frmt,
     &          '(''('''' pd'''','',i2,''i6,'''';pm2;fp;'''')'')')
     &            nxy*2
            write(graphics_LogicalUnit,frmt)
     &           (ix(i),iy(i),i=1,nxy)
         end if
      end if

      RETURN
      END      ! HPGL_POLY_FILL