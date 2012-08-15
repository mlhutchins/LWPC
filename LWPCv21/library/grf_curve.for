      SUBROUTINE GRF_CURVE
     &          (xarray,yarray,nrpts,xmin,ymin,xscale,yscale,
     &           line,dots_per_inch)

c***********************************************************************

c  Function:
c     Draws a curve representing the input x,y pairs

c  Parameters passed:
c     xarray        [r] array of X values; user units
c     yarray        [r] array of Y values; user units
c     nrpts         [i] number of points to be plotted
c     xmin          [r] origin of the X values; user units
c     ymin          [r] origin of the Y values; user units
c     xscale        [r] scale along the X axis; user units per inch
c     yscale        [r] scale along the Y axis; user units per inch
c     line          [i] line type to be used while drawing the curve
c     dots_per_inch [i] number of dost per inch; used to define the
c                       length of segments of all line types

c     NOTE: line=1: solid
c                2: long dash
c                3: medium dash
c                4: short dash
c                5: dotted
c                6: short + long dash
c                7: short + short + long dash

c  Parameters returned:

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     mod
c     sqrt

c     grf_draw
c     grf_move

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      integer       dots_per_inch

      dimension     xarray(1),yarray(1)

      logical       up(8),move
      integer       jo(7),jc(7)

      data          up/.false.,.false.,.false.,.true.,
     &                 .false.,.true. ,.false.,.true./,
     &              jo/1,1,2,3,4,1,1/,
     &              jc/1,4,3,2,1,6,8/

c     If insufficient number of points, don't do anything
      if (nrpts .le. 1) RETURN

      if (graphics_device(1:3) .eq. 'sys') then

c        Set up screen output

c        Set up parameters for line type
         resolution=1./dots_per_inch

         kk=MOD(line,7)
         if (kk .le. 0) kk=kk+7
         j_origin=jo(kk)
         j_cycle =jc(kk)
         move=up(j_origin)
         j=0

c        Two different integrals are formed:
c        The first one is the total length of the curve being drawn <rho>;
c        the second is the length of the curve with respect to a segment of
c        the particular line type being used <rho2>.
         rho1=0.
         rho2=resolution
         dr=0.

c        Select the first point
         px1=(xarray(1)-xmin)/xscale
         py1=(yarray(1)-ymin)/yscale

c        Go to first position with pen up
         call GRF_MOVE (px1,py1)

         if (kk .eq. 5) then

c           The curve is dotted; generate a very short dash along the line
c           segment between the first and second points
            px2=(xarray(2)-xmin)/xscale
            py2=(yarray(2)-ymin)/yscale
            delx=px2-px1
            dely=py2-py1
            rho=SQRT(delx**2+dely**2)
            if (rho .gt. 0.)
     &         call GRF_DRAW (px1+.01*delx/rho,py1+.01*dely/rho)
         end if

c        Start curve
         do i=2,nrpts

c           Select next point
            px2=(xarray(i)-xmin)/xscale
            py2=(yarray(i)-ymin)/yscale

            if (kk .eq. 1) then

c              Solid line:
c              No need to integrate the curve
               call GRF_DRAW (px2,py2)

            else

c              Multi-segmented line:
c              Integrate the curve to get even-length line segments

c              First, calculate the length of the segment
               delx=px2-px1
               dely=py2-py1
               rho=SQRT(delx**2+dely**2)

c              Increment integration variable
               rho1=rho1+rho
               if (rho2 .le. rho1) then

c                 The data point is beyond the end of the line segment.
                  delx=delx*resolution/rho
                  dely=dely*resolution/rho

                  if (dr .gt. 0.) then

c                    Complete current line segment
                     dx=delx*dr/resolution
                     dy=dely*dr/resolution
                     px1=px1+dx
                     py1=py1+dy

                     if (kk .eq. 5) then
c                       Dotted line
                        call GRF_MOVE (px1,py1)
                        call GRF_DRAW (px1+.1*delx,py1+.1*dely)
                     else
     &               if (move) then
                        call GRF_MOVE (px1,py1)
                     else
                        call GRF_DRAW (px1,py1)
                     end if

c                    Increment interval counter
                     j=j+1
                     move=up(j_origin+MOD(j,j_cycle))
                     rho2=rho2+resolution
                  end if

                  do while (rho2 .le. rho1)
                     px1=px1+delx
                     py1=py1+dely

                     if (kk .eq. 5) then
c                       Dotted line
                        call GRF_MOVE (px1,py1)
                        call GRF_DRAW (px1+.1*delx,py1+.1*dely)
                     else
     &               if (move) then
                        call GRF_MOVE (px1,py1)
                     else
                        call GRF_DRAW (px1,py1)
                     end if

c                    Increment interval counter
                     j=j+1
                     move=up(j_origin+MOD(j,j_cycle))
                     rho2=rho2+resolution
                  end do
               end if

c              Complete segment
               if (move) then
                  call GRF_MOVE (px2,py2)
               else
                  call GRF_DRAW (px2,py2)
               end if
            end if
            dr=rho2-rho1
            px1=px2
            py1=py2
         end do
      else

c        Get the pattern length as a percentage of the diagonal
c        distance of the plot page
         pl=400./dots_per_inch/
     &      SQRT((graphics_extent(1)/graphics_units(1))**2+
     &           (graphics_extent(2)/graphics_units(2))**2)

c        Set the line type
         kk=MOD(line,7)
         if (kk .le. 0) kk=kk+7
         if (kk .eq. 1) then
            write(graphics_LogicalUnit,'('' lt;'')')
         else
            write(graphics_LogicalUnit,'('' lt'',i1,f6.2'';'')')
     &            kk-1,pl
         end if

c        Select the first point
         px1=(xarray(1)-xmin)/xscale
         py1=(yarray(1)-ymin)/yscale

c        Go to first position with pen up
         call GRF_MOVE (px1,py1)

c        Start curve
         do i=2,nrpts

c           Select next point
            px2=(xarray(i)-xmin)/xscale
            py2=(yarray(i)-ymin)/yscale

            call GRF_DRAW (px2,py2)
         end do

c        Reset line type to solid line
         write(graphics_LogicalUnit,'('' lt;'')')
      end if

      RETURN
      END      ! GRF_CURVE