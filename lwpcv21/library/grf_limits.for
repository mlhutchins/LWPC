      SUBROUTINE GRF_LIMITS
     &          (mxholes,nrholes,nrpts,mxp,xp,yp,xl,yl)

c***********************************************************************

c  Function:
c     Adjusts the input arrays so that all points in the curve are
c     within the boundary of the graph adding extra points if required.

c  Parameters passed:
c     mxholes       [i]           dimension of NRPTS in calling program
c     nrholes       [i]           number of holes found
c     nrpts         [i,0:mxholes] counter for the number of points in
c                                 each curve
c     mxp           [i]           the dimension of XP and YP
c     xp            [r,mxp]       array to store X coordinates of curves
c     yp            [r,mxp]       array to store Y coordinates of curves
c     xl,yl         [r]           limit of the horizontal, vertical
c                                 extent of the graph

c  Parameters returned:
c     xp            [r,mxp]       array to store X coordinates of curves
c     yp            [r,mxp]       array to store Y coordinates of curves
c     nrpts         [i,0:mxholes] counter for the number of points in
c                                 each curve

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     max
c     min

c     lwpc_error

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      IMPLICIT NONE

      integer       mxholes
      integer       mxp
      integer       nrholes
      integer       nrpts
      real          xl
      real          xp
      real          yl
      real          yp

      dimension     nrpts(0:mxholes)
      dimension     xp(mxp)
      dimension     yp(mxp)

      character*200 error_msg
      logical       point1_in
      logical       point2_in
      integer       n
      integer       n1
      integer       nh
      integer       nmax
      integer       np
      real          x1
      real          x2
      real          xp1
      real          xp2
      real          y1
      real          y2
      real          yb
      real          yp1
      real          yp2

c     Loop over all line segments
      np=0
      do nh=0,nrholes

         np=np+1
         x1=xp(np)
         y1=yp(np)

         n1=np
         nmax=nrpts(nh)
         do while (np .lt. nmax)

            np=np+1
            x2=xp(np)
            y2=yp(np)

c           Test endpoints of line segments for being inside or outside
c           and for crossing the boundary.
            point1_in=x1 .ge. 0. .and. x1 .le. xl .and.
     &                y1 .ge. 0. .and. y1 .le. yl

            point2_in=x2 .ge. 0. .and. x2 .le. xl .and.
     &                y2 .ge. 0. .and. y2 .le. yl

            if (point1_in .and. point2_in) then

c              Both positions are inside
               xp1=x1
               yp1=y1
               xp2=x2
               yp2=y2
            else
     &      if (.not.point1_in .and. .not.point2_in) then

c              Both positions are outside
               xp1=MIN(xl,MAX(0.,x1))
               yp1=MIN(yl,MAX(0.,y1))
               xp2=MIN(xl,MAX(0.,x2))
               yp2=MIN(yl,MAX(0.,y2))
            else

               if (point1_in) then

c                 Position 1 is inside
                  xp1=x1
                  yp1=y1
               else

c                 Position 1 is outside
                  if (x1 .ne. x2 .and. x1 .lt. 0.) then

c                    Compute where the line crosses the left edge
                     yb=y1+(y2-y1)*(0.-x1)/(x2-x1)

                     if (yb .lt. 0.) then

c                       Crosses the bottom
c                       before it passes the left edge
                        xp1=x1+(x2-x1)*(0.-y1)/(y2-y1)
                        yp1=0.
                     else
     &               if (yb .gt. yl) then

c                       Crosses the top
c                       before it passes the left edge
                        xp1=x1+(x2-x1)*(yl-y1)/(y2-y1)
                        yp1=yl
                     else

c                       Crosses the left edge
c                       between the bottom and the top
                        xp1=0.
                        yp1=yb
                     end if
                  else
     &            if (x1 .ne. x2 .and. x1 .gt. xl) then

c                    Compute where the line crosses the right edge
                     yb=y1+(y2-y1)*(xl-x1)/(x2-x1)

                     if (yb .lt. 0.) then

c                       Crosses the bottom
c                       before it passes the right edge
                        xp1=x1+(x2-x1)*(0.-y1)/(y2-y1)
                        yp1=0.
                     else
     &               if (yb .gt. yl) then

c                       Crosses the top
c                       before it passes the right edge
                        xp1=x1+(x2-x1)*(yl-y1)/(y2-y1)
                        yp1=yl
                     else

c                       Crosses the right edge
c                       between the bottom and the top
                        xp1=xl
                        yp1=yb
                     end if
                  else
     &            if (y1 .ne. y2 .and. y1 .lt. 0.) then

c                    Crosses the bottom
c                    between the left and right edges
                     xp1=x1+(x2-x1)*(0.-y1)/(y2-y1)
                     yp1=0.
                  else
     &            if (y1 .ne. y2) then

c                    Crosses the top
c                    between the left and right edges
                     xp1=x1+(x2-x1)*(yl-y1)/(y2-y1)
                     yp1=yl
                  end if
               end if

               if (point2_in) then

c                 Position 2 is inside
                  xp2=x2
                  yp2=y2
               else

c                 Position 2 is outside
                  if (x2 .ne. x1 .and. x2 .lt. 0.) then

c                    Compute where the line crosses the left edge
                     yb=y1+(y2-y1)*(0.-x1)/(x2-x1)

                     if (yb .lt. 0.) then

c                       Crosses the bottom
c                       before it passes the left edge
                        xp2=x1+(x2-x1)*(0.-y1)/(y2-y1)
                        yp2=0.
                     else
     &               if (yb .gt. yl) then

c                       Crosses the top
c                       before it passes the left edge
                        xp2=x1+(x2-x1)*(yl-y1)/(y2-y1)
                        yp2=yl
                     else

c                       Crosses the left edge
c                       between the bottom and the top
                        xp2=0.
                        yp2=yb
                     end if
                  else
     &            if (x2 .ne. x1 .and. x2 .gt. xl) then

c                    Compute where the line crosses the right edge
                     yb=y1+(y2-y1)*(xl-x1)/(x2-x1)

                     if (yb .lt. 0.) then

c                       Crosses the bottom
c                       before it passes the right edge
                        xp2=x1+(x2-x1)*(0.-y1)/(y2-y1)
                        yp2=0.
                     else
     &               if (yb .gt. yl) then

c                       Crosses the top
c                       before it passes the right edge
                        xp2=x1+(x2-x1)*(yl-y1)/(y2-y1)
                        yp2=yl
                     else

c                       Crosses the right edge
c                       between the bottom and the top
                        xp2=xl
                        yp2=yb
                     end if
                  else
     &            if (y2 .ne. y1 .and. y2 .lt. 0.) then

c                    Crosses the bottom
c                    between the left and right edges
                     xp2=x1+(x2-x1)*(0.-y1)/(y2-y1)
                     yp2=0.
                  else
     &            if (y2 .ne. y1) then

c                    Crosses the top
c                    between the left and right edges
                     xp2=x1+(x2-x1)*(yl-y1)/(y2-y1)
                     yp2=yl
                  end if
               end if
            end if

            if (np .eq. n1+1) then

c              Store the first point
               xp(n1)=xp1
               yp(n1)=yp1
            end if

            if (.not.point1_in .and. point2_in) then

c              Insert an interpolated point;
c              first, shift the arrays to make room for it
               if (nrpts(nrholes) .eq. mxp) then

                  write(error_msg,
     &                '(''[GRF_LIMITS]: '',
     &                  ''Insufficient number of points allocated'')')

                  call LWPC_ERROR ('ERROR',error_msg)
               else

                  do n=nrpts(nrholes),np+1,-1
                     xp(n+1)=xp(n)
                     yp(n+1)=yp(n)
                  end do
               end if

c              Insert the interpolated point
               xp(np)=xp1
               yp(np)=yp1

c              Adjust the counters
               do n=nh,nrholes
                  nrpts(n)=nrpts(n)+1
               end do
               np=np+1
               nmax=nmax+1
            end if

c           Store adjusted point
            xp(np)=xp2
            yp(np)=yp2

            x1=x2
            y1=y2
         end do
      end do

      RETURN
      END      ! GRF_LIMITS