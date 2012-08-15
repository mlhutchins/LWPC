      SUBROUTINE GRF_CNTR_EDGE
     &          (v,mxx,mxy,nrx,nry,vc,
     &           nc,xc,yc,nx,ny,edge,
     &           ccw,new_curve)

c***********************************************************************

c  Function:
c     This routine searches the boundary of the contour area in the
c     specified direction to find the next crossing of the specified
c     contour level. If NEW_CURVE is TRUE, then we are looking for the
c     beginning of a new curve (after which we will trace the contour
c     into the interior of the data array); otherwise, we are looking
c     for the continuation of a curve which we are tracing (this occurs
c     when a curve runs into the edge; we then trace back along the edge
c     to find either the beginning of the current curve or the end of
c     another curve leading into the interior).

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     v            [r,mxx,mxy]   array containing values for contours
c     mxx          [i]           X dimension of V in calling program
c     mxy          [i]           Y dimension of V in calling program
c     nrx          [i]           number of elements of V along X
c     nry          [i]           number of elements of V along Y
c     vc           [r,2]         definition of the contour band
c     nc           [i]           index of the contour level to search;
c                                =1: VC(1); =2: VC(2)
c     ccw          [l]           flag showing the direction of search;
c                                =T: counterclockwise; =F: clockwise
c     new_curve    [l]           flag showing the type of search
c                                =T: searching for the start of a new
c                                curve; =F: searching for the end of the
c                                current curve

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}
c     xc           [r]           X coordinate of the crossing
c     yc           [r]           Y coordinate of the crossing
c     nx           [i]           X index      of the crossing
c     ny           [i]           Y index      of the crossing
c     edge         [i]           edge         of the crossing

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:
c     grf_cntr_flag
c     grf_cntr_col
c     grf_cntr_row

c  References:

c  Change History:

c     27 May 98     Corrections for grid points having the same values
c                   as the contour level.

c*******************!***************************************************

      IMPLICIT      NONE

      include      'grf_cntr.cmn'
      include      'lwpc_lun.cmn'

      character*200 error_msg

      logical       ccw
      logical       grf_cntr_flag      ! Function
      logical       new_curve
      logical       bit
      logical       set
      logical       tst

      integer       edge
      integer       inc
      integer       m
      integer       mc
      integer       mx1
      integer       mx2
      integer       mxx
      integer       mxy
      integer       my1
      integer       my2
      integer       nc
      integer       ncx(2)
      integer       ne
      integer       nrc
      integer       nrx
      integer       nry
      integer       nx
      integer       nx0
      integer       nx1
      integer       nx2
      integer       ny
      integer       ny0
      integer       ny1
      integer       ny2
      integer       pass

      real          v(mxx,mxy)
      real          vc(2)
      real          vcx
      real          xc
      real          xcx(2)
      real          yc
      real          ycx(2)

      data          set/.true./
      data          tst/.false./

c     If NEW_CURVE is TRUE, then we are searching for the beginning of
c     a contour line so we exit when we find the first unused cell;
c     otherwise, we are trying to close a contour line at the edge so
c     we exit when we find the first cell which contains either VC1 or
c     VC2: 1) if it has been used, we may have reached the beginning of
c             the contour line;
c          2) if it hasn't been used, then the contour line is re-
c             entering the interior of the array.
c     In either case, we just have to make sure that the curve will
c     enclose values between VC1 and VC2.

c     Store the starting point
      nx0=nx
      ny0=ny
      mx1=nx
      my1=ny

c     The corners and edges of each cell are numbered from 1 to 4 in a
c     counter-clockwise direction with the lower left corner and the
c     bottom edge being 1.

c     We count the passes around the edge so we know when to exit if
c     we don't find any acceptable crossings
      pass=0

cxxcDEBUG
cxx      m=0
cxx      write(lwpcLOG_lun,'(/a10,3i5,2l5,2f10.4)')
cxx     &     'edge',m,nx,ny,ccw,new_curve,vc

c     Check if the boundary should be the contour for the lower level
      if (new_curve .and.
     &    nx .eq. 1 .and. ny .eq. 1 .and.
     &    vc(1) .ge. v(1,1) .and. v(1,1) .ge. vc(2)) then

c        Check bottom edge for any crossings
         call GRF_CNTR_ROW
     &       (v,mxx,mxy,nrx,vc,1,nrx,
     &        nx,ny,nrc,ncx,xcx,ycx)

         if (nrc .eq. 0) then

c           Check right edge for any crossings
            nx=nrx
            call GRF_CNTR_COL
     &          (v,mxx,mxy,nry,vc,1,nry,
     &           nx,ny,nrc,ncx,xcx,ycx)

            if (nrc .eq. 0) then

c              Check top edge for any crossings
               ny=nry
               call GRF_CNTR_ROW
     &             (v,mxx,mxy,nrx,vc,nrx,1,
     &              nx,ny,nrc,ncx,xcx,ycx)

               if (nrc .eq. 0) then

c                 Check right edge for any crossings
                  nx=1
                  call GRF_CNTR_COL
     &                (v,mxx,mxy,nry,vc,nry,1,
     &                 nx,ny,nrc,ncx,xcx,ycx)

                  if (nrc .eq. 0) then

c                    There were no crossings and the lower left corner
c                    is bounded by the contour levels so the boundary
c                    of the array must be treated as the exterior curve.
                     nc=1
                     edge=0
                     do m=1,nry
                        nrcntrp=nrcntrp+1
                        cntrpx (nrcntrp)=1
                        cntrpy (nrcntrp)=m
                     end do
                     do m=2,nrx
                        nrcntrp=nrcntrp+1
                        cntrpx (nrcntrp)=m
                        cntrpy (nrcntrp)=nry
                     end do
                     do m=nry-1,1,-1
                        nrcntrp=nrcntrp+1
                        cntrpx (nrcntrp)=nrx
                        cntrpy (nrcntrp)=m
                     end do
                     do m=nrx-1,1,-1
                        nrcntrp=nrcntrp+1
                        cntrpx (nrcntrp)=m
                        cntrpy (nrcntrp)=1
                     end do
                     RETURN
                  end if
               end if
            end if
         end if

c        There was at least one crossing;
c        reset NX, NY
         nx=1
         ny=1
      end if

      go to (11,13,15,17),edge

c     Search bottom edge for crossing of VC
11    if (ccw) then

c        Search left to right
         nx1=nx
         nx2=nrx
      else

c        Search right to left
         nx1=nx
         nx2=1
      end if
      ne=1

      call GRF_CNTR_ROW
     &    (v,mxx,mxy,nrx,vc,nx1,nx2,
     &     nx,ny,nrc,ncx,xcx,ycx)

cxxcDEBUG
cxx      if (nrc .eq. 0) then
cxx         write(lwpcLOG_lun,'(a10,3i5,2i5,2f10.4)')
cxx     &        'edge',edge,nx,ny
cxx      else
cxx         write(lwpcLOG_lun,'(a10,3i5,2l5,3f10.4)')
cxx     &        'edge',edge,nx,ny,
cxx     &               GRF_CNTR_FLAG(cells(1,1),nrx,tst,nx,ny,ne),
cxx     &               GRF_CNTR_FLAG(cells(1,2),nrx,tst,nx,ny,ne),
cxx     &               v(nx-1,ny),v(nx,ny),v(nx+1,ny)
cxx      end if

      if (nrc .gt. 0) then

c        Check all crossings;
c        if we're looking for the start of a new curve, then we need
c        a cell that hasn't already been used
c        furthermore, we are looking for a cell which leads into the
c        interior of the array for which the filled area will be on
c        the right as we follow it into the interior of the array
         do m=1,nrc
            mc=m
            nc=ncx(m)

            if (new_curve) then

               if (.not.GRF_CNTR_FLAG
     &                 (cells(1,nc),nrx,tst,nx,ny,ne)) then

                  if (nc .eq. 1) then
                     if (v(nx  ,ny) .ge. vc(1) .and.
     &                   v(nx+1,ny) .lt. vc(1)) go to 29
                     if (v(nx  ,ny) .gt. vc(1) .and.
     &                   v(nx+1,ny) .le. vc(1)) go to 29
                  else
                     if (v(nx  ,ny) .le. vc(2) .and.
     &                   v(nx+1,ny) .gt. vc(2)) go to 29
                     if (v(nx  ,ny) .lt. vc(2) .and.
     &                   v(nx+1,ny) .ge. vc(2)) go to 29
                  end if
               end if
            else
               if (nc .eq. 1) then
                  if (v(nx  ,ny) .ge. vc(1) .and.
     &                v(nx+1,ny) .lt. vc(1)) go to 21
                  if (v(nx  ,ny) .gt. vc(1) .and.
     &                v(nx+1,ny) .le. vc(1)) go to 21
               else
                  if (v(nx  ,ny) .le. vc(2) .and.
     &                v(nx+1,ny) .gt. vc(2)) go to 21
                  if (v(nx  ,ny) .lt. vc(2) .and.
     &                v(nx+1,ny) .ge. vc(2)) go to 21
               end if
            end if
         end do

c        No crossings were acceptable; continue search
         if (ccw) then
            nx=nx+1
         else
            nx=nx-1
         end if
         if (nx .gt. 0 .and. nx .lt. nrx) go to 11
      end if
12    pass=pass+1
      if (pass .eq. 5) go to 91

c     Continue to the next edge
      if (ccw) then
         mx2=nrx
         inc=1
         nx=nrx
         ny=1
      else
         mx2=1
         inc=-1
         nx=1
         ny=1
      end if
      if (.not.new_curve) then

c        Store coordinates along this edge
         if (inc .gt. 0) then
            if (mx1 .eq. nx0) mx1=nx0+inc
         else
            if (mx1 .eq.  xc) mx1=nx0+inc
         end if
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(a8,i2,4i5)') 'fill',edge,mx1,ny,mx2,ny
         do m=mx1,mx2,inc
            if (nrcntrp .ge. mxcntrp) go to 99
            nrcntrp=nrcntrp+1
            cntrpx (nrcntrp)=m
            cntrpy (nrcntrp)=ny
cxx            write(lwpcLOG_lun,'(30x,i5,2f10.4)')
cxx     &            nrcntrp,cntrpx(nrcntrp),cntrpy(nrcntrp)
         end do
         mx1=nx
         my1=ny
      end if
      if (ccw) then
         edge=2
      else
         edge=4
      end if
      if (.not.ccw) go to 17

c     Search right edge for crossing of VC
13    if (ccw) then

c        Search bottom to top
         ny1=ny
         ny2=nry
      else

c        Search top to bottom
         ny1=ny
         ny2=1
      end if
      ne=4

      call GRF_CNTR_COL
     &    (v,mxx,mxy,nry,vc,ny1,ny2,
     &     nx,ny,nrc,ncx,xcx,ycx)

cxxcDEBUG
cxx      if (nrc .eq. 0) then
cxx         write(lwpcLOG_lun,'(a10,3i5,2i5,2f10.4)')
cxx     &        'edge',edge,nx,ny
cxx      else
cxx         write(lwpcLOG_lun,'(a10,3i5,2l5,3f10.4)')
cxx     &        'edge',edge,nx,ny,
cxx     &               GRF_CNTR_FLAG(cells(1,1),nrx,tst,nx,ny,ne),
cxx     &               GRF_CNTR_FLAG(cells(1,2),nrx,tst,nx,ny,ne),
cxx     &               v(nx,ny-1),v(nx,ny),v(nx,ny+1)
cxx      end if

      if (nrc .gt. 0) then

c        Check all crossings;
c        if we're looking for the start of a new curve, then we need
c        a cell that hasn't already been used
c        furthermore, we are looking for a cell which leads into the
c        interior of the array for which the filled area will be on
c        the right as we follow it into the interior of the array
         do m=1,nrc
            mc=m
            nc=ncx(m)

            if (new_curve) then

               if (.not.GRF_CNTR_FLAG
     &                 (cells(1,nc),nrx,tst,nx,ny,ne)) then

                  if (nc .eq. 1) then
                     if (v(nx,ny  ) .ge. vc(1) .and.
     &                   v(nx,ny+1) .lt. vc(1)) go to 29
                     if (v(nx,ny  ) .gt. vc(1) .and.
     &                   v(nx,ny+1) .le. vc(1)) go to 29
                  else
                     if (v(nx,ny  ) .le. vc(2) .and.
     &                   v(nx,ny+1) .gt. vc(2)) go to 29
                     if (v(nx,ny  ) .lt. vc(2) .and.
     &                   v(nx,ny+1) .ge. vc(2)) go to 29
                  end if
               end if
            else
               if (nc .eq. 1) then
                  if (v(nx,ny  ) .ge. vc(1) .and.
     &                v(nx,ny+1) .lt. vc(1)) go to 23
                  if (v(nx,ny  ) .gt. vc(1) .and.
     &                v(nx,ny+1) .le. vc(1)) go to 23
               else
                  if (v(nx,ny  ) .le. vc(2) .and.
     &                v(nx,ny+1) .gt. vc(2)) go to 23
                  if (v(nx,ny  ) .lt. vc(2) .and.
     &                v(nx,ny+1) .ge. vc(2)) go to 23
               end if
            end if
         end do

c        No crossings were acceptable; continue search
         if (ccw) then
            ny=ny+1
         else
            ny=ny-1
         end if
         if (ny .gt. 0 .and. ny .lt. nry) go to 13
      end if
14    pass=pass+1
      if (pass .eq. 5) go to 91

c     Continue to the next edge
      if (ccw) then
         my2=nry
         inc=1
         nx=nrx
         ny=nry
      else
         my2=1
         inc=-1
         nx=nrx
         ny=1
      end if
      if (.not.new_curve) then

c        Store coordinates along this edge
         if (inc .gt. 0) then
            if (my1 .eq. ny0) my1=ny0+inc
         else
            if (my1 .eq.  yc) my1=ny0+inc
         end if
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(a8,i2,4i5)') 'fill',edge,nx,my1,nx,my2
         do m=my1,my2,inc
            if (nrcntrp .ge. mxcntrp) go to 99
            nrcntrp=nrcntrp+1
            cntrpx (nrcntrp)=nx
            cntrpy (nrcntrp)=m
cxx            write(lwpcLOG_lun,'(30x,i5,2f10.4)')
cxx     &            nrcntrp,cntrpx(nrcntrp),cntrpy(nrcntrp)
         end do
         mx1=nx
         my1=ny
      end if
      if (ccw) then
         edge=3
      else
         edge=1
      end if
      if (.not.ccw) go to 11

c     Search top edge for crossing of VC
15    if (ccw) then

c        Search right to left
         nx1=nx
         nx2=1
      else

c        Search left to right
         nx1=nx
         nx2=nrx
      end if
      ne=1

      call GRF_CNTR_ROW
     &    (v,mxx,mxy,nrx,vc,nx1,nx2,
     &     nx,ny,nrc,ncx,xcx,ycx)

cxxcDEBUG
cxx      if (nrc .eq. 0) then
cxx         write(lwpcLOG_lun,'(a10,3i5,2i5,2f10.4)')
cxx     &        'edge',edge,nx,ny
cxx      else
cxx         write(lwpcLOG_lun,'(a10,3i5,2l5,3f10.4)')
cxx     &        'edge',edge,nx,ny,
cxx     &               GRF_CNTR_FLAG(cells(1,1),nrx,tst,nx,ny,ne),
cxx     &               GRF_CNTR_FLAG(cells(1,2),nrx,tst,nx,ny,ne),
cxx     &               v(nx-1,ny),v(nx,ny),v(nx+1,ny)
cxx      end if

      if (nrc .gt. 0) then

c        Check all crossings;
c        if we're looking for the start of a new curve, then we need
c        a cell that hasn't already been used
c        furthermore, we are looking for a cell which leads into the
c        interior of the array for which the filled area will be on
c        the right as we follow it into the interior of the array
         do m=1,nrc
            mc=m
            nc=ncx(m)

            if (new_curve) then

               if (.not.GRF_CNTR_FLAG
     &                 (cells(1,nc),nrx,tst,nx,ny,ne)) then

                  if (nc .eq. 1) then
                     if (v(nx+1,ny) .ge. vc(1) .and.
     &                   v(nx  ,ny) .lt. vc(1)) go to 29
                     if (v(nx+1,ny) .gt. vc(1) .and.
     &                   v(nx  ,ny) .le. vc(1)) go to 29
                  else
                     if (v(nx+1,ny) .le. vc(2) .and.
     &                   v(nx  ,ny) .gt. vc(2)) go to 29
                     if (v(nx+1,ny) .lt. vc(2) .and.
     &                   v(nx  ,ny) .ge. vc(2)) go to 29
                  end if
               end if
            else
               if (nc .eq. 1) then
                  if (v(nx+1,ny) .ge. vc(1) .and.
     &                v(nx  ,ny) .lt. vc(1)) go to 21
                  if (v(nx+1,ny) .gt. vc(1) .and.
     &                v(nx  ,ny) .le. vc(1)) go to 21
               else
                  if (v(nx+1,ny) .le. vc(2) .and.
     &                v(nx  ,ny) .gt. vc(2)) go to 21
                  if (v(nx+1,ny) .lt. vc(2) .and.
     &                v(nx  ,ny) .ge. vc(2)) go to 21
               end if
            end if
         end do

c        No crossings were acceptable; continue search
         if (.not.ccw) then
            nx=nx+1
         else
            nx=nx-1
         end if
         if (nx .gt. 0 .and. nx .lt. nrx) go to 15
      end if
16    pass=pass+1
      if (pass .eq. 5) go to 91

c     Continue to the next edge
      if (ccw) then
         mx2=1
         inc=-1
         nx=1
         ny=nry
      else
         mx2=nrx
         inc=1
         nx=nrx
         ny=nry
      end if
      if (.not.new_curve) then

c        Store coordinates along this edge
         if (inc .gt. 0) then
            if (mx1 .eq. nx0) mx1=nx0+inc
         else
            if (mx1 .eq.  xc) mx1=nx0+inc
         end if
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(a8,i2,4i5)') 'fill',edge,mx1,ny,mx2,ny
         do m=mx1,mx2,inc
            if (nrcntrp .ge. mxcntrp) go to 99
            nrcntrp=nrcntrp+1
            cntrpx (nrcntrp)=m
            cntrpy (nrcntrp)=ny
cxx            write(lwpcLOG_lun,'(30x,i5,2f10.4)')
cxx     &            nrcntrp,cntrpx(nrcntrp),cntrpy(nrcntrp)
         end do
         mx1=nx
         my1=ny
      end if
      if (ccw) then
         edge=4
      else
         edge=2
      end if
      if (.not.ccw) go to 13

c     Search left edge for crossing of VC
17    if (ccw) then

c        Search top to bottom
         ny1=ny
         ny2=1
      else

c        Search bottom to top
         ny1=ny
         ny2=nry
      end if
      ne=4

      call GRF_CNTR_COL
     &    (v,mxx,mxy,nry,vc,ny1,ny2,
     &     nx,ny,nrc,ncx,xcx,ycx)

cxxcDEBUG
cxx      if (nrc .eq. 0) then
cxx         write(lwpcLOG_lun,'(a10,3i5,2i5,2f10.4)')
cxx     &        'edge',edge,nx,ny
cxx      else
cxx         write(lwpcLOG_lun,'(a10,3i5,2l5,3f10.4)')
cxx     &        'edge',edge,nx,ny,
cxx     &               GRF_CNTR_FLAG(cells(1,1),nrx,tst,nx,ny,ne),
cxx     &               GRF_CNTR_FLAG(cells(1,2),nrx,tst,nx,ny,ne),
cxx     &               v(nx,ny-1),v(nx,ny),v(nx,ny+1)
cxx      end if

      if (nrc .gt. 0) then

c        Check all crossings;
c        if we're looking for the start of a new curve, then we need
c        a cell that hasn't already been used
c        furthermore, we are looking for a cell which leads into the
c        interior of the array for which the filled area will be on
c        the right as we follow it into the interior of the array
         do m=1,nrc
            mc=m
            nc=ncx(m)

            if (new_curve) then

               if (.not.GRF_CNTR_FLAG
     &                 (cells(1,nc),nrx,tst,nx,ny,ne)) then

                  if (nc .eq. 1) then
                     if (v(nx,ny+1) .ge. vc(1) .and.
     &                   v(nx,ny  ) .lt. vc(1)) go to 29
                     if (v(nx,ny+1) .gt. vc(1) .and.
     &                   v(nx,ny  ) .le. vc(1)) go to 29
                  else
                     if (v(nx,ny+1) .le. vc(2) .and.
     &                   v(nx,ny  ) .gt. vc(2)) go to 29
                     if (v(nx,ny+1) .lt. vc(2) .and.
     &                   v(nx,ny  ) .ge. vc(2)) go to 29
                  end if
               end if
            else
               if (nc .eq. 1) then
                  if (v(nx,ny+1) .ge. vc(1) .and.
     &                v(nx,ny  ) .lt. vc(1)) go to 23
                  if (v(nx,ny+1) .gt. vc(1) .and.
     &                v(nx,ny  ) .le. vc(1)) go to 23
               else
                  if (v(nx,ny+1) .le. vc(2) .and.
     &                v(nx,ny  ) .gt. vc(2)) go to 23
                  if (v(nx,ny+1) .lt. vc(2) .and.
     &                v(nx,ny  ) .ge. vc(2)) go to 23
               end if
            end if
         end do

c        No crossings were acceptable; continue search
         if (.not.ccw) then
            ny=ny+1
         else
            ny=ny-1
         end if
         if (ny .gt. 0 .and. ny .lt. nry) go to 17
      end if
18    pass=pass+1
      if (pass .eq. 5) go to 91

c     Continue to the next edge
      if (ccw) then
         my2=1
         inc=-1
         nx=1
         ny=1
      else
         my2=nry
         inc=1
         nx=1
         ny=nry
      end if
      if (.not.new_curve) then

c        Store coordinates along this edge
         if (inc .gt. 0) then
            if (my1 .eq. ny0) my1=ny0+inc
         else
            if (my1 .eq.  yc) my1=ny0+inc
         end if
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(a8,i2,4i5)') 'fill',edge,nx,my1,nx,my2
         do m=my1,my2,inc
            if (nrcntrp .ge. mxcntrp) go to 99
            nrcntrp=nrcntrp+1
            cntrpx (nrcntrp)=nx
            cntrpy (nrcntrp)=m
cxx            write(lwpcLOG_lun,'(30x,i5,2f10.4)')
cxx     &            nrcntrp,cntrpx(nrcntrp),cntrpy(nrcntrp)
         end do
         mx1=nx
         my1=ny
      end if
      if (ccw) then
         edge=1
      else
         edge=3
      end if
      if (.not.ccw) go to 15
      go to 11

c     Found a crossing along a row;
c     store coordinates along this edge
21    mx2=nx
      if (mx1 .lt. mx2) then
         inc=1
         if (mx1 .ne. 1) mx1=mx1+1
      else
         inc=-1
         if (mx2 .ne. 1) mx2=mx2+1
      end if
      if (mx1 .ne. mx2) then
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(a8,i2,4i5)') 'fill',edge,mx1,ny,mx2,ny
         do m=mx1,mx2,inc
            if (nrcntrp .ge. mxcntrp) go to 99
            nrcntrp=nrcntrp+1
            cntrpx (nrcntrp)=m
            cntrpy (nrcntrp)=ny
cxx            write(lwpcLOG_lun,'(30x,i5,2f10.4)')
cxx     &            nrcntrp,cntrpx(nrcntrp),cntrpy(nrcntrp)
         end do
         mx1=mx2
      end if

c     If at the end of the row,
c     check if we should continue to the next side
      vcx=vc(nc)
      if (edge .eq. 1) then
         if (ccw .and. nx .eq. nrx-1) then
            if (nc .eq. 1) then
               if (v(nrx,1) .eq. vcx .and. v(nrx,2) .le. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 12
               end if
            else
               if (v(nrx,1) .eq. vcx .and. v(nrx,2) .ge. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 12
               end if
            end if
         else
     &   if (.not.ccw .and. nx .eq. 1) then
            if (nc .eq. 1) then
               if (v(1,1) .eq. vcx .and. v(1,2) .le. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 12
               end if
            else
               if (v(1,1) .eq. vcx .and. v(1,2) .ge. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 12
               end if
            end if
         end if
      else
         if (ccw .and. nx .eq. 1) then
            if (nc .eq. 1) then
               if (v(1,nry) .eq. vcx .and. v(1,nry-1) .le. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 16
               end if
            else
               if (v(1,nry) .eq. vcx .and. v(1,nry-1) .ge. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 16
               end if
            end if
         else
     &   if (.not.ccw .and. nx .eq. nrx-1) then
            if (nc .eq. 1) then
               if (v(nrx,ny) .eq. vcx .and. v(nrx,ny-1) .le. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 16
               end if
            else
               if (v(nrx,ny) .eq. vcx .and. v(nrx,ny-1) .ge. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 16
               end if
            end if
         end if
      end if
      go to 29

c     Found a crossing along a column;
c     store coordinates along this edge
23    my2=ny
      if (my1 .lt. my2) then
         inc=1
         if (my1 .ne. 1) my1=my1+1
      else
         inc=-1
         if (my2 .ne. 1) my2=my2+1
      end if
      if (my1 .ne. my2) then
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(a8,i2,4i5)') 'fill',edge,nx,my1,nx,my2
         do m=my1,my2,inc
            if (nrcntrp .ge. mxcntrp) go to 99
            nrcntrp=nrcntrp+1
            cntrpx (nrcntrp)=nx
            cntrpy (nrcntrp)=m
cxx            write(lwpcLOG_lun,'(30x,i5,2f10.4)')
cxx     &            nrcntrp,cntrpx(nrcntrp),cntrpy(nrcntrp)
         end do
         my1=my2
      end if

c     At the end of the column;
c     check if we should continue to the next side
      vcx=vc(nc)
      if (edge .eq. 2) then
         if (ccw .and. ny .eq. nry-1) then
            if (nc .eq. 1) then
               if (v(nrx,nry) .eq. vcx .and. v(nrx-1,nry) .le. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 14
               end if
            else
               if (v(nrx,nry) .eq. vcx .and. v(nrx-1,nry) .ge. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 14
               end if
            end if
         else
     &   if (.not.ccw .and. ny .eq. 1) then
            if (nc .eq. 1) then
               if (v(nrx,1) .eq. vcx .and. v(nrx-1,1) .le. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 14
               end if
            else
               if (v(nrx,1) .eq. vcx .and. v(nrx-1,1) .ge. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 14
               end if
            end if
         end if
      else
         if (ccw .and. ny .eq. 1) then
            if (nc .eq. 1) then
               if (v(1,1) .eq. vcx .and. v(2,1) .le. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 18
               end if
            else
               if (v(1,1) .eq. vcx .and. v(2,1) .ge. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 18
               end if
            end if
         else
     &   if (.not.ccw .and. ny .eq. nry-1) then
            if (nc .eq. 1) then
               if (v(1,nry) .eq. vcx .and. v(2,nry) .le. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 18
               end if
            else
               if (v(1,nry) .eq. vcx .and. v(2,nry) .ge. vcx) then
                  bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,ne)
                  go to 18
               end if
            end if
         end if
      end if

c     Found a crossing;
c     set the parameters to be returned and
c     flag the last cell as used
29    nc=ncx(mc)
      xc=xcx(mc)
      yc=ycx(mc)

      vcx=vc(nc)
      if (edge .eq. 1) then

         bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,1)
      else
     &if (edge .eq. 2) then

         bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx  ,ny,4)
         bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx-1,ny,2)
      else
     &if (edge .eq. 3) then

         bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny  ,1)
         bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny-1,3)
      else
c        (edge .eq. 4)

         bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,4)
      end if

cxxcDEBUG
cxx      m=29
cxx      write(lwpcLOG_lun,'(a10,3i5,5x,i5,2f10.4)')
cxx     &     'edge',m,nx,ny,nc,xc,yc
      RETURN

c     No crossings found
91    edge=0
      nx=nx0
      ny=ny0

cxxcDEBUG
cxx      m=91
cxx      write(lwpcLOG_lun,'(a10,3i5,5x,i5,2f10.4)')
cxx     &     'edge',m
      RETURN

c     Too many points generated while going around the edge
99    write(error_msg,
     &    '(''[GRF_CNTR_EDGE]: '',
     &      ''The search around the edge of the grid '',
     &      ''produced more than '',i4,'' points'')')
     &        mxcntrp
      call LWPC_ERROR ('ERROR',error_msg)
      END      ! GRF_CNTR_EDGE