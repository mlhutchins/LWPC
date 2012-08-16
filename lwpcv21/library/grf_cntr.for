      SUBROUTINE GRF_CNTR
     &          (v,mxx,mxy,nrx,nry,
     &           vcntr,status)

c***********************************************************************

c  Function:
c     This routine searches input array to find contour lines.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     v            [r,mxx,mxy]   array containing values for contours
c     mxx          [i]           X dimension of V in calling program
c     mxy          [i]           Y dimension of V in calling program
c     nrx          [i]           number of elements of V along X
c     nry          [i]           number of elements of V along Y
c     vcntr        [r,2]         definition of the contour band
c     status       [s]           status string [begin, continue]

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:
c     grf_cntr_cell
c     grf_cntr_flag
c     grf_cntr_edge
c     grf_cntr_holes
c     grf_cntr_search
c     grf_cntr_trace

c  References:

c  Change History:

c     27 May 98     Call to GRF_CNTR_CELL to ensure that an interior
c                   cell actually has exits before starting a contour
c                   trace; removed adjustment of contour levels when
c                   grid points have the same values as the contours.

c     25 Aug 98     Modified to use new GRF_CNTR_TRACE.

c*******************!***************************************************

      include      'grf_cntr.cmn'
      include      'lwpc_lun.cmn'

cxxcDEBUG
cxx      integer       str_length

      character*(*) status
      character*  8 xstatus
      character* 10 label
      character*200 error_msg

      logical       bit
      logical       ccw
      logical       first
      logical       grf_cntr_flag      ! Function
      logical       new_curve
      logical       set
      logical       tst

      integer       edge
      integer       edge0
      integer       ex
      integer       i
      integer       j
      integer       mxx
      integer       mxy
      integer       n
      integer       nc
      integer       ncx(2)
      integer       nex
      integer       nrc
      integer       nrow
      integer       nrow1
      integer       nrow2
      integer       nrx
      integer       nry
      integer       nx
      integer       nx0
      integer       nx1
      integer       nx2
      integer       ny
      integer       ny0
      integer       polygons
      integer       trace

      real          v(mxx,mxy)
      real          vc(2)
      real          vcmax
      real          vcmin
      real          vcntr(2)
      real          vmax
      real          vmin
      real          xc
      real          xc0
      real          xcx(2)
      real          xen
      real          xex
      real          yc
      real          yc0
      real          ycx(2)
      real          yen
      real          yex

      data          first/.true./
      data          set/.true./
      data          tst/.false./

c     We trace the contour in a counter-clockwise direction, keeping the
c     part of the array with values below the contour on the right.

c     The corners and edges of each cell are numbered from 1 to 4 in a
c     counter-clockwise direction with the lower left corner and the
c     bottom edge being 1.

      if (first) then

c        Make sure that the work array is large enough
         if ((nrx*nry*4)/32+1 .gt. mxcells) then

            write(error_msg,
     &             '(''ERROR [GRF_CNTR]: '',
     &               ''CELLS must be dimensioned at least'',i5)')
     &               (nrx*nry*4)/32+1

            call GRF_DONE
            call LWPC_ERROR ('ERROR',error_msg)
         end if
         first=.false.
      end if
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(/2a10,2f10.4)')
cxx     &     'Contour',status(:STR_LENGTH(status)),vcntr

c     Counter for the number of holes and number of points
      nrholes=0
      nrpts(0)=0

      xstatus=status
      call STR_LOWER (xstatus,0,0)
      if (xstatus .eq. 'begin') then

c        Sort input contour levels
         vcmax=MAX(vcntr(1),vcntr(2))
         vcmin=MIN(vcntr(1),vcntr(2))

c        Scan the array for its range
         vmin=v(1,1)
         vmax=v(1,1)
         do i=1,nrx
            do j=1,nry
               vmin=MIN(vmin,v(i,j))
               vmax=MAX(vmax,v(i,j))
            end do
         end do

c        Check special cases
         if (vmin .ge. vcmax) then

            status='complete'
            RETURN
         end if

         if (vmin .ge. vcmin .and. vmax .le. vcmax) then

c           The whole array is to be filled
            cntrpx(1)=1
            cntrpy(1)=1
            cntrpx(2)=nrx
            cntrpy(2)=1
            cntrpx(3)=nrx
            cntrpy(3)=nry
            cntrpx(4)=1
            cntrpy(4)=nry
            cntrpx(5)=1
            cntrpy(5)=1

            nrpts(0)=5
            status='complete'
            RETURN
         end if

c        Set contour level array
         vc(1)=vcmax
         vc(2)=vcmin

c        Arbitrary adjustment to handle grids with corners that
c        match the contour levels
         vc(1)=vc(1)+.00099
         vc(2)=vc(2)-.00099

c        Initialize counters for sides
         do n=1,mxcells
            cells(n,1)=0
            cells(n,2)=0
         end do

c        Counter for the number of polygons
         polygons=0

c        We first search the edge of the data, looking for VC1 to cross
c        the boundary; we start at the bottom, left corner
         xc0=1.
         yc0=1.
         nx0=1
         ny0=1
         edge0=1
         if (v(1,1) .eq. vc(1) .or. v(1,1) .eq. vc(2)) then
            xc0=2.
            nx0=2
         end if

c        Set range for interior search (executed after search of edges)
         nrow1=1
         nrow2=nry-1
      end if

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(/a10)') 'Edges'

      do while (edge0 .gt. 0)

c        Start in the counter-clockwise direction
         ccw=.true.
         new_curve=.true.

c        Set up return to last intersection with the edge
         edge=edge0
         nx=nx0
         ny=ny0
         xc=xc0
         yc=yc0

c        Find the first intersection with the edge
         call GRF_CNTR_EDGE
     &       (v,mxx,mxy,nrx,nry,vc,
     &        nc,xc,yc,nx,ny,edge,
     &        ccw,new_curve)

         edge0=edge

         if (edge .gt. 0) then

c           Find out if there is an exit
            xen=xc
            yen=yc

c           Adjust indices so that the lower left corner of the
c           current cell is consistent with the numbering scheme
c           for the edges
            i=nx
            j=ny
            if (edge .eq. 2) i=i-1
            if (edge .eq. 3) j=j-1

            call GRF_CNTR_CELL
     &          (v,mxx,mxy,nrx,vc,nc,
     &           edge,xen,yen,i,j,
     &           nex,ex,xex,yex,label)

            if (nex .eq. 0) then

c              No exits;
c              tag this cell so it won't be used again
               bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,edge)

               nx0=nx
               ny0=ny
            else

c              Store starting point
               edge0=edge
               nx0=nx
               ny0=ny
               xc0=xc
               yc0=yc

c              Counter for the number of points in the polygon
               nrcntrp=0

c              Trace contour line
               call GRF_CNTR_TRACE
     &             (v,mxx,mxy,nrx,nry,vc,
     &              nc,edge,nx,ny,xc,yc,'edges')
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(/a10)') 'Close cntr'

c              Done with this contour line;
c              check for a valid number of points
               if (nrcntrp .gt. 5) then

c                 Increment the polygon counter
                  polygons=polygons+1
cxycDEBUG
cxy                  write(lwpcLOG_lun,'(/a10,2i6)')
cxy     &                 'Polygon',polygons,nrcntrp
cxy                  do n=1,nrcntrp
cxy                     write(lwpcLOG_lun,'(5x,i5,2f10.3)')
cxy     &                     n,cntrpx(n),cntrpy(n)
cxy                  end do
cxy                  write(lwpcLOG_lun,'(1x)')

c                 Search the interior of the enclosed area for areas
c                 where the array is greater than the contour level;
c                 these appear as holes in the filled area.

c                 Counter for the number of holes
                  nrholes=0
                  nrpts(0)=nrcntrp

                  call GRF_CNTR_HOLES
     &                (v,mxx,mxy,nrx,nry,vc)
cxycDEBUG
cxy                  write(lwpcLOG_lun,'(/a10,i5)')
cxy     &                 'Hole',nrholes
cxy                  do n=nrpts(nrholes-1)+1,nrpts(nrholes)
cxy                     write(lwpcLOG_lun,'(2i5,2f10.4)')
cxy     &                     n-nrpts(nrholes-1),n,
cxy     &                     cntrpx(n),cntrpy(n)
cxy                  end do
cxy                  write(lwpcLOG_lun,'(1x)')
               end if
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(/a10)') 'Close cntr'

               status='continue'
               RETURN
            end if
         end if
      end do

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(/a10)') 'Interior'

c     Search the interior of the array;
c     now we should only find closed polygons
      do nrow=nrow1,nrow2

         nx1=2
         nx2=nrx-1
         do while (nx1 .lt. nx2)

            edge=1
            ny=nrow

            call GRF_CNTR_ROW
     &          (v,mxx,mxy,nrx,vc,nx1,nx2,
     &           nx,ny,nrc,ncx,xcx,ycx)
cxxcDEBUG
cxx            write(lwpcLOG_lun,'(a10,3i5,2f10.4,l5)')
cxx     &           'crossing',nrc

            if (nrc .eq. 0) then

c              No crossings were found;
c              terminate search along this row
               nx1=nx2
               trace=0
            else

c              Check the crossings
               n=1
19             nc=ncx(n)
               xc=xcx(n)
               yc=ycx(n)
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(a10,3i5,2f10.4,l5)')
cxx     &              'crossing',nx,ny,nc,v(nx,ny),v(nx+1,ny),
cxx     &               GRF_CNTR_FLAG
cxx     &              (cells(1,nc),nrx,tst,nx,ny,edge)

               if (GRF_CNTR_FLAG
     &            (cells(1,nc),nrx,tst,nx,ny,edge)) then

c                 This cell has been used
                  trace=0
               else

c                 This cell hasn't been used yet
                  trace=1

c                 Find out if there is an exit
                  xen=xc
                  yen=yc

                  call GRF_CNTR_CELL
     &                (v,mxx,mxy,nrx,vc,nc,
     &                 edge,xen,yen,nx,ny,
     &                 nex,ex,xex,yex,label)

c                 Start a contour only if there is one exit
                  if (nex .eq. 1) then

c                    Verify that this exit satisfies
c                    the tracing criteria
                     if (nc .eq. 1 .and.
     &                  ((ex .eq. 2 .and.
     &                    v(nx+1,ny  ) .gt. vc(nc)) .or.
     &                   (ex .eq. 3 .and.
     &                    v(nx+1,ny  ) .gt. vc(nc) .and.
     &                    v(nx+1,ny+1) .gt. vc(nc)) .or.
     &                   (ex .eq. 4 .and.
     &                    v(nx  ,ny  ) .lt. vc(nc)))) then
                        trace=0
                     else
     &               if (nc .eq. 2 .and.
     &                  ((ex .eq. 2 .and.
     &                    v(nx+1,ny  ) .lt. vc(nc)) .or.
     &                   (ex .eq. 3 .and.
     &                    v(nx  ,ny  ) .lt. vc(nc) .and.
     &                    v(nx  ,ny+1) .lt. vc(nc)) .or.
     &                   (ex .eq. 4 .and.
     &                    v(nx  ,ny  ) .gt. vc(nc)))) then
                        trace=0
                     end if
                  else

c                    Zero or multiple exits
                     trace=0
                  end if
               end if
               if (trace .eq. 0 .and. n .lt. nrc) then
                  n=2
                  go to 19
               end if
            end if

            if (trace .eq. 1) then

c              Store current row
               nrow1=nrow

c              Counter for the number of points in the polygon
               nrcntrp=0

c              Trace contour line
               call GRF_CNTR_TRACE
     &             (v,mxx,mxy,nrx,nry,vc,
     &              nc,edge,nx,ny,xen,yen,'interior')
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(/a10)') 'Close cntr'

c              Done with this contour line;
c              check for a valid number of points
               if (nrcntrp .gt. 5) then

c                 Increment the polygon counter
                  polygons=polygons+1
cxycDEBUG
cxy                  write(lwpcLOG_lun,'(/a10,2i5)')
cxy     &                 'Polygon',polygons,nrcntrp
cxy                  do n=1,nrcntrp
cxy                     write(lwpcLOG_lun,'(5x,i5,2f10.3)')
cxy     &                     n,cntrpx(n),cntrpy(n)
cxy                  end do
cxy                  write(lwpcLOG_lun,'(1x)')

c                 Search the interior of the enclosed area
c                 for areas where the array is greater than
c                 the contour level; these appear as holes
c                 in the filled area.

c                 Counter for the number of holes
                  nrholes=0
                  nrpts(0)=nrcntrp

                  call GRF_CNTR_HOLES
     &                (v,mxx,mxy,nrx,nry,vc)
cxycDEBUG
cxy                  write(lwpcLOG_lun,'(/a10,51i5)')
cxy     &                 'Holes',nrholes,(nrpts(n),n=0,nrholes)
cxy                  do n=nrpts(nrholes-1)+1,nrpts(nrholes)
cxy                     write(lwpcLOG_lun,'(2i5,2f10.4)')
cxy     &                     n-nrpts(nrholes-1),n,
cxy     &                     cntrpx(n),cntrpy(n)
cxy                  end do
cxy                  write(lwpcLOG_lun,'(1x)')
               end if
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(/a10)') 'Close cntr'

               status='continue'
               RETURN
            else

c              This crossing is not acceptable;
c              continue search
               nx1=nx+1
            end if
         end do
      end do

      status='complete'
      RETURN
      END      ! GRF_CNTR