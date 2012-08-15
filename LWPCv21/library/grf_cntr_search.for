      SUBROUTINE GRF_CNTR_SEARCH
     &          (v,mxx,mxy,nrx,nry,vc,
     &           nc,xc,yc,nx,ny,edge)

c***********************************************************************

c  Function:
c     This routine searches the interior of the contour area until the
c     contour hits an edge or closes. This routine is not called unless
c     a cell has been found in which at least one side is intersected by
c     the contour line.

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
c     xc           [r]           X coordinate of the initial crossing
c     yc           [r]           Y coordinate of the initial crossing
c     nx           [i]           X index      of the initial crossing
c     ny           [i]           Y index      of the initial crossing
c     edge         [i]           edge         of the initial crossing

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}
c     xc           [r]           X coordinate of the final crossing
c     yc           [r]           Y coordinate of the final crossing
c     nx           [i]           X index      of the final crossing
c     ny           [i]           Y index      of the final crossing
c     edge         [i]           edge         of the final crossing

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:
c     abs

c     grf_cntr_cell
c     grf_cntr_flag

c  References:

c  Change History:

c     27 May 98     Changes to allow grid values to be the same as
c                   contour levels; additional tests to properly
c                   handle cells with no exits (like when the contour
c                   closes at the entrance cell).

c*******************!***************************************************

      IMPLICIT      NONE

      include      'grf_cntr.cmn'
      include      'lwpc_lun.cmn'

      character* 10 label
      character*200 error_msg

      logical       grf_cntr_flag      ! Function
      logical       bit
      logical       set

      integer       edge
      integer       edge0
      integer       en
      integer       ex
      integer       i
      integer       i0
      integer       j
      integer       j0
      integer       mxx
      integer       mxy
      integer       nc
      integer       nex
      integer       nrp
      integer       nrx
      integer       nry
      integer       nx
      integer       nx0
      integer       ny
      integer       ny0

      real          v(mxx,mxy)
      real          vc(2)
      real          xc
      real          xc0
      real          xen
      real          xex
      real          yc
      real          yc0
      real          yen
      real          yex

      data          set/.true./

c     The corners and edges of each cell are numbered from 1 to 4 in a
c     counter-clockwise direction with the lower left corner and the
c     bottom edge being 1.

c     We trace the contour line keeping values of the array which are
c     lower than VC on the right (enclosed by the curve).

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(/a10,5i5,3f10.4/)')
cxx     &     'Search',edge,nx,ny,nc,nrcntrp,xc,yc,vc(nc)

c     Initialize indices
      i=nx
      j=ny

c     Adjust indices so that the lower left corner of the current cell
c     is consistent with the numbering scheme for the edges
      if (edge .eq. 2 .and. i .eq. nrx) i=i-1
      if (edge .eq. 3 .and. j .eq. nry) j=j-1

c     Store initial conditions
      edge0=edge
      nx0=nx
      ny0=ny
      xc0=xc
      yc0=yc

      i0=i
      j0=j

      nrp=nrcntrp

c     Store the entrance point
      nrcntrp=nrcntrp+1
      cntrpx (nrcntrp)=xc
      cntrpy (nrcntrp)=yc

c     Set the flag for the entrance edge
      bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,i,j,edge)

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,i5,15x,i5,2f10.4)')
cxx     &     'begin',edge,nrcntrp,xc,yc

c     Enter this cell and return with its exit parameters
11    en=edge
      xen=xc
      yen=yc

      call GRF_CNTR_CELL
     &    (v,mxx,mxy,nrx,vc,nc,
     &     en,xen,yen,i,j,
     &     nex,ex,xex,yex,label)

c     Check for no exits from this cell
      if (nex .eq. 0) then

c        Check if we are back to the entrance
         if (i .eq. i0 .and. j .eq. j0) go to 95

         xen=-1.
         yen=-1.
         if (j .eq.     1 .and. en .ne. 1) then

c           Check for possible exit at the bottom edge
            ex=1
            yex=1
            if (v(i  ,1) .eq. vc(nc)) then
               xex=i
               i=i-1
            else
     &      if (v(i+1,1) .eq. vc(nc)) then
               xex=i+1
               i=i+1
            end if
         else
     &   if (i .eq. nrx-1 .and. en .ne. 2) then

c           Check for possible exit at the right edge
            ex=2
            xex=nrx
            if (v(nrx,j  ) .eq. vc(nc)) then
               yex=j
               j=j-1
            else
     &      if (v(nrx,j+1) .eq. vc(nc)) then
               yex=j+1
               j=j+1
            end if
         else
     &   if (j .eq. nry-1 .and. en .ne. 3) then

c           Check for possible exit at the top edge
            ex=3
            yex=nry
            if (v(i  ,nry) .eq. vc(nc)) then
               xex=i
               i=i-1
            else
     &      if (v(i+1,nry) .eq. vc(nc)) then
               xex=i+1
               i=i+1
            end if
         else
     &   if (i .eq.     1 .and. en .ne. 4) then

c           Check for possible exit at the left edge
            ex=4
            xex=1
            if (v(1,j  ) .eq. vc(nc)) then
               yex=j
               j=j-1
            else
     &      if (v(1,j+1) .eq. vc(nc)) then
               yex=j+1
               j=j+1
            end if
         else

c           No luck
            go to 99
         end if
      end if

c     Set the flag for the exit edge of the current cell
      bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,i,j,ex)

c     Store point at the exit edge
      nrcntrp=nrcntrp+1
      cntrpx (nrcntrp)=xex
      cntrpy (nrcntrp)=yex
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,i5,15x,i5,2f10.4)')
cxx     &      label,ex,nrcntrp,xex,yex

c     Transfer cell exit data to working parameters
      edge=ex
      xc=xex
      yc=yex

c     Check if we are back at the beginning
      if (xc .eq. xc0 .and. yc .eq. yc0) go to 90

c     Check if current cell is on the bottom edge
      if (j .eq.     1 .and. edge .eq. 1) go to 91

c     Check if current cell is on the right edge
      if (i .eq. nrx-1 .and. edge .eq. 2) go to 92

c     Check if current cell is on the top edge
      if (j .eq. nry-1 .and. edge .eq. 3) go to 93

c     Check if current cell is on the left edge
      if (i .eq.     1 .and. edge .eq. 4) go to 94

c     Move search to the next (adjacent) cell
      if (edge .eq. 1) then
         edge=3
         j=j-1
      else
     &if (edge .eq. 2) then
         edge=4
         i=i+1
      else
     &if (edge .eq. 3) then
         edge=1
         j=j+1
      else
c        (edge .eq. 4)
         edge=2
         i=i-1
      end if

c     Set the flag for the edge of the adjacent cell
      bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,i,j,edge)

c     Check for closure;
c     this can only happen if we have returned to the original cell
c     and we are at the original edge
      if (edge .ne. edge0 .or.
     &    i  .ne. nx0 .or. j  .ne. ny0 .or.
     &    xc .ne. xc0 .or. yc .ne. yc0) go to 11

c     We are back to the entrance cell;
c     set exit cell to initial parameters
90    edge=edge0
      nx=nx0
      ny=ny0
      xc=xc0
      yc=yc0

c     Store point at the exit edge
      nrcntrp=nrcntrp+1
      cntrpx (nrcntrp)=xc
      cntrpy (nrcntrp)=yc
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,3i5,10x,2f10.4)')
cxx     &     'exit 90',edge,nx,ny,xc,yc
      RETURN

c     Current cell is on the bottom edge
91    nx=i
      ny=1
      edge=1

c     Store point at the exit edge
      nrcntrp=nrcntrp+1
      cntrpx (nrcntrp)=xc
      cntrpy (nrcntrp)=yc
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,3i5,10x,2f10.4)')
cxx     &     'exit 91',edge,nx,ny,xc,yc
      RETURN

c     Current cell is on the right edge;
c     set indices to be at the edge
92    nx=nrx
      ny=j
      edge=2

c     Set the flag for the edge of the exit cell
      bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,4)

c     Store point at the exit edge
      nrcntrp=nrcntrp+1
      cntrpx (nrcntrp)=xc
      cntrpy (nrcntrp)=yc
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,3i5,10x,2f10.4)')
cxx     &     'exit 92',edge,nx,ny,xc,yc
      RETURN

c     Current cell is on the top edge;
c     set indices to be at the edge
93    nx=i
      ny=nry
      edge=3

c     Set the flag for the edge of the exit cell
      bit=GRF_CNTR_FLAG(cells(1,nc),nrx,set,nx,ny,1)

c     Store point at the exit edge
      nrcntrp=nrcntrp+1
      cntrpx (nrcntrp)=xc
      cntrpy (nrcntrp)=yc
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,3i5,10x,2f10.4)')
cxx     &     'exit 93',edge,nx,ny,xc,yc
      RETURN

c     Current cell is on the left edge
94    nx=1
      ny=j
      edge=4

c     Store point at the exit edge
      nrcntrp=nrcntrp+1
      cntrpx (nrcntrp)=xc
      cntrpy (nrcntrp)=yc
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,3i5,10x,2f10.4)')
cxx     &     'exit 94',edge,nx,ny,xc,yc
      RETURN

c     We are back to the entrance cell;
c     set exit cell to initial parameters
95    edge=edge0
      nx=nx0
      ny=ny0
      xc=xc0
      yc=yc0

c     Store point at the exit edge
      nrcntrp=nrcntrp+1
      cntrpx (nrcntrp)=xc
      cntrpy (nrcntrp)=yc
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,i5,15x,i5,2f10.4)')
cxx     &      label,edge,nrcntrp,xc,yc
cxx      write(lwpcLOG_lun,'(a10,3i5,10x,2f10.4)')
cxx     &     'exit 95',edge,nx,ny,xc,yc
      RETURN

c     Nothing can be done;
c     maybe we can ignore the problem
99    if (nrcntrp-nrp .lt. 9) then

c        Not enough points generated to be a real contour
         nx=nx0
         ny=ny0
         xc=xc0
         yc=yc0
         edge=edge0
         nrcntrp=nrp
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(a10,i5,15x,i5,2f10.4)')
cxx     &         label,edge,nrcntrp,xc,yc
cxx         write(lwpcLOG_lun,'(a10,i5,5x,2i5,5x,2f10.4)')
cxx     &        'exit 99',edge,nx,ny,xc,yc
         RETURN
      else

c        Hopeless
         write(error_msg,
     &       '(''[GRF_CNTR_SEARCH]: '',a)') label
         write(lwpcLOG_lun,'(a)') error_msg
         call LWPC_ERROR ('ERROR',error_msg)
      end if
      END      ! GRF_CNTR_SEARCH