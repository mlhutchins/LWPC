      SUBROUTINE GRF_CNTR_CELL
     &          (v,mxx,mxy,nrx,vc,nc,
     &           en,xen,yen,ien,jen,
     &           nex,ex,xex,yex,label)

c***********************************************************************

c  Function:
c     This routine searches the edges of a cell to find all of the the
c     exits.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     v            [r,mxx,mxy]   array containing values for contours
c     mxx          [i]           X dimension of V in calling program
c     mxy          [i]           Y dimension of V in calling program
c     nrx          [i]           number of elements of V along X
c     vc           [r,2]         definition of the contour band
c     nc           [i]           index of the contour level to search;
c                                =1: VC(1); =2: VC(2)
c     en           [i]           edge         of the initial crossing
c     xen          [r]           X coordinate of the initial crossing
c     yen          [r]           Y coordinate of the initial crossing
c     ien          [i]           X index      of the initial crossing
c     jen          [i]           Y index      of the initial crossing

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}
c     nex          [i]           number of exits from the cell
c     ex           [i]           edge         of the final crossing
c     xex          [r]           X coordinate of the final crossing
c     yex          [r]           Y coordinate of the final crossing
c     label        [s]           debugging label

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:
c     abs

c     grf_cntr_flag

c  References:

c  Change History:

c*******************!***************************************************

      IMPLICIT      NONE

      include      'grf_cntr.cmn'
      include      'lwpc_lun.cmn'

cxxcDEBUG
cxx      logical       edge1
cxx      logical       edge2
cxx      logical       edge3
cxx      logical       edge4

      character* 10 label

      logical       grf_cntr_flag      ! Function
      logical       tst

      integer       ecx(3)
      integer       en
      integer       ex
      integer       ien
      integer       ix(5)
      integer       jen
      integer       jx(5)
      integer       mxx
      integer       mxy
      integer       nc
      integer       ne
      integer       nex
      integer       next
      integer       nrx

      real          dvx
      real          vc(2)
      real          vcx
      real          vn
      real          v(mxx,mxy)
      real          vx(5)
      real          xcx(3)
      real          xen
      real          xex
      real          ycx(3)
      real          yen
      real          yex

      data          tst/.false./

c     Set up search of the edges of the current cell
      ix(1)=ien
      ix(2)=ien+1
      ix(3)=ien+1
      ix(4)=ien

      jx(1)=jen
      jx(2)=jen
      jx(3)=jen+1
      jx(4)=jen+1

      vx(1)=v(ix(1),jx(1))
      vx(2)=v(ix(2),jx(2))
      vx(3)=v(ix(3),jx(3))
      vx(4)=v(ix(4),jx(4))

      ix(5)=ix(1)
      jx(5)=jx(1)
      vx(5)=vx(1)

c     Contour value for search of the current cell
      vcx=vc(nc)
      if (nc .eq. 1) then
         dvx=-.001
      else
         dvx= .001
      end if
      if (vx(1) .eq. vcx) vx(1)=vx(1)+dvx
      if (vx(2) .eq. vcx) vx(2)=vx(2)+dvx
      if (vx(3) .eq. vcx) vx(3)=vx(3)+dvx
      if (vx(4) .eq. vcx) vx(4)=vx(4)+dvx
      if (vx(5) .eq. vcx) vx(5)=vx(5)+dvx

cxxcDEBUG
cxx      edge1=GRF_CNTR_FLAG(cells(1,nc),nrx,tst,ien,jen,1)
cxx      edge2=GRF_CNTR_FLAG(cells(1,nc),nrx,tst,ien,jen,2)
cxx      edge3=GRF_CNTR_FLAG(cells(1,nc),nrx,tst,ien,jen,3)
cxx      edge4=GRF_CNTR_FLAG(cells(1,nc),nrx,tst,ien,jen,4)
cxx
cxx      write(lwpcLOG_lun,'(a10,i5,3f10.4    )')
cxx     &     'enter cell',jx(4),vx(4),vx(3),vcx
cxx      write(lwpcLOG_lun,'(10x,i5,2f10.4,4l5)')
cxx     &      jx(1),vx(1),vx(2),edge1,edge2,edge3,edge4
cxx      write(lwpcLOG_lun,'(10x,i5,2i10      )')
cxx     &      en,ix(1),ix(2)

c     Average value for the cell
      vn=(vx(1)+vx(2)+vx(3)+vx(4))/4.

c     Check edges in a counter-clockwise order from the entrance
      nex=0
      ecx(1)=en
      xcx(1)=xen
      ycx(1)=yen
      do ne=en+1,en+3

         if (ne .le. 4) then
            next=ne
         else
            next=ne-4
         end if

         if (.not.GRF_CNTR_FLAG
     &           (cells(1,nc),nrx,tst,ien,jen,next)) then

            if (vx(next) .lt. vcx .and. vcx .le. vx(next+1) .or.
     &          vx(next) .le. vcx .and. vcx .lt. vx(next+1) .or.
     &          vx(next) .gt. vcx .and. vcx .ge. vx(next+1) .or.
     &          vx(next) .ge. vcx .and. vcx .gt. vx(next+1)) then

               nex=nex+1
               ecx(nex)=next
               dvx=vx(next+1)-vx(next)
               if (dvx .ne. 0.) then
                  xcx(nex)=ix(next)+(ix(next+1)-ix(next))*
     &                              (vcx-vx(next))/dvx
                  ycx(nex)=jx(next)+(jx(next+1)-jx(next))*
     &                              (vcx-vx(next))/dvx
               else
                  if (vcx .eq. vx(next)) then
                     xcx(nex)=ix(next)
                     ycx(nex)=jx(next)
                  else
                     xcx(nex)=ix(next+1)
                     ycx(nex)=jx(next+1)
                  end if
               end if
cxxcDEBUG
cxx               write(lwpcLOG_lun,'(a10,i5,2f10.4,i5)')
cxx     &              'ex',next,xcx(nex),ycx(nex),nex
            end if
         end if
      end do
      write(label,'(i1,''exit cell'')') nex

      if (nex .eq. 0) RETURN

      ne=1
      if (nex .gt. 1) then

c        There are multiple exits from this cell;
c        choose the edge which keeps the part of the array with
c        values in the direction of the other contour line on
c        the right as we trace the curve
         if (vx(en+1) .gt. vx(en)) then

c           Turn left
            ne=nex
         else
c           Turn right
            ne=1
         end if
      end if

c     Set exit parameters
      ex =ecx(ne)
      xex=xcx(ne)
      yex=ycx(ne)

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,i5,2f10.4,i5)')
cxx     &     'exit',ex,xex,yex,ne

      RETURN
      END      ! GRF_CNTR_CELL