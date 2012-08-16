      SUBROUTINE GRF_BORDER
     &          (xlng,xmin,xmax,xtic1,xtic2,ndx,
     &           ylng,ymin,ymax,ytic1,ytic2,ndy)

c***********************************************************************

c  Function:
c     Draws a border with tic marks labeled

c  Parameters passed:
c     xlng          [r] length of the x axis; inches
c     xmin          [r] minimum value on the x axis; user units
c     xmax          [r] maximum value on the x axis; user units
c     xtic1         [r] primary tic mark interval; user units
c     xtic2         [r] label   tic mark interval; user units
c     ndx           [i] number of decimal places used to label axis
c     ylng          [r] length of the y axis; inches
c     ymin          [r] minimum value on the y axis; user units
c     ymax          [r] maximum value on the y axis; user units
c     ytic1         [r] primary tic mark interval; user units
c     ytic2         [r] label   tic mark interval; user units
c     ndy           [i] number of decimal places used to label axis

c     NOTE:  The number of decimal places of the axis labels is
c            controlled by <ndx> and <ndy>. A value of -1 indicates
c            whole number only.

c  Parameters returned:

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs
c     mod

c     grf_done
c     grf_draw
c     grf_move
c     grf_string

c  Entry:

c  References:

c  Change History:
c 2/16/10 MLH added the  parameter (null='z0') statement to repalce null/z0/
c 3/10/10 MLH changed it back

c*******************!***************************************************

      character*  1 null/' '/
c      parameter (null='z0')

      character*  8 frmt
      character* 40 number
      character* 41 string
      character*200 error_msg

      xscale=xlng/(xmax-xmin)
      yscale=ylng/(ymax-ymin)

      if (xtic1*xscale .le. 0. .or. xtic2*xscale .le. 0. .or.
     &    ytic1*yscale .le. 0. .or. ytic2*yscale .le. 0.) then

         write(error_msg,
     &       '(''[GRF_BORDER]: '',
     &         ''xaxis: lng min max tic1 tic2 nd='',1p5e11.3,i5,
     &         ''yaxis: lng min max tic1 tic2 nd='',1p5e11.3,i5)')
     &           xlng,xmin,xmax,xtic1,xtic2,ndx,
     &           ylng,ymin,ymax,ytic1,ytic2,ndy

         call GRF_DONE
         call LWPC_ERROR ('ERROR',error_msg)
      end if

c     Character height and vertical spacing
      chrht=.1
      chrsp=.15

      nx=ndx
      ny=ndy

      xtic=MOD(xtic2,xtic1)
      if (ABS(xtic) .lt. xtic1) xtic=xtic1
      xres=ABS(xtic)/100.

      ytic=MOD(ytic2,ytic1)
      if (ABS(ytic) .lt. ytic1) ytic=ytic1
      yres=ABS(ytic)/100.


c***  Draw the left hand border
      t1=.05
      t2=.10

      xtc1=xmin
      xtc2=xmin

      ytc1=ymin
      ytc2=ymin

      xx=0.
      yy=0.
      call GRF_MOVE (xx,yy)
      do yval=ymin,ymax,ytic

         yy=(yval-ymin)*yscale
         call GRF_DRAW (xx,yy)
         if (ABS(yval-ytc1) .le. yres) then
            call GRF_DRAW (t1,yy)
            ytc1=ytc1+ytic1
         end if
         if (ABS(yval-ytc2) .le. yres) then
            call GRF_DRAW (t2,yy)
            ytc2=ytc2+ytic2
         end if
         call GRF_DRAW (xx,yy)
      end do

c     Close the axis
      yval=ymax
      yy=ylng
      call GRF_DRAW (xx,yy)


c***  Draw top border with tic marks.
      t1=ylng-.05
      t2=ylng-.10

      do xval=xmin,xmax,xtic
         xx=(xval-xmin)*xscale
         call GRF_DRAW (xx,yy)
         if (ABS(xval-xtc1) .le. xres) then
            call GRF_DRAW (xx,t1)
            xtc1=xtc1+xtic1
         end if
         if (ABS(xval-xtc2) .le. xres) then
            call GRF_DRAW (xx,t2)
            xtc2=xtc2+xtic2
         end if
         call GRF_DRAW (xx,yy)
      end do

c     Close the axis
      xval=xmax
      xx=xlng
      call GRF_DRAW (xx,yy)


c***  Draw the bottom of the border;
c     duplicate the tic mark sequence of the upper border.
      t1=.05
      t2=.10

      xtc1=xmin
      xtc2=xmin

      ytc1=ymin
      ytc2=ymin

      xx=0.
      yy=0.
      call GRF_MOVE (xx,yy)
      do xval=xmin,xmax,xtic

         xx=(xval-xmin)*xscale
         call GRF_DRAW (xx,yy)
         if (ABS(xval-xtc1) .le. xres) then
            call GRF_DRAW (xx,t1)
            xtc1=xtc1+xtic1
         end if
         if (ABS(xval-xtc2) .le. xres) then
            call GRF_DRAW (xx,t2)
            xtc2=xtc2+xtic2
         end if
         call GRF_DRAW (xx,yy)
      end do

c     Close the axis
      xval=xmax
      xx=xlng
      call GRF_DRAW (xx,yy)


c***  Draw right hand border;
c     duplicate tic mark sequence of the left hand side.
      t1=xlng-.05
      t2=xlng-.10

      do yval=ymin,ymax,ytic
         yy=(yval-ymin)*yscale
         call GRF_DRAW (xx,yy)
         if (ABS(yval-ytc1) .le. yres) then
            call GRF_DRAW (t1,yy)
            ytc1=ytc1+ytic1
         end if
         if (ABS(yval-ytc2) .le. yres) then
            call GRF_DRAW (t2,yy)
            ytc2=ytc2+ytic2
         end if
         call GRF_DRAW (xx,yy)
      end do

c     Close the axis
      yval=ymax
      yy=ylng
      call GRF_DRAW (xx,yy)


c***  Annotate the major tic marks of the left hand border.
      t1=.05
      t2=.10

      xtc1=xmin
      xtc2=xmin

      ytc1=ymin
      ytc2=ymin

      if (ny .eq. -1) then

c        Integer labels
         write(frmt,'(''(i40)'')')
      else

c        Floating point labels
         if (ny .lt. 9) then
            write(frmt,'(''(f40.'',i1,'')'')') ny
         else
            write(frmt,'(''(f40.'',i2,'')'')') ny
         end if
      end if

      xx=0.
      yy=0.
      xl=-.5*chrsp
      do yval=ymin,ymax,ytic

         yy=(yval-ymin)*yscale
         if (ABS(yval-ytc2) .le. yres) then

c           Annotate Y axis
            number=null
            if (ny .eq. -1) then
               write(number,frmt) INT(yval)
            else
               write(number,frmt) yval
            end if
            string=number//null
            nd=40
            nc=1
            do while (nd .gt. 1 .and. number(nd-1:nd-1) .ne. ' ')
               nd=nd-1
               nc=nc+1
            end do
            call GRF_STRING (xl,yy,chrht,string(nd:41),0.,'RC',retX)
            call GRF_MOVE   (xx,yy)

            ytc2=ytc2+ytic2
         end if
      end do

c     Close the axis
      yval=ymax
      yy=ylng
      if (ABS(yval-ytc2) .le. yres) then

c        Annotate Y axis
         number=null
         if (ny .eq. -1) then
            write(number,frmt) INT(yval)
         else
            write(number,frmt) yval
         end if
         string=number//null
         nd=40
         nc=1
         do while (nd .gt. 1 .and. number(nd-1:nd-1) .ne. ' ')
            nd=nd-1
            nc=nc+1
         end do
         call GRF_STRING (xl,yy,chrht,string(nd:41),0.,'RC',retX)
         call GRF_MOVE   (xx,yy)
      end if


c***  Annotate the major tic marks of the bottom border.
      t1=.05
      t2=.10

      xtc1=xmin
      xtc2=xmin

      ytc1=ymin
      ytc2=ymin

      if (nx .eq. -1) then

c        Integer labels
         write(frmt,'(''(i40)'')')
      else

c        Floating point labels
         if (nx .lt. 9) then
            write(frmt,'(''(f40.'',i1,'')'')') nx
         else
            write(frmt,'(''(f40.'',i2,'')'')') nx
         end if
      end if

      xx=0.
      yy=0.
      yl=-chrsp
      do xval=xmin,xmax,xtic

         xx=(xval-xmin)*xscale
         if (ABS(xval-xtc2) .le. xres) then

c           Annotate X axis
            number=null
            if (nx .eq. -1) then
               write(number,frmt) INT(xval)
            else
               write(number,frmt) xval
            end if
            string=number//null
            nd=40
            nc=1
            do while (nd .gt. 1 .and. number(nd-1:nd-1) .ne. ' ')
               nd=nd-1
               nc=nc+1
            end do
            call GRF_STRING (xx,yl,chrht,string(nd:41),0.,'CC',retX)
            call GRF_MOVE   (xx,yy)

            xtc2=xtc2+xtic2
         end if
      end do

c     Close the axis
      xval=xmax
      xx=xlng
      if (ABS(xval-xtc2) .le. xres) then

c        Annotate X axis
         number=null
         if (nx .eq. -1) then
            write(number,frmt) INT(xval)
         else
            write(number,frmt) xval
         end if
         string=number//null
         nd=40
         nc=1
         do while (nd .gt. 1 .and. number(nd-1:nd-1) .ne. ' ')
            nd=nd-1
            nc=nc+1
         end do
         call GRF_STRING (xx,yl,chrht,string(nd:41),0.,'CC',retX)
         call GRF_MOVE   (xx,yy)
      end if

      RETURN
      END      ! GRF_BORDER