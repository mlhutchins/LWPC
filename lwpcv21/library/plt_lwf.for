      SUBROUTINE PLT_LWF
     &          (print_lwf,phs_plot,phs_conv,color,line,
     &           plt_label,mxlbl,nrlbl,
     &           dmin,dscale,
     &           ymin,ymax,yscale,
     &           mxpts,nrcmp,nxmin,nxmax,
     &           dst,amp,phs)

c***********************************************************************
c  Subroutine       plt_lwf
c***********************************************************************
c
c  Program Source:
c     Naval Ocean Systems Center - Code 542
c
c  Date:
c     26 Jan 1996
c
c  Function:
c     Plots and prints signal parameters as functions of distance.
c
c  Parameters passed:
c     print_lwf     [i] print flag
c     phs_plot      [l] phase plot flag
c     phs_conv      [r] phase conversion (1 or degrees to microseconds)
c
c     color         [s] color to use for the current curve
c     line          [i] line type to use
c
c     plt_label     [s] array of data labels
c     mxlbl         [i] dimension of PLT_LABEL
c     nrlbl         [i] number    of PLT_LABELs
c
c     dmin          [r] range axis minimum
c     dscale        [r] scale  of x axis
c
c     ymin          [r] axis minimum
c     ymax          [r] axis maximum
c     yscale        [r] scale  of y axis
c
c     mxpts         [i] maximum number of points along path
c     nrcmp         [i] number of field components
c     nxmin         [i] first point to be plotted along the path
c     nxmax         [i] last  point to be plotted along the path
c
c     dst           [r] array of distances
c     amp           [r] amplitude of signal
c     phs           [r] relative phase of signal
c
c
c  Parameters returned:
c     None
c
c  Common blocks referenced:
c     lwpc_cfg
c     lwpc_lun
c
c  Functions and subroutines referenced:
c     None
c
c  References:
c     None
c
c  Change History:
c     11 Nov 1993   Dropped PLT_UNIT for conversion to OS/2.

c     09 Feb 1994   Changed PLT_DEVICE and ORIENTATION to CHARACTER*(*).

c     30 Jun 1994   Dropped NRPS, RHOMX, RLAT, RLON and RRHO from
c                   argument list.

c     27 Oct 1995   Changed to use lwpc_LUN.cmn to set logical units.

c     07 Dec 1995   Changes to allow plot of phase.

c     25 Jan 1996   Moved call to PLT_LWF_BORDER, GRF_BEGIN and
c                   GRF_ORIGIN to the calling routine; many mods to
c                   the argument list to support these changes.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_cfg.cmn'
      include      'lwpc_lun.cmn'

      character*(*) color,
     &              plt_label(mxlbl)

      character*  4 label
      logical       phs_plot
      integer       print_lwf
      real          phs_conv

      dimension     dst(mxpts),amp(mxpts,3),phs(mxpts,3)

      data          label/'zyx '/


      if (print_lwf .gt. 0) then

c        Print the field strength and relative phase
         write(lwpcLOG_lun,'(/)')
         do n=1,nrlbl
            write(lwpcLOG_lun,'(a)') plt_label(n)
         end do
         nl=(nxmax-nxmin)/3+1
         do nc=1,nrcmp
            write(lwpcLOG_lun,
     &          '(/a,'' component''/
     &          3(''  dist   amplitude  phase  ''))')
     &           label(nc:nc)
            if (amp(1,nc) .ge. 100.) amp(1,nc)=99.99
            do i1=nxmin,nl
               write(lwpcLOG_lun,
     &             '(3(f7.0,2f10.4))')
     &              (dst(i),amp(i,nc),phs(i,nc),i=i1,nxmax,nl)
            end do
         end do
      end if

c     Store current color
      call GRF_COLOR ('store')

c     Set the color
      call GRF_COLOR (color)

      if (.not.phs_plot) then

         do nc=1,nrcmp
            do i=nxmin,nxmax
               if (amp(i,nc) .gt. ymax) amp(i,nc)=ymax
               if (amp(i,nc) .lt. ymin) amp(i,nc)=ymin
            end do
            call GRF_CURVE
     &          (dst(nxmin),amp(nxmin,nc),nxmax-nxmin+1,
     &           dmin,ymin,dscale,yscale,line,20)
         end do
      else

         do nc=1,nrcmp
            cycle=0.
            do i=nxmin,nxmax
               phase=phs(i,nc)*phs_conv+cycle
               do while (phase .lt. ymin .or. phase .gt. ymax)
                  if (phase .gt. ymax) then
                     if (ymax .eq. 0.) then
                        cycle=cycle+ymin
                        phase=phase+ymin
                     else
                        cycle=cycle-ymax
                        phase=phase-ymax
                     end if
                  end if
                  if (phase .lt. ymin) then
                     if (ymin .eq. 0.) then
                        cycle=cycle+ymax
                        phase=phase+ymax
                     else
                        cycle=cycle-ymin
                        phase=phase-ymin
                     end if
                  end if
               end do
               phs(i,nc)=phase
            end do
            call GRF_CURVE
     &          (dst(nxmin),phs(nxmin,nc),nxmax-nxmin+1,
     &           dmin,ymin,dscale,yscale,line,20)
         end do
      end if

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! PLT_LWF