      SUBROUTINE PLT_LWF_BORDER
     &          (xlngth,dscale,dmin,dmax,dtic1,dtic2,nxdec,
     &           ylngth,yscale,ymin,ymax,ytic1,ytic2,nydec,
     &           mxps,mxsgmnt,nrsgmnt,sgmnt)

      dimension     sgmnt(mxps,mxsgmnt)


c     Store current color
      call GRF_COLOR ('store')

c     Centered diamond
      ksymbl = 5

c     Draw border
      xmin =dmin*.001
      xmax =dmax*.001

      xtic1=dtic1*.001
      xtic2=dtic2*.001

      call GRF_BORDER (xlngth,xmin,xmax,xtic1,xtic2,nxdec,
     &                 ylngth,ymin,ymax,ytic1,ytic2,nydec)

c     Plot segmentation data; annotate the right side of the plot

c     Number of inches to the first tic mark
      v=ytic1/yscale
      do while (v .lt. .75)
         v=v+ytic1/yscale
      end do
      if (v .ge. 1.) then
         sz=.1
      else
         sz=v/10.
      end if

      call GRF_STRING (xlngth+.5*sz,    v,sz,'100',   0.,'LC',retX)
      call GRF_STRING (xlngth+.5*sz,.75*v,sz,' h''',  0.,'LC',retX)
      call GRF_STRING (xlngth+.5*sz,.50*v,sz,' 50',   0.,'LC',retX)
      call GRF_STRING (xlngth+.5*sz,.30*v,sz,' 1',    0.,'LC',retX)
      call GRF_STRING (xlngth+.5*sz,.15*v,sz,'log(s)',0.,'LC',retX)
      call GRF_STRING (xlngth+.5*sz,   0.,sz,'-5',    0.,'LC',retX)

      jmin=1
      do while (sgmnt(1,jmin) .lt. dmin)
         jmin=jmin+1
      end do
      jmax=nrsgmnt
      do while (sgmnt(1,jmax) .gt. dmax)
         jmax=jmax-1
      end do

c     Plot variation in h'
      call GRF_COLOR ('blue')
      scale=100./v
      xp=(sgmnt(1,jmin)-dmin)/dscale
      y1=sgmnt(10,jmin)/scale
      call GRF_MOVE (0.,y1)
      call GRF_DRAW (xp,y1)
      do j=jmin+1,jmax
         xp=(sgmnt(1,j)-dmin)/dscale
         y2=sgmnt(10,j)/scale
         call GRF_DRAW (xp,y1)
         call GRF_DRAW (xp,y2)
         y1=y2
      end do
      call GRF_DRAW (xlngth,y1)

c     Plot variation in ground conductivity
      call GRF_COLOR ('green')
      scale=6./(.3*v)
      xp=(sgmnt(1,jmin)-dmin)/dscale
      y1=(LOG10(sgmnt(7,jmin))+5.)/scale
      call GRF_MOVE (0.,y1)
      call GRF_DRAW (xp,y1)
      call GRF_SYMBOL (xp,y1,.03,ksymbl,0.,-1)

      do j=jmin+1,jmax
         xp=(sgmnt(1,j)-dmin)/dscale
         y2=(LOG10(sgmnt(7,j))+5.)/scale
         call GRF_DRAW (xp,y1)
         call GRF_DRAW (xp,y2)
         y1=y2
         call GRF_SYMBOL (xp,y1,.03,ksymbl,0.,-1)
      end do
      call GRF_DRAW (xlngth,y1)

      mc_flag=INT(sgmnt(11,jmin))/65536
      if (mc_flag .gt. 0) then

c        Plot index indicating full mode conversion:
c        mc_flag=1 for LW_FAST_MC
c        mc_flag=2 for LW_FULL_MC

         call GRF_COLOR ('purple')
         scale=100./v

c        First, draw a reference line
         y1=50./scale
         call GRF_MOVE (0.,y1)
         call GRF_DRAW (xlngth,y1)

c        Now, draw the line to indicate the value of MC_FLAG.
         xp=(sgmnt(1,jmin)-dmin)/dscale
         mc_flag=INT(sgmnt(11,jmin))/65536
         y1=(47+3*mc_flag)/scale
         call GRF_MOVE (0.,y1)
         call GRF_DRAW (xp,y1)
         do j=jmin+1,jmax
            xp=(sgmnt(1,j)-dmin)/dscale
            mc_flag=INT(sgmnt(11,j))/65536
            y2=(47+3*mc_flag)/scale
            call GRF_DRAW (xp,y1)
            call GRF_DRAW (xp,y2)
            y1=y2
         end do
         call GRF_DRAW (xlngth,y1)
      end if

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! PLT_LWF_BORDER