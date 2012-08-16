      SUBROUTINE PLT_CNTR_RANGE
     &          (amp,mxlat,nrlat,mxlon,nrlon,
     &           ampinc,ampmin,ampmax)

c Determines the range of contour levels found in input array

c Change History

c*******************!***************************************************

      dimension     amp(mxlon,mxlat)


c     Get data range
      ampmin=amp(1,1)
      ampmax=amp(1,1)
      do l=2,nrlat
         do k=2,nrlon
            ampmin=AMIN1(ampmin,amp(k,l))
            ampmax=AMAX1(ampmax,amp(k,l))
         end do
      end do

      ampmin=INT(ampmin/ampinc-.9)*ampinc
      if (ampmin .gt. 0.) ampmin=ampmin+ampinc

      ampmax=INT(ampmax/ampinc+.9)*ampinc
      if (ampmax .lt. 0.) ampmax=ampmax-ampinc

      RETURN
      END      ! PLT_CNTR_RANGE