      SUBROUTINE A_XFER_B
     &          (mxlat,nrlat,mxlon,nrlon,
     &           amp1,sig1,amp2,sig2)

c     Transfers amp1 and sig1 to amp2 and sig2

      dimension     amp1(mxlon,mxlat),sig1(mxlon,mxlat),
     &              amp2(mxlon,mxlat),sig2(mxlon,mxlat)


      do l=1,nrlat
         do k=1,nrlon
            amp2(k,l)=amp1(k,l)
            sig2(k,l)=sig1(k,l)
         end do
      end do

      RETURN
      END      ! A_XFER_B