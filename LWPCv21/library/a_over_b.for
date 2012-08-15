      SUBROUTINE A_OVER_B
     &          (mxlat,nrlat,mxlon,nrlon,
     &           amp1,sig1,amp2,sig2,amp3,sig3)

c     Obtains amp1/amp2 and SQRT(sig1**2+sig2**2) assumming all
c     parameters are in dB.

      dimension     amp1(mxlon,mxlat),sig1(mxlon,mxlat),
     &              amp2(mxlon,mxlat),sig2(mxlon,mxlat),
     &              amp3(mxlon,mxlat),sig3(mxlon,mxlat)


      do l=1,nrlat
         do k=1,nrlon
            amp3(k,l)=amp1(k,l)-amp2(k,l)
            sig3(k,l)=SQRT(sig1(k,l)**2+sig2(k,l)**2)
         end do
      end do

      RETURN
      END      ! A_OVER_B