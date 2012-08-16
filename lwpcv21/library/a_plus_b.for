      SUBROUTINE A_PLUS_B
     &          (mxlat,nrlat,mxlon,nrlon,
     &           amp1,sig1,amp2,sig2,amp3,sig3)

c     Adds signal powers and computes combined standard deviation by
c     adding the separate standard deviations weighted by their powers.
c     Assumes inputs are all in dB.

      dimension     amp1(mxlon,mxlat),sig1(mxlon,mxlat),
     &              amp2(mxlon,mxlat),sig2(mxlon,mxlat),
     &              amp3(mxlon,mxlat),sig3(mxlon,mxlat)


      do l=1,nrlat
         do k=1,nrlon
            pwr1=10.**(amp1(k,l)/10.)
            pwr2=10.**(amp2(k,l)/10.)
            amp3(k,l)=10.*LOG10(pwr1+pwr2)
            sig3(k,l)=SQRT((sig1(k,l)**2*pwr1+sig2(k,l)**2*pwr2)
     &                    /(pwr1+pwr2))
         end do
      end do

      RETURN
      END      ! A_PLUS_B