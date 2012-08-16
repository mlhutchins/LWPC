      SUBROUTINE PLT_CNTR_N
     &          (mxlat,nrlat,mxlon,nrlon,
     &           amp1,std1,
     &           icomp,iplot,
     &           ampn,stdn)

      dimension     amp1(mxlon,mxlat  ),std1(mxlon,mxlat  ),
     &              ampn(mxlon,mxlat,2),stdn(mxlon,mxlat,2)


      if (iplot .eq. 0) RETURN

      ntl=0

      do l=1,nrlat
         do k=1,nrlon
            amp1(k,l)=ampn(k,l,icomp)
            std1(k,l)=stdn(k,l,icomp)
         end do
      end do

      RETURN
      END      ! PLT_CNTR_N