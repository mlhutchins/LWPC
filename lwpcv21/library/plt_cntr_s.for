      SUBROUTINE PLT_CNTR_S
     &          (mxlat,nrlat,mxlon,nrlon,
     &           amp1,std1,
     &           icomp,iplot,ntlist,ntl,
     &           ampt,stdt)

      dimension     amp1(mxlon,mxlat    ),std1(mxlon,mxlat    ),
     &              ampt(mxlon,mxlat,2,5),stdt(mxlon,mxlat,2,5),
     &              ntlist(5)


      ntl=1
      nt=iplot
      ntlist(1)=iplot

      do l=1,nrlat
         do k=1,nrlon
            amp1(k,l)=ampt(k,l,icomp,nt)
            std1(k,l)=stdt(k,l,icomp,nt)
         end do
      end do

      RETURN
      END      ! PLT_CNTR_S