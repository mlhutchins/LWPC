      SUBROUTINE PLT_CNTR_SN
     &          (mxlat,nrlat,mxlon,nrlon,
     &           amp1,std1,
     &           icomp,iplot,ntlist,ntl,
     &           ampt,stdt,ampn,stdn)

      dimension     amp1(mxlon,mxlat    ),std1(mxlon,mxlat    ),
     &              ampt(mxlon,mxlat,2,5),stdt(mxlon,mxlat,2,5),
     &              ampn(mxlon,mxlat,2  ),stdn(mxlon,mxlat,2  ),
     &              ntlist(5)


      ntl=1
      ntlist(1)=iplot

c     Get signal/noise
      nt=iplot
      call A_OVER_B (mxlat,nrlat,mxlon,nrlon,
     &               ampt(1,1,icomp,nt),stdt(1,1,icomp,nt),
     &               ampn(1,1,icomp   ),stdn(1,1,icomp   ),
     &               amp1(1,1         ),std1(1,1         ))

      RETURN
      END      ! PLT_CNTR_SN