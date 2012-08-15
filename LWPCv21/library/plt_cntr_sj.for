      SUBROUTINE PLT_CNTR_SJ
     &          (mxlat,nrlat,mxlon,nrlon,
     &           amp1,std1,amp2,std2,
     &           icomp,iplot,ntlist,ntl,
     &           ampt,stdt)

      character*  4 temp

      dimension     amp1(mxlon,mxlat    ),std1(mxlon,mxlat    ),
     &              amp2(mxlon,mxlat    ),std2(mxlon,mxlat    ),
     &              ampt(mxlon,mxlat,2,5),stdt(mxlon,mxlat,2,5),
     &              ntlist(5),
     &              jammer(4)


c     Extract list of jammers
      write(temp,'(i4)') iplot
      read (temp,'(4i1)') jammer
c     Shift the list of jammers left in the array
      do while (jammer(1) .eq. 0)
         jammer(1)=jammer(2)
         jammer(2)=jammer(3)
         jammer(3)=jammer(4)
         jammer(4)=0
      end do

c     Count the number of jammers, set up the plot label and
c     sum the jammer powers at the transmitter
      nj=1
      ntl=1
      ntlist(ntl)=1
      do while (nj .le. 4 .and. jammer(nj) .ne. 0)
         ntl=ntl+1
         ntlist(ntl)=6-jammer(nj)
         nj=nj+1
      end do
      njr=nj-1

      nj=1
      nt=6-jammer(nj)
      call A_XFER_B (mxlat,nrlat,mxlon,nrlon,
     &               ampt(1,1,icomp,nt),stdt(1,1,icomp,nt),
     &               amp2(1,1         ),std2(1,1         ))

      do while (nj .lt. njr)
         nj=nj+1
         nt=6-jammer(nj)
         call A_PLUS_B (mxlat,nrlat,mxlon,nrlon,
     &                  ampt(1,1,icomp,nt),stdt(1,1,icomp,nt),
     &                  amp2(1,1         ),std2(1,1         ),
     &                  amp2(1,1         ),std2(1,1         ))
      end do

c     Get signal/jammer
      call A_OVER_B (mxlat,nrlat,mxlon,nrlon,
     &               ampt(1,1,icomp,1),stdt(1,1,icomp,1),
     &               amp2(1,1        ),std2(1,1        ),
     &               amp1(1,1        ),std1(1,1        ))

      RETURN
      END      ! PLT_CNTR_SJ