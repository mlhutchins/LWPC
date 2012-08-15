      SUBROUTINE OPA_INTERPOLATE
     &          (tlng,tclt,ctclt,stclt,
     &           rlng,rclt,crclt,srclt,
     &           mxpath,nrpath,beta,ibeta,
     &           mxpts,nrpts,nrcmp,
     &           dst_lwf,amp_rho,sig_rho,
     &           ampl,sigm,nout)

c Interpolates data from an array of signal amplitude and standard
c deviation vs. distance to get value at specified position

c***********************************************************************

c     tlng           transmitter longitude;  radians
c     tclt           transmitter colatitude; radians
c     ctclt          cosine(tclt)
c     stclt            sine(tclt)

c     rlng           receiver longitude;  radians
c     rclt           receiver colatitude; radians
c     crclt          cosine(rclt)
c     srclt            sine(rclt)

c     mxpath         maximum number of bearings
c     nrpath                 number of bearings
c     beta                   list   of bearings
c     ibeta                  index  of bearings

c     mxpts          maximum number of ranges
c     nrpts                  number of ranges
c     nrcmp          number of field components

c     dst_lwf

c     amp_rho
c     sig_rho

c     ampl           signal amplitude   at the point; dB
c     sigm           standard deviation at the point; dB

c     nout           number of points in the op area which are not
c                    covered by the spread of paths in the input data

c***********************************************************************

c  Change history:

c********************!**************************************************

      dimension      beta(*),ibeta(*),
     &               dst_lwf(mxpts),
     &               amp_rho(mxpts,2,mxpath),ampl(2),
     &               sig_rho(mxpts,2,mxpath),sigm(2)


c     Compute range and bearing to op area grid point
      dl=tlng-rlng
      if (ABS(dl) .gt. 3.1415926535898) dl=dl-SIGN(6.2831853071796,dl)
      call GCDBR2 (dl,tclt,ctclt,stclt,rclt,crclt,srclt,r,b,0)

      if (b .le. beta(1) .or. b .ge. beta(nrpath)) then
         n1=ibeta(nrpath)
         n2=ibeta(     1)
         b1= beta(nrpath)
         b2= beta(     1)+6.2831853071796
         if (b .le. beta(1)) b=b+6.2831853071796
      else
         jb=1
         do while (b .gt. beta(jb+1))
            jb=jb+1
         end do
         n1=ibeta(jb  )
         n2=ibeta(jb+1)
         b1= beta(jb  )
         b2= beta(jb+1)
      end if
      bs=(b-b1)/(b2-b1)

      nr=(r*6366.1977-dst_lwf(1))/dst_lwf(2)+1.
c     Check for grid point outside of radial coverage
      if (nr .lt. 1) then
         nr=1
         nout=nout+1
      else
     &if (nr .ge. nrpts) then
         nr=nrpts-1
         nout=nout+1
      end if
      rs=(r*6366.1977-dst_lwf(nr))/dst_lwf(2)

      a1=amp_rho(nr,1,n1)+(amp_rho(nr+1,1,n1)-amp_rho(nr,1,n1))*rs
      a2=amp_rho(nr,1,n2)+(amp_rho(nr+1,1,n2)-amp_rho(nr,1,n2))*rs
      ampl(1)=a1+(a2-a1)*bs

      s1=sig_rho(nr,1,n1)+(sig_rho(nr+1,1,n1)-sig_rho(nr,1,n1))*rs
      s2=sig_rho(nr,1,n2)+(sig_rho(nr+1,1,n2)-sig_rho(nr,1,n2))*rs
      sigm(1)=s1+(s2-s1)*bs

      if (nrcmp .gt. 1) then ! also store horizontal field

         a1=amp_rho(nr,2,n1)+(amp_rho(nr+1,2,n1)-amp_rho(nr,2,n1))*rs
         a2=amp_rho(nr,2,n2)+(amp_rho(nr+1,2,n2)-amp_rho(nr,2,n2))*rs
         ampl(2)=a1+(a2-a1)*bs

         s1=sig_rho(nr,2,n1)+(sig_rho(nr+1,2,n1)-sig_rho(nr,2,n1))*rs
         s2=sig_rho(nr,2,n2)+(sig_rho(nr+1,2,n2)-sig_rho(nr,2,n2))*rs
         sigm(2)=s1+(s2-s1)*bs

      end if
      RETURN
      END      ! OPA_INTERPOLATE