      SUBROUTINE PLT_LWF_AVG
     &          (mxpts,nrpts,nrcmp,ampl,mxavg)

c     mxpts         dimension of        amplitude array
c     nrpts         number of points in amplitude array
c     nrcmp         number of field components in amplitude array
c     ampl          array of signal amplitudes
c     mxavg         number of points over which to do the running
c                   average

      real          ampl(mxpts,3)

      real          amp(0:50,3),sum(3)


      if (mxavg .gt. 51)
     &   call LWPC_ERROR ('Error','[PLT_LWF_AVG]: MXAVG gt 51')

      if (mxavg .gt. 1) then

c        Store smoothed signal amplitude for current radial
         nravg=0
         do nc=1,nrcmp
            sum(nc)=0.
         end do

c        Loop over all ranges
         do i=2,nrpts

            nravg=nravg+1
            if (nravg .le. mxavg) then

c              Store the first set of points and do the average over
c              the number of points processed
               ma=MOD(nravg,mxavg)
               do nc=1,nrcmp

                  amp(ma,nc)=ampl(i,nc)
                  sum(nc)=sum(nc)+amp(ma,nc)
                  ampl(i,nc)=sum(nc)/nravg
               end do
            else

c              Store the running average over MXAVG points
               ma=MOD(nravg,mxavg)
               na=ma-mxavg
               if (na .lt. 0) na=na+mxavg
               do nc=1,nrcmp

c                 Remove oldest value from the sum
                  sum(nc)=sum(nc)-amp(na,nc)

c                 Add newest value to the sum
                  amp(ma,nc)=ampl(i,nc)
                  sum(nc)=sum(nc)+amp(ma,nc)
                  ampl(i-mxavg/2+1,nc)=sum(nc)/mxavg
               end do
            end if
         end do

c        Finish averaging to the end of the path
         na=nravg
         nravg=mxavg
         do i=nrpts-mxavg/2+1,nrpts

            nravg=nravg-1
            na=na+1
            ma=MOD(na,mxavg)
            if (ma .lt. 0) ma=ma+mxavg
            do nc=1,nrcmp

c              Remove oldest value from the sum
               sum(nc)=sum(nc)-amp(ma,nc)
               ampl(i,nc)=sum(nc)/nravg
            end do
         end do
      end if

      RETURN
      END      ! PLT_LWF_AVG