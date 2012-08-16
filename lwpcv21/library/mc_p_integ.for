      FUNCTION   MC_P_INTEG
     &          (delht,nrht,prod,err)

      IMPLICIT REAL*8 (a-h,o-z)

      character*200 error_msg
      complex  * 16 MC_P_INTEG,
     &              prod(nrht),t(6),ta,tb


      ix=nrht-1
      iy=ix/2
      if (ix .ne. 2*iy) then
           write(error_msg,
     &         '(''[MC_P_INTEG]: '',
     &           ''NRHT is not 1 + a power of 2'')')
           call LWPC_ERROR ('Error',error_msg)
      end if

      index=1
      ht=delht*DFLOAT(ix)
      t(1)=0.5d0*(prod(1)+prod(ix+1))*ht

      m=1
      do i=1,7
         if (ix .gt. 1) then
            ht=ht*0.5d0
            iy=ix/2
            ix=iy
            ta=(0.d0,0.d0)
            do j=1,index
               jj=(2*j-1)*ix
               ta=ta+prod(jj+1)
            end do
            ta=ta*ht+t(1)*0.5d0
            index=index*2

            m=i
            if (m .gt. 5) m=5
            power=1.d0
            do j=1,m
               power=power*4.d0
               tb=ta-t(j)
               t(j)=ta
               ta=ta+tb/DCMPLX(power-1.d0,0.d0)
            end do
            t(m+1)=ta
         end if
      end do
      err=CDABS(t(m+1)-t(m))
      MC_P_INTEG=t(m+1)

      RETURN
      END      ! MC_P_INTEG