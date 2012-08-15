      SUBROUTINE WF_BNDY
     &          (isotropic,b)

c     Computes the vector b, which determines how to combine
c     the solution vectors in order to satisfy the boundary conditions.
c     This routine is valid only for eigen angles of the modal equation,
c     and is used to compute height gains.

      implicit real*8 (a-h,o-z)

c     Wave Fields
      complex  * 16 exc,dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld,
     &              c,s,p,dpdh,
     &              r11,r22,r12,r21,rbar11,rbar22

      common/wf_grnd/
     &              exc(3),dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld
     &      /wf_pmtx/
     &              c(2),s(2),p(4,2,2),dpdh(4,2,2)
     &      /wf_rmtx/
     &              r11,r22,r12,r21,rbar11,rbar22

c     Local
      character*200 error_msg
      complex  * 16 b(2),nurmf,denmf,
     &              ex1,ex2,ey1,ey2,hx1,hx2,hy1,hy2,
     &              fofr,numa,dena,hysum,abparl,abperp


      ex1= p(1,1,1)
      ex2= p(1,2,1)
      ey1=-p(2,1,1)
      ey2=-p(2,2,1)
      hx1= p(3,1,1)
      hx2= p(3,2,1)
      hy1= p(4,1,1)
      hy2= p(4,2,1)

      abparl=1.d0-rbar11*r11
      abperp=1.d0-rbar22*r22

      if (isotropic .eq. 0) then

c        Compute b:
c        non-isotropic case (from polarization Ey/Hy, see Budden).
         if (ABS(abparl) .lt. ABS(abperp)) then
            nurmf=(1.d0+rbar22)*rbar11*r21
            denmf=(1.d0+rbar11)*abperp
         else
            nurmf=(1.d0+rbar22)*abparl
            denmf=(1.d0+rbar11)*rbar22*r12
         end if
         fofr=nurmf/denmf
         numa=-ey1+fofr*hy1
         dena= ey2-fofr*hy2
         hysum=hy1+numa/dena*hy2
         b(1)=hy0/hysum
         b(2)=b(1)*numa/dena
         eyg=(ey1*b(1)+ey2*b(2))/ey0
      else

c        Compute b:
c        isotropic case (choose correctly polarized solution).
         temp a=ABS(abperp)
         temp b=ABS(abparl)
         if (temp a .lt. temp b) then
            b(1)=0.d0
            b(2)=ey0/ey2
            hyg=0.d0
         else
            b(1)=hy0/hy1
            b(2)=0.d0
            eyg=0.d0
         end if
         temp=temp a/temp b
         if (temp   .lt. 10.d0 .or. temp   .gt. 0.1d0 .or.
     &       temp b .gt. 0.1d0 .or. temp a .gt. 0.1d0) then
            write(error_msg,
     &          '(''[WF_BNDY]: '',
     &            ''Polarization values: '',
     &            ''abparl= '',1p2e13.5,'' abperp= '',1p2e13.5)')
     &              abparl,abperp
            call LWPC_ERROR ('Warning',error_msg)
         end if
      end if
      RETURN
      END      ! WF_BNDY