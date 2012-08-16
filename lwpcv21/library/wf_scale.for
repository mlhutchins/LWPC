      SUBROUTINE WF_SCALE
     &          (index)

c     Scales and orthogonalizes the solution vectors P. This scaling
c     must later be removed to yield correct (unscaled) solutions.

      implicit real*8 (a-h,o-z)

c     Wave Fields
      real     *  8 p sav,a norm,b norm,ht,wfht1,wfht2
      complex  * 16 c,s,p,dpdh,
     &              m31 sav,m32 sav,m33 sav,ortho

      common/wf_pmtx/
     &              c(2),s(2),p(4,2,2),dpdh(4,2,2)
     &      /wf_save/
     &              p sav(16),
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              a norm,b norm,ht,wfht1,wfht2,
     &              levl
     &      /wf_petc/
     &              p etc(27,65)

c     Local
      complex  * 16 term


      a term=0.d0
      do j=1,4
         a term=a term+ABS(p(j,1,index))**2
      end do
      term=0.d0
      do j=1,4
         term=term+CONJG(p(j,1,index))*p(j,2,index)
      end do
      term=term/a term
      do j=1,4
         p(j,2,index)=p(j,2,index)-term*p(j,1,index)
      end do
      b term=0.d0
      do j=1,4
         b term=b term+ABS(p(j,2,index))**2
      end do
      a term=1.d0/SQRT(a term)
      b term=1.d0/SQRT(b term)
      do j=1,4
         p(j,1,index)=p(j,1,index)*DCMPLX(a term,0.d0)
         p(j,2,index)=p(j,2,index)*DCMPLX(b term,0.d0)
      end do
      if (index .eq. 1) then
         ortho=ortho+term*DCMPLX(a norm/b norm,0.d0)
         a norm=a norm*a term
         b norm=b norm*b term
      end if
      RETURN
      END      ! WF_SCALE