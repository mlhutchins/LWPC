      SUBROUTINE WF_QUARTC
     &          (fourb3,sixb2,fourb1,b0,q)

c     Finds the roots of a quartic polynomial from the closed form.

      implicit real*8 (a-h,o-z)

      character*200 error_msg
      real     *  8 mgplus,mgmnus
      complex  * 16 b3,b2,b1,b0,q,fourb3,sixb2,fourb1,b3sq,h,i,g,
     &              hprime,gprime,sqroot,pplus,p,cbert0,cbert1,cbert2,
     &              rootp,rootq,rootr

      dimension     q(4),part(2)

      equivalence  (p,part)


      b3=fourb3*0.25d0
      b2=sixb2/6.d0
      b1=fourb1*0.25d0
      b3sq=b3**2
      h=b2-b3sq
      i=b0-4.d0*b3*b1+3.d0*b2**2
      g=b1+b3*(-3.d0*b2+2.d0*b3sq)
      hprime=-i/12.d0
      gprime=-g**2/4.d0-h*(h**2+3.d0*hprime)
      sqroot=SQRT(gprime**2+4.d0*hprime**3)
      p=(-gprime+sqroot)*0.5d0
      mgplus=ABS(part(1))+ABS(part(2))
      pplus=p
      p=(-gprime-sqroot)*0.5d0
      mgmnus=ABS(part(1))+ABS(part(2))
      if (mgplus .gt. mgmnus) p=pplus
      cbert0=EXP(LOG(p)/3.d0)
      cbert1=cbert0*(-.5d0, .8660254038d0)
      cbert2=cbert0*(-.5d0,-.8660254038d0)
      rootp=SQRT(cbert0-hprime/cbert0-h)
      rootq=SQRT(cbert1-hprime/cbert1-h)
      rootr=SQRT(cbert2-hprime/cbert2-h)
      if (ABS(g) .gt. 1.d-30) then
         sign=-rootp*rootq*rootr*2.d0/g
         if (sign .lt. 0.d0) rootr=-rootr
      end if
      q(1)=+rootp+rootq+rootr-b3
      q(2)=+rootp-rootq-rootr-b3
      q(3)=-rootp+rootq-rootr-b3
      q(4)=-rootp-rootq+rootr-b3
      do n=1,4
         iter=0
         do while (iter .le. 10)
            rootp=(((q(n)+fourb3)*q(n)+sixb2)*q(n)+fourb1)*q(n)+b0
            rootq=((4.d0*q(n)+3.d0*fourb3)*q(n)+2.d0*sixb2)*q(n)+fourb1
            rootr=rootp/rootq
            q(n)=q(n)-rootr
            if (ABS(rootr) .lt. 1.d-10) then
               iter=99
            else
               iter=iter+1
            end if
         end do

         if (iter .eq. 11) then
             write(error_msg,
     &           '(''[WF_QUARTC]: '',
     &             ''q= '',1p2e13.5,'' fails to converge after'',i3,1x,
     &             ''iterations'')')
     &               q(n),iter
            call LWPC_ERROR ('Warning',error_msg)
         end if
      end do
      RETURN
      END      ! WF_QUARTC