      SUBROUTINE MF_QUARTC
     &          (b4,four b3,six b2,four b1,one b0,q)

c     Solution for the roots of a fourth-order polynomial (quartic
c     equation) taken from Burnside and Panton (1904), The Theory of
c     Equations.  A summary of pertinent equations is given in Radio
c     Science Vol. 3, Aug. 1968, pp.792-795.

      character*200 error_msg
      real          mag pos,mag neg
      complex       b4,four b3,six b2,four b1,one b0,q,
     &              h,i,g,h prime,g prime,
     &              sq root,p pos,p,log p,cub rt0,cub rt1,cub rt2,
     &              omega1,omega2,root p,root q,root r,twlv b3,twlv b2,
     &              f,dfdq,del q,fb4
      complex  * 16 b3,b2,b1,b0,b3 sq

      dimension     p ri(2),q(4)

      equivalence  (p ri(1),p)

      data          omega1/(-0.5, 0.8660254038)/,
     &              omega2/(-0.5,-0.8660254038)/


      b3=four b3/(4.*b4)
      b2=six b2/(6.*b4)
      b1=four b1/(4.*b4)
      b0=one b0/b4

      b3 sq=b3**2
      h=b2-b3 sq
      i=b0-4.*b3*b1+3.*b2**2
      g=b1+b3*(-3.*b2+2.*b3 sq)
      h prime=-i/12.
      g prime=-g**2*.25-h*(h**2+3.*h prime)

      sq root=SQRT(g prime**2+4.*h prime**3)
      p=(-g prime+sq root)*0.5
      mag pos=ABS(p ri(1))+ABS(p ri(2))
      p pos=p
      p=(-g prime-sq root)*0.5
      mag neg=ABS(p ri(1))+ABS(p ri(2))
      if (mag pos .gt. mag neg) p=p pos
      log p=LOG(p)
      cub rt0=EXP(log p/3.)
      cub rt1=omega1*cub rt0
      cub rt2=omega2*cub rt0

      root p=SQRT(cub rt0-h prime/cub rt0-h)
      root q=SQRT(cub rt1-h prime/cub rt1-h)
      root r=SQRT(cub rt2-h prime/cub rt2-h)
      if (ABS(g) .ge. 1.e-20) then
         sign=-root p*root q*root r*2./g
         if (sign .lt. 0.) root r=-root r
      end if
      q(1)=+root p+root q+root r-b3
      q(2)=+root p-root q-root r-b3
      q(3)=-root p+root q-root r-b3
      q(4)=-root p-root q+root r-b3

c     Newton-Raphson (first order) iterative improvement.
      fb4=4.*b4
      twlv b3=3.*four b3
      twlv b2=2.*six b2
      do j=1,4
         dlqmin=9.9e9
         do jj=1,4
            if (jj .ne. j) then
               dlq=ABS(q(j)-q(jj))
               if (dlq .lt. dlqmin) dlqmin=dlq
             end if
         end do
         dlqmax=dlqmin/3.

         last=0
         ncount=1
         do while (last .eq. 0 .and. ncount .le. 10)
            f=(((b4*q(j)+four b3)*q(j)+six b2)*q(j)+four b1)*q(j)+one b0
            dfdq=((fb4*q(j)+twlv b3)*q(j)+twlv b2)*q(j)+four b1
            del q=-f/dfdq
            if (ABS(delq) .gt. dlqmax) delq=delq*dlqmax/ABS(delq)
            q(j)=q(j)+del q
            if (ABS(del q/q(j)) .lt. 1.e-4) last=1
            ncount=ncount+1
         end do
         if (ABS(del q/q(j)) .gt. 1.e-2) then
             write(error_msg,
     &           '(''[MF_QUARTC]: q='',4(1pe13.3,e11.3))') q
             call LWPC_ERROR ('WARNING',error_msg)
             write(error_msg,
     &            '(''[MF_QUARTC]: q fails to converge'')')
             call LWPC_ERROR ('ERROR',error_msg)
         end if
      end do
      RETURN
      END      ! MF_QUARTC