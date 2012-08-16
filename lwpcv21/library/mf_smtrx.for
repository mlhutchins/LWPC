      SUBROUTINE MF_SMTRX

c     Computation of coefficients used in the differential equations
c     for (r+1)/c.  These are stored in common area S MTX. Derivatives
c     of these coefficients wrt  c=COS(THETA) are computed if IDERIV is
c     set non-zero. The coefficients are derived from the differential
c     equation for the reflection coeffieient matrix given by Budden in
c     Proc. Roy. Soc. (london), A227, pp. 516-537 (1955).

      implicit complex (a-h,o-z)

      real           omega,wavenr

      common/mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_smtx/a11,a22,b11,b12,b22,c11,c21,c22,d11,d21,d12,d22,
     &               db11,db22,dc11,dc22,dd11,dd21,dd12
     &      /mf_tmtx/t11ovs,t44ovs,t12ovs,t34ovs,t14a,t31,t42,t32mc2,
     &               t41m1


      ds=-c/s
      dcs=s-csq/s

      call MF_TMTRX

      a11= 4.0*t41m1+(4.,0.)
      a22= (4.,0.)
      b11= 2.0*(s*t44ovs-c*t41m1)-(2.,0.)*c
      b12=-2.0*t42
      b22=-(2.,0.)*c
      c11=-2.0*(s*t11ovs+c*t41m1)-(2.,0.)*c
      c21= 2.0*t31
      c22=-(2.,0.)*c
      d11= c*s*(t11ovs-t44ovs)+csq*t41m1-ssq*t14a
      d21=-c*t31+s*t34ovs
      d12= s*t12ovs+c*t42
      d22=-t32mc2

      if (ideriv .eq. 1) then
         db11= 2.0*(ds*t44ovs-t41m1)-(2.,0.)
         db22=-(2.,0.)
         dc11=-2.0*(ds*t11ovs+t41m1)-(2.,0.)
         dc22=-(2.,0.)
         dd11= dcs*(t11ovs-t44ovs)+2.0*c*(t41m1+t14a)
         dd21=-t31+ds*t34ovs
         dd12= ds*t12ovs+t42
      end if
      RETURN
      END      ! MF_SMTRX