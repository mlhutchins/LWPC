      SUBROUTINE MF_LAGRNG

      implicit complex (a-h,o-z)

      real           omega,wavenr

      common/mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rntg/r(4),dr(4),drdc(4),drdcdh(4),roe(8),rnd(5)
     &      /mf_rseq/r0(4),drdt(4),d2rdt2(4),d3rdt3(4),
     &               zetap(2),gammap(2),theta0,extra


      s=SIN(theta*0.01745329252)
      dcdt=-s*0.01745329252

      delt=theta-theta0
      r(1)=((d3rdt3(1)*delt/6.+d2rdt2(1)/2.)*delt+drdt(1))*delt+r0(1)
      r(2)=((d3rdt3(2)*delt/6.+d2rdt2(2)/2.)*delt+drdt(2))*delt+r0(2)
      r(3)=((d3rdt3(3)*delt/6.+d2rdt2(3)/2.)*delt+drdt(3))*delt+r0(3)
      r(4)=((d3rdt3(4)*delt/6.+d2rdt2(4)/2.)*delt+drdt(4))*delt+r0(4)

      if (ideriv .eq. 1) then
         dr(1)=((d3rdt3(1)*delt/2.+d2rdt2(1))*delt+drdt(1))/dcdt
         dr(2)=((d3rdt3(2)*delt/2.+d2rdt2(2))*delt+drdt(2))/dcdt
         dr(3)=((d3rdt3(3)*delt/2.+d2rdt2(3))*delt+drdt(3))/dcdt
         dr(4)=((d3rdt3(4)*delt/2.+d2rdt2(4))*delt+drdt(4))/dcdt
      end if
      RETURN
      END      ! MF_LAGRNG