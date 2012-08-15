      SUBROUTINE WF_HTGAIN
     &          (height,ex,ey,ez,hx,hy,hz)

      implicit real*8 (a-h,o-z)

c     Wave Fields
      real     *  8 freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht,
     &              dtheta,lub,thtinc,alpha,h,prec
      complex  * 16 eigen2,
     &              exc,dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld,
     &              c,s,p,dpdh

      common/wf_grnd/
     &              exc(3),dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld
     &      /wf_inpt/
     &              eigen2,
     &              freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht
     &      /wf_iter/
     &              dtheta(2),lub(2),thtinc,alpha,h,prec,
     &              iter_flag,mxiter,nriter
     &      /wf_pmtx/
     &              c(2),s(2),p(8,2),dpdh(8,2)

c     Local
      real     *  8 kvraot,kvratt,n0sq,nzsq
      complex  * 16 ssq,ngsq,sqroot,den12,den34,
     &              term1,term2,
     &              q0,h10,h20,dh10,dh20,
     &              qz,h1z,h2z,dh1z,dh2z,exz,exzsq,
     &              a1st,a2nd,a3rd,a4th,z1,z2,
     &              ex,ey,ez,hx,hy,hz

      data          den12/(0.d0,1.457495441040461d0)/,
     &              den34/(0.d0,1.457495441040461d0)/


      omega=6.2831853071796d3*freq2
      wn=20.958445d-3*freq2

      ssq=s(1)**2
      ngsq=DCMPLX(epsr2,-sigma2/(8.85434d-12*omega))
      sqroot=SQRT(ngsq-ssq)

      alt=height
      if (DIMAG(eigen2) .lt. -10.d0) then

c        Flat earth
         exz=EXP(DCMPLX(0.d0,-alt*wn)*c(1))
         exzsq=exz**2

         z1=(ngsq*c(1)-sqroot)
     &     /(ngsq*c(1)+sqroot)
         z2=(     c(1)-sqroot)
     &     /(     c(1)+sqroot)

         hy= (1.d0+z1*exzsq)
     &      /(1.d0+z1)/exz*hyg
         ey= (1.d0+z2*exzsq)
     &      /(1.d0+z2)/exz*eyg

         hx= (1.d0-z2*exzsq)
     &      /(1.d0+z2)/exz*eyg*c(1)
         ex= (1.d0-z1*exzsq)
     &      /(1.d0+z1)/exz*hyg*c(1)

         hz= ey*s(1)
         ez=-hy*s(1)
      else

         expon=EXP(0.5d0*alpha*alt)

         kvraot=EXP(LOG(wn/alpha)/3.d0)
         kvratt=kvraot**2
         avrkot=1.d0/kvraot
         avrktt=avrkot**2*0.5d0

         n0sq=1.d0-alpha*h
         q0=kvratt*(n0sq-ssq)

ccc      call MDHNKL (q0,h10,h20,dh10,dh20,eigen2,'rb 1')
ccc      rtiort=n0sq/ngsq*sqroot
ccc      caph10=dh10+avrktt*h10
ccc      caph20=dh20+avrktt*h20
ccc      a1st=caph20-(0.d0,1.d0)*rtiort*kvraot*h20
ccc      a2nd=caph10-(0.d0,1.d0)*rtiort*kvraot*h10
ccc      a3rd=dh20-(0.d0,1.d0)*kvraot*sqroot*h20
ccc      a4th=dh10-(0.d0,1.d0)*kvraot*sqroot*h10
ccc
ccc      den12=h20*a2nd-h10*a1st  == (0.d0,1.457495441040461d0)
ccc      den34=h20*a4th-h10*a3rd  == (0.d0,1.457495441040461d0)

         call XC_MDHNKL (q0, h10, h10e, h20, h20e,
     &                      dh10,dh10e,dh20,dh20e,eigen2,'HG 1')

         nzsq=1.d0-alpha*(h-alt)
         qz=kvratt*(nzsq-ssq)

ccc      call MDHNKL (qz,h1z,h2z,dh1z,dh2z,eigen2,'hg 1')
ccc
ccc      hy=(h2z*a2nd-h1z*a1st)/den12*hyg*expon
ccc      ey=(h2z*a4th-h1z*a3rd)/den34*eyg
ccc
ccc      hx=avrkot/(0.d0,1.d0)*( dh2z*a4th-dh1z*a3rd)/den34*eyg
ccc      ex=(0.d0,1.d0)*avrkot*((dh2z*a2nd-dh1z*a1st)//den12*hyg*expon
ccc  &                          +avrktt*hy)/nzsq
ccc
ccc      hz= s(1)*ey
ccc      ez=-s(1)*hy/nzsq

         call XC_MDHNKL (qz, h1z, h1ze, h2z, h2ze,
     &                      dh1z,dh1ze,dh2z,dh2ze,eigen2,'HG 2')

         a1st= h10* h2z*EXP( h10e+ h2ze)- h1z* h20*EXP( h1ze+ h20e)
         a2nd= h10*dh2z*EXP( h10e+dh2ze)-dh1z* h20*EXP(dh1ze+ h20e)
         a3rd=dh10* h2z*EXP(dh10e+ h2ze)- h1z*dh20*EXP( h1ze+dh20e)
         a4th=dh10*dh2z*EXP(dh10e+dh2ze)-dh1z*dh20*EXP(dh1ze+dh20e)

         term1=avrktt+DCMPLX(0.d0,-kvraot*n0sq)*sqroot/ngsq
         term2=DCMPLX(0.d0,kvraot)*sqroot

         hy= (term1*a1st+a3rd)/den12*hyg*expon
         ey=-(term2*a1st-a3rd)/den34*eyg

         hx=DCMPLX(0.d0,avrkot)*( term2*a2nd-a4th)/den34*eyg
         ex=DCMPLX(0.d0,avrkot)*((term1*a2nd+a4th)/den12*hyg*expon
     &                           +avrktt*hy)/nzsq

         hz= ey*s(1)
         ez=-hy*s(1)/nzsq
      end if
      RETURN
      END      ! WF_HTGAIN