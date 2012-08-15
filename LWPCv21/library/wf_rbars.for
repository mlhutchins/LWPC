      SUBROUTINE WF_RBARS
     &          (index)

      implicit real*8 (a-h,o-z)

c     Wave Fields
      real     *  8 freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht,
     &              dtheta,lub,thtinc,alpha,h,prec
      complex  * 16 eigen2,
     &              exc,dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld,
     &              c,s,p,dpdh,
     &              r11,r22,r12,r21,rbar11,rbar22

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
     &      /wf_rmtx/
     &              r11,r22,r12,r21,rbar11,rbar22

c     Local
      real     *  8 kvraot,kvratt,n0sq,ndsq
      complex  * 16 ssq,ngsq,sqroot,den12,den34,
     &              term1,term2,
     &              q0,h10,h20,dh10,dh20,
     &              qd,h1d,h2d,dh1d,dh2d,exd,exdsq,
     &              a1st,a2nd,a3rd,a4th,a1,a2,a3,a4,z1,z2

      data          den12/(0.d0,1.457495441040461d0)/,
     &              den34/(0.d0,1.457495441040461d0)/


      omega=6.2831853071796d3*freq2
      wn=20.958445d-3*freq2

      alt=bot ht
      eyg=1.d0
      hyg=1.d0

      ssq=s(index)**2
      ngsq=DCMPLX(epsr2,-sigma2/(8.85434d-12*omega))
      sqroot=SQRT(ngsq-ssq)

      if (DIMAG(eigen2) .lt. -10.d0) then

c        Flat earth
         exd=EXP(DCMPLX(0.d0,-alt*wn)*c(index))
         exdsq=exd**2
         z1=(ngsq*c(index)-sqroot)
     &     /(ngsq*c(index)+sqroot)
         z2=(     c(index)-sqroot)
     &     /(     c(index)+sqroot)
         rbar11=z1*exdsq
         rbar22=z2*exdsq

         fprld=(1.d0+rbar11)/exd
         fprpd=(1.d0+rbar22)/exd

         hy0=(1.d0+z1*exdsq)
     &      /(1.d0+z1)/exd
         ey0=(1.d0+z2*exdsq)
     &      /(1.d0+z2)/exd
      else

         if (bot ht .eq. 0.d0) then

            z1=(ngsq*c(index)-sqroot)
     &        /(ngsq*c(index)+sqroot)
            z2=(     c(index)-sqroot)
     &        /(     c(index)+sqroot)
            rbar11=z1
            rbar22=z2

            fprld=(0.d0,-1.457495441040461d0)
            fprpd=(0.d0,-1.457495441040461d0)

            hy0=(1.d0+z1)
     &         /(1.d0+z1)
            ey0=(1.d0+z2)
     &         /(1.d0+z2)
         else

            kvraot=EXP(LOG(wn/alpha)/3.d0)
            kvratt=kvraot**2
            avrkot=1.d0/kvraot
            avrktt=avrkot**2*0.5d0

            n0sq=1.d0-alpha*h
            q0=kvratt*(n0sq-ssq)

ccc         call MDHNKL (q0,h10,h20,dh10,dh20,eigen2,'rb 1')
ccc         rtiort=n0sq/ngsq*sqroot
ccc         caph10=dh10+avrktt*h10
ccc         caph20=dh20+avrktt*h20
ccc         a1st=caph20-(0.d0,1.d0)*rtiort*kvraot*h20
ccc         a2nd=caph10-(0.d0,1.d0)*rtiort*kvraot*h10
ccc         a3rd=dh20-(0.d0,1.d0)*kvraot*sqroot*h20
ccc         a4th=dh10-(0.d0,1.d0)*kvraot*sqroot*h10
ccc
ccc         den12=h20*a2nd-h10*a1st  == (0.d0,1.457495441040461d0)
ccc         den34=h20*a4th-h10*a3rd  == (0.d0,1.457495441040461d0)

            call XC_MDHNKL (q0, h10, h10e, h20, h20e,
     &                         dh10,dh10e,dh20,dh20e,eigen2,'RB 1')

            ndsq=1.d0-alpha*(h-bot ht)
            qd=kvratt*(ndsq-ssq)

ccc         call MDHNKL (qd,h1d,h2d,dh1d,dh2d,eigen2,'rb 2')
ccc         caph1d=dh1d+avrktt*h1d
ccc         caph2d=dh2d+avrktt*h2d
ccc         fprld=(h2d*a2nd-h1d*a1st)
ccc         fprpd=(h2d*a4th-h1d*a3rd)
ccc         a1=c(index)*ndsq*fprld
ccc         a2=(0.d0,1.d0)*avrkot*(caph1d*a1st-caph2d*a2nd)
ccc         a3=(0.d0,1.d0)*avrkot*(dh2d*a4th-dh1d*a3rd)
ccc         a4=c(index)*fprpd

            call XC_MDHNKL (qd, h1d, h1de, h2d, h2de,
     &                         dh1d,dh1de,dh2d,dh2de,eigen2,'RB 2')

            a1st= h10* h2d*EXP( h10e+ h2de)- h1d* h20*EXP( h1de+ h20e)
            a2nd= h10*dh2d*EXP( h10e+dh2de)-dh1d* h20*EXP(dh1de+ h20e)
            a3rd=dh10* h2d*EXP(dh10e+ h2de)- h1d*dh20*EXP( h1de+dh20e)
            a4th=dh10*dh2d*EXP(dh10e+dh2de)-dh1d*dh20*EXP(dh1de+dh20e)

            term1=avrktt+DCMPLX(0.d0,-kvraot*n0sq)*sqroot/ngsq
            term2=DCMPLX(0.d0,kvraot)*sqroot

            fprld= term1*a1st+a3rd
            fprpd=-term2*a1st+a3rd

            a1=c(index)*ndsq*fprld
            a2=DCMPLX(0.d0,-avrkot)*(avrktt*term1*a1st+term1*a2nd
     &                              +avrktt*a3rd+a4th)
            a3=DCMPLX(0.d0,-avrkot)*(term2*a2nd-a4th)
            a4=c(index)*fprpd

            rbar11=(a1-a2)/(a1+a2)
            rbar22=(a3+a4)/(a4-a3)

            hy0=fprld/den12*EXP(0.5d0*alpha*alt)
            ey0=fprpd/den34
         end if
      end if
      RETURN
      END      ! WF_RBARS