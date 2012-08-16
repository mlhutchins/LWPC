      SUBROUTINE LW_FAST_MC
     &          (print_mc)

c***********************************************************************

c Gets mode conversion coefficients using the approximate formulation.

c***********************************************************************

c     INPUT PARAMETERS:

c     print_mc      Indicates levels of print from ModeConversion
c                   =0: minimum print
c                   =1: adds conversion coefficients
c                   =2: adds integrals

c     stp           sine(theta')
c     nreigen       number of theta'
c     nsgmnt        path segment number

c     OUTPUT PARAMETERS:

c     a             conversion coefficients

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     03 Nov 95     Changed to use XC routines in more of the
c                   calculations.

c     13 Sep 98     Changed test on nearly identical modes for
c                   calculations across slab boundaries.
c 2/26/10 MLH Added lw_fast common block with pnmode to prevent seg fault
c 3/10/10 MLH Removed above change, solved by deleting .mds file each run
c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxeigen=50,mxsgmnt=201)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht
      complex  *  8 thetap,capt,fofr,
     &              a,tp,stp,xtra,eyhy
      complex  * 16 norm,capi

      integer       print_mc,pnmode

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex
     &      /mf_sw_2/
     &              thetap(mxsgmnt,mxeigen),
     &              capt(mxsgmnt,4,mxeigen),
     &              fofr(mxsgmnt,mxeigen)
     &      /mc_slab/
     &              a(mxeigen,mxeigen),tp(mxeigen),stp(mxeigen),
     &              xtra(3,mxeigen),eyhy(mxeigen),nreigen,nsgmnt
     &      /mc_stor/
     &              norm(mxeigen,mxeigen),capi(mxeigen)
c     &      /lw_fast/
c     &              pnmode

      real     *  8 waveno,ak,ak13,ka13,ka23,nthsq,err,

     &              a1e,a2e,a3e,a4e,fprle,fprpe,dfprle,dfprpe,
     &              argte,arg0e,argge,

     &              exte(mxeigen),pexte(mxeigen),
     &              eyte(mxeigen),peyte(mxeigen),
     &              hxte(mxeigen),phxte(mxeigen),
     &              hyte(mxeigen),phyte(mxeigen),

     &              h10e,h20e,h1p0e,h2p0e,
     &              h1te,h2te,h1pte,h2pte

      complex  * 16 eigen,ssq,sj,sm,tj,tm,
     &              mik,mult,ngsq,pngsq,
     &              tau(mxeigen),ptau(mxeigen),
     &              ans(mxeigen),pstp(mxeigen),

     &              a1,a2,a3,a4,fprl,fprp,dfprl,dfprp,
     &              argt,arg0,argg,

     &              ex0(mxeigen),pex0(mxeigen),
     &              ey0(mxeigen),pey0(mxeigen),
     &              hx0(mxeigen),phx0(mxeigen),

     &              ext(mxeigen),pext(mxeigen),
     &              eyt(mxeigen),peyt(mxeigen),
     &              hxt(mxeigen),phxt(mxeigen),
     &              hyt(mxeigen),phyt(mxeigen),

     &              q0,h10,h20,h1p0,h2p0,
     &              qt,h1t,h2t,h1pt,h2pt,

     &              i/(0.d0,1.d0)/,w/(0.,-1.45749544)/

      data          alpha/3.14e-04/,epsln0/8.85434e-12/
      
      omega=6.283185308e3*freq
      waveno=20.958445e-3*freq
      mik=-i*waveno
      ak=alpha/waveno
      ak13=EXP(LOG(ak)/3.)
      ka13=1./ak13
      ka23=ka13**2
      ngsq=epsr-i*(sigma/(omega*epsln0))
      nthsq=1.+alpha*hprime
      do m=1,nreigen

         eigen=m
         ssq=stp(m)**2
         tm=SQRT(ngsq-ssq)
         tau(m)=tm
         a2=ka13*i*tm
         a1=a2/ngsq

         q0=ka23*(1.-ssq)
         call XC_MDHNKL (q0,h10 ,h10e ,h20 ,h20e ,
     &                      h1p0,h1p0e,h2p0,h2p0e,eigen,'MC0 ')
         qt=ka23*(nthsq-ssq)
         call XC_MDHNKL (qt,h1t ,h1te ,h2t ,h2te ,
     &                      h1pt,h1pte,h2pt,h2pte,eigen,'MCT ')

c        a3=h1t*h20-h10*h2t
         call XC_ADD (a3,a3e,h1t  *h20  ,h1te +h20e ,
     &                      -h10  *h2t  ,h10e +h2te )
c        a4=h1t*h2p0-h1p0*h2t
         call XC_ADD (a4,a4e,h1t  *h2p0 ,h1te +h2p0e,
     &                      -h1p0 *h2t  ,h1p0e+h2te )
c        fprl=a4-a1*a3
c        fprp=a4-a2*a3
         call XC_ADD (fprl,fprle,a4,a4e,-a1*a3,a3e)
         call XC_ADD (fprp,fprpe,a4,a4e,-a2*a3,a3e)

c        a3=h1pt*h20-h10*h2pt
         call XC_ADD (a3,a3e,h1pt *h20  ,h1pte+h20e ,
     &                      -h10  *h2pt ,h10e +h2pte)
c        a4=h1pt*h2p0-h1p0*h2pt
         call XC_ADD (a4,a4e,h1pt *h2p0 ,h1pte+h2p0e,
     &                      -h1p0 *h2pt ,h1p0e+h2pte)
c        dfprl=a4-a1*a3
c        dfprp=a4-a2*a3
         call XC_ADD (dfprl,dfprle,a4,a4e,-a1*a3,a3e)
         call XC_ADD (dfprp,dfprpe,a4,a4e,-a2*a3,a3e)

         ex0(m)=-tm/ngsq
         ey0(m)=    eyhy(m)
         hx0(m)= tm*eyhy(m)
c        hy0(m)= 1

         ext(m)= ak13*i       *dfprl/w
         eyt(m)=        ey0(m)* fprp/w
         hxt(m)=-ak13*i*ey0(m)*dfprp/w
         hyt(m)=                fprl/w

         exte(m)=dfprle
         eyte(m)= fprpe
         hxte(m)=dfprpe
         hyte(m)= fprle

         call XC_NORME (ext(m),exte(m))
         call XC_NORME (eyt(m),eyte(m))
         call XC_NORME (hxt(m),hxte(m))
         call XC_NORME (hyt(m),hyte(m))
      end do
      
      if (nsgmnt .eq. 1) then

c        First slab
         do m=1,nreigen
            do j=1,nreigen
               if (j .eq. m) then
                  a(j,m)=1.
               else
                  a(j,m)=0.
               end if
            end do
         end do
      else
      
c        Integrals in current slab
         if (print_mc .gt. 1)
     &      write(lwpcLOG_lun,
     &          '(/''LW_FAST_MC: Integrals in segment'',i3)') nsgmnt
         do m=1,nreigen

            sm=stp(m)
            tm=tau(m)

            if (print_mc .gt. 1) write(lwpcLOG_lun,'('' '')')
            do j=1,nreigen

               sj=stp(j)
               tj=tau(j)

               if (j .eq. m) then

c                 Modes are equal
                  mult=sm*2./alpha

                  ssq=sm**2
                  q0=1.-ssq
                  qt=nthsq-ssq

c                 argt=(qt*eyt(m)**2-hxt(m)**2)!*capt(nsgmnt,4,m)
c    &                +(qt*hyt(m)**2-ext(m)**2)

                  call XC_ADD (a1,a1e,qt*eyt(m)**2,2.*eyte(m),
     &                                  -hxt(m)**2,2.*hxte(m))

                  call XC_ADD (a2,a2e,qt*hyt(m)**2,2.*hyte(m),
     &                                  -ext(m)**2,2.*exte(m))

                  call XC_ADD (argt,argte,a1,a1e,a2,a2e)

                  arg0=(q0*ey0(m)**2-hx0(m)**2)!*capt(nsgmnt,4,m)
     &                +(q0          -ex0(m)**2)
                  arg0e=0.
                  call XC_NORME (arg0,arg0e)
               else

c                 Modes are distinct
                  mult=1./(mik*(sj-sm))

c                 argt=(eyt(m)*hxt(j)-eyt(j)*hxt(m))!*capt(nsgmnt,4,m)
c    &                -(hyt(m)*ext(j)-hyt(j)*ext(m))

                  call XC_ADD (a1,a1e,eyt(m)*hxt(j),eyte(m)+hxte(j),
     &                               -eyt(j)*hxt(m),eyte(j)+hxte(m))

                  call XC_ADD (a2,a2e,hyt(m)*ext(j),hyte(m)+exte(j),
     &                               -hyt(j)*ext(m),hyte(j)+exte(m))

                  call XC_ADD (argt,argte,a1,a1e,-a2,a2e)

                  arg0=(ey0(m)*hx0(j)-ey0(j)*hx0(m))!*capt(nsgmnt,4,m)
     &                -(       ex0(j)-       ex0(m))
                  arg0e=0.
                  call XC_NORME (arg0,arg0e)
               end if

               argg=(1.+ey0(m)*ey0(j))*(sm+sj)/(mik*(tm+tj))
               argge=0.
               call XC_NORME (argg,argge)

c              norm(j,m)=mult*(argt-arg0)-argg

               call XC_ADD (a1,a1e, argt,argte,-arg0,arg0e)
               call XC_ADD (a2,a2e,mult*a1,a1e,-argg,argge)

               norm(j,m)=a2*EXP(a2e)

               if (print_mc .gt. 1)
     &            write(lwpcLOG_lun,
     &                '(''Norm('',i2,'','',i2,'')='',
     &                  1p2e15.6)') m,j,norm(j,m)
            end do
         end do
         
         if (nthsq .ne. pnthsq) then

c           Previous slab had different TOPHT, must recompute fields
            do j=1,pnmode

               eigen=j
               ssq=pstp(j)**2
               a2=ka13*i*ptau(j)
               a1=a2/pngsq

               q0=ka23*(1.-ssq)
                              
               call XC_MDHNKL (q0,h10 ,h10e ,h20 ,h20e ,
     &                            h1p0,h1p0e,h2p0,h2p0e,eigen,'MCP0')

               qt=ka23*(nthsq-ssq)
               call XC_MDHNKL (qt,h1t ,h1te ,h2t ,h2te ,
     &                            h1pt,h1pte,h2pt,h2pte,eigen,'MCPT')

c              a3=h1t   *h20 -h10 *h2t
               call XC_ADD (a3,a3e,h1t  *h20  ,h1te +h20e ,
     &                            -h10  *h2t  ,h10e +h2te )
c              a4=h1t   *h2p0-h1p0*h2t
               call XC_ADD (a4,a4e,h1t  *h2p0 ,h1te +h2p0e,
     &                            -h1p0 *h2t  ,h1p0e+h2te )
c              fprl=a4-a1*a3
c              fprp=a4-a2*a3

               call XC_ADD (fprl,fprle,a4,a4e,-a1*a3,a3e)
               call XC_ADD (fprp,fprpe,a4,a4e,-a2*a3,a3e)

c              a3=h1pt*h20 -h10 *h2pt
               call XC_ADD (a3,a3e,h1pt *h20  ,h1pte+h20e ,
     &                            -h10  *h2pt ,h10e +h2pte)
c              a4=h1pt*h2p0-h1p0*h2pt
               call XC_ADD (a4,a4e,h1pt *h2p0 ,h1pte+h2p0e,
     &                            -h1p0 *h2pt ,h1p0e+h2pte)
c              dfprl=a4-a1*a3
c              dfprp=a4-a2*a3
               call XC_ADD (dfprl,dfprle,a4,a4e,-a1*a3,a3e)
               call XC_ADD (dfprp,dfprpe,a4,a4e,-a2*a3,a3e)

               pext(j)= ak13*i        *dfprl/w
               peyt(j)=        pey0(j)* fprp/w
               phxt(j)=-ak13*i*pey0(j)*dfprp/w
               phyt(j)=                 fprl/w

               pexte(j)=dfprle
               peyte(j)= fprpe
               phxte(j)=dfprpe
               phyte(j)= fprle

               call XC_NORME (pext(j),pexte(j))
               call XC_NORME (peyt(j),peyte(j))
               call XC_NORME (phxt(j),phxte(j))
               call XC_NORME (phyt(j),phyte(j))
                              
            end do
         end if
         
c        Integrals across slab boundary
         init=0
         do j=1,pnmode

            sj=pstp(j)
            tj=ptau(j)

            if (print_mc .gt. 1) write(lwpcLOG_lun,'('' '')')
            do m=1,nreigen

               sm=stp(m)
               tm=tau(m)

               if (ABS(DREAL((sj-sm)/sj)) .lt. 1.d-5 .and.
     &             ABS(DIMAG((sj-sm)/sj)) .lt. 1.d-5) then

c                 Modes are nearly identical
                  mult=sm*2./alpha

                  ssq=sm**2
                  q0=1.-ssq
                  qt=nthsq-ssq

c                 argt=(qt*eyt(m)*peyt(j)-hxt(m)*phxt(j))!*capt(nsgmnt,4,m)
c    &                +(qt*hyt(m)*phyt(j)-ext(m)*pext(j))

                  call XC_ADD
     &                (a1,a1e,qt*eyt(m)*peyt(j),eyte(m)+peyte(j),
     &                          -hxt(m)*phxt(j),hxte(m)+phxte(j))

                  call XC_ADD
     &                (a2,a2e,qt*hyt(m)*phyt(j),hyte(m)+phyte(j),
     &                          -ext(m)*pext(j),exte(m)+pexte(j))

                  call XC_ADD (argt,argte,a1,a1e,a2,a2e)

                  arg0=(q0*ey0(m)*pey0(j)-hx0(m)*phx0(j))!*capt(nsgmnt,4,m)
     &                +(q0               -ex0(m)*pex0(j))
                  arg0e=0.
                  call XC_NORME (arg0,arg0e)
               else

c                 Modes are distinct
                  mult=1./(mik*(sj-sm))

c                 argt=(eyt(m)*phxt(j)-peyt(j)*hxt(m))!*capt(nsgmnt,4,m)
c    &                -(hyt(m)*pext(j)-phyt(j)*ext(m))

                  call XC_ADD
     &                (a1,a1e, eyt(m)*phxt(j), eyte(m)+phxte(j),
     &                       -peyt(j)* hxt(m),peyte(j)+ hxte(m))

                  call XC_ADD
     &                (a2,a2e, hyt(m)*pext(j), hyte(m)+pexte(j),
     &                       -phyt(j)* ext(m),phyte(j)+ exte(m))

                  call XC_ADD (argt,argte,a1,a1e,-a2,a2e)

                  arg0=(ey0(m)*phx0(j)-pey0(j)*hx0(m))!*capt(nsgmnt,4,m)
     &                -(       pex0(j)-        ex0(m))
                  arg0e=0.
                  call XC_NORME (arg0,arg0e)
               end if

               argg=(1.+ey0(m)*pey0(j))*(sm+sj)/(mik*(tm+tj))
               argge=0.
               call XC_NORME (argg,argge)

c              capi(m)=mult*(argt-arg0)-argg

               call XC_ADD (a1,a1e, argt,argte,-arg0,arg0e)
               call XC_ADD (a2,a2e,mult*a1,a1e,-argg,argge)

               capi(m)=a2*EXP(a2e)

               if (print_mc .gt. 1)
     &            write(lwpcLOG_lun,
     &                '(''Capi('',i2,'','',i2,'')='',
     &                    1p2e15.6)') m,j,capi(m)
            end do

c           Calculate conversion coefficients for Hy
            call CLINEQ (norm,capi,ans,nreigen,mxeigen,init,err)
            init=1
            do m=1,nreigen
               a(j,m)=ans(m)
            end do
         end do
      end if
      
c     Save data for next slab
      pngsq=ngsq
      pnthsq=nthsq
      pnmode=nreigen
      do j=1,pnmode
         pstp(j)=stp(j)
         ptau(j)=tau(j)

         pex0(j)=ex0(j)
         pey0(j)=ey0(j)
         phx0(j)=hx0(j)

         pext(j)=ext(j)
         peyt(j)=eyt(j)
         phxt(j)=hxt(j)
         phyt(j)=hyt(j)

         pexte(j)=exte(j)
         peyte(j)=eyte(j)
         phxte(j)=hxte(j)
         phyte(j)=hyte(j)
      end do
      
      RETURN
      END      ! LW_FAST_MC