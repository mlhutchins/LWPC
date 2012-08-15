      SUBROUTINE MF_INITOE

c***********************************************************************

c     This routine interpolates values of the elements of the magneto-
c     ionic reflection matrix. Entry INIT OE determines the coefficients
c     for the interpolation series if this kind of interpolation can be
c     used. Otherwise, the parameter LENSET is set to be non-zero in the
c     call to E MTRX indicating that the rectangle size is to be reduced
c     or routine LAGRNG is to be used. Additional details are found in
c     section V of NOSC TR 1143.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     20 Nov 96     Added test for SZSQ2D to be zero to prevent division
c                   by zero.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxboxes=200,mxcorners=400)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      integer        pflag,pindex,rprnt,xprnt
      real           freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,
     &               adjmsh,wdchng,unitr,uniti,
     &               boxlft,boxrht,boxtop,boxbot,
     &               dcl,dcm,dcn,ec,
     &               rtol,xintrp,htntrp,d,z0,zz,
     &               omega,wavenr,tlft,trht,ttop,tbot,
     &               atol,diffsq,roemin,tlengt,
     &               sizesq,szsqmn,szsq2d,delp,delm,size,etol,
     &               tl(2),trin(2,25),amp(4),phs(4),lprnt1

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex

     &      /mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_crnr/tcornr(mxcorners),rcornr(4,mxcorners),nrcorner
     &      /mf_drcs/dcl,dcm,dcn,ec,g,q
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rntg/r(4),dr(4),drdh(4),drdcdh(4),
     &               roe(4),droe(4),rnd(5)
     &      /mf_roes/rlist(22,mxboxes)
     &      /mf_rseq/r0(4),drdt(4),d2rdt2(4),d3rdt3(4),
     &               zetap(2),gammap(2),theta0,extra
     &      /mf_side/tlft,trht,ttop,tbot

      dimension      rseq(22),thetag(4),rgiven(4,4),rlow(4,4),
     &               droedt(4),drlsq(4),rcomp(4),htrefl(2),eigrfh(2)

      equivalence   (rseq,r0)

      data           nrlist/22/,maxnrt/4/,roemin/1.e-8/

c     Values of theta ath the corners and the center of the search area.
      thetag(1)=CMPLX(tlft,ttop)
      thetag(2)=CMPLX(trht,ttop)
      thetag(3)=CMPLX(tlft,tbot)
      thetag(4)=CMPLX(trht,tbot)

      theta0=CMPLX(.5*(tlft+trht),.5*(tbot+ttop))

c     Reflection coefficients at the bottom of the ionosphere for each
c     of the corners are found and stored unless they have already been
c     found for an adjacent rectangle. Corners at which the integration
c     cannot be done are noted. No search is done near such points in
c     the theta plane which might be near a pole in an element of the
c     reflection matrix. The size of rectangles near such points are
c     reduced in routine BNDRYS.
      ideriv=0
      nointg=0

      atol=0.01*adjmsh
      do l=1,4
         last=0
         if (nrcorner .gt. 0) then
            lc=1
            do while (last .eq. 0 .and. lc .le. nrcorner)
               delt=thetag(l)-tcornr(lc)
               diffsq=REAL(delt)**2+AIMAG(delt)**2
               if (diffsq .le. atol**2) then
                  call MF_XFER (rcornr(1,lc),r,4)
                  if (REAL(r(1)) .lt. 22000.) then ! EXP(RLGMAX)
                     last=1
                  else
                     nointg=1
                     RETURN
                  end if
               end if
               lc=lc+1
            end do
         end if
         if (last .eq. 0) then
            theta=thetag(l)
            call MF_INTEG
            if (nointg .ne. 0) r(1)=60000. ! EXP(RLGMAX+1)
            if (nrcorner .lt. mxcorners) then
               nrcorner=nrcorner+1
               tcornr(nrcorner)=theta
               call MF_XFER (r,rcornr(1,nrcorner),4)
            end if
            if (nointg .ne. 0) RETURN
         end if
         call MF_XFER (r,rlow(1,l),4)
      end do

c     The values of square roots at the center of the search area are
c     found in routine E MTRX. They are reference values.
      theta=theta0
      call MF_EMTRX (1)

c     Set up heights for FSINTG
      z0=botht
      zz=htntrp

c     The reflection coefficients are referred to HTNTRP and then
c     transformed to magneto-ionic reflection coefficients. The logs
c     of these, in turn, are found and taken to be given values at the
c     corners of the search rectangle.
      do l=1,4
         theta=thetag(l)
         call MF_XFER (rlow(1,l),r,4)
         call MF_FSINTG
         call MF_ROEMTX
         call MF_RMTRX
         do k=1,4
            if (ABS(roe(k)) .lt. roemin) roe(k)=roemin
            rgiven(k,l)=LOG(roe(k))
         end do
      end do

c     A set of 25 unique pairs of integers are generated such that each
c     integer is at least -2 and not greater than 2. These pairs are
c     stored in TRIN.

      tl(1)=-1.
      tl(2)=-1.
      m=0
      do m1=1,5
         tl(1)=tl(1)+1.
         if (tl(1) .gt. 2.5) tl(1)=-2.
         do m2=1,5
            tl(2)=tl(2)+1.
            if (tl(2) .gt. 2.5) tl(2)=-2.
            m=m+1
            trin(1,m)=tl(1)
            trin(2,m)=tl(2)
         end do
      end do

c     Values of parameters are found that pertain to the geometry of the
c     search rectangle.

      tlengt=ABS(thetag(1)-theta0)
      d1=(thetag(1)-theta0)/tlengt
      d2=(thetag(2)-theta0)/tlengt

      d1sq=d1**2
      d1cu=d1**3
      d2sq=d2**2
      d2cu=d2**3

      twdet1=2.*(d2sq-d1sq)
      twdet2=2.*(d2sq-d1sq)*d1*d2

      conjd1=conjg(d1)
      conjd2=conjg(d2)

      g11= d2sq/twdet1
      g12=-d1sq/twdet1
      g21=-1./twdet1
      g22= 1./twdet1

      w11= d2cu/twdet2
      w12=-d1cu/twdet2
      w21=-d2/twdet2
      w22= d1/twdet2

c     Each value of K corresponds to an element of the magneto-ionic
c     reflection matrix.

      notlin=0
      do k=1,4
         if (mrprnt .ne. 0) then
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: k='',i1)')
     &              k
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: Set a1 and a3'')')
         end if
         nrt=0

c        Tentative values of the first and third order interpolation
c        coefficients are found. Ambiguities in differences in whole
c        cycles of phase between logs of the coefficients at opposite
c        corners of the rectangle must be resolved.

41       v1=rgiven(k,1)-rgiven(k,4)
         v2=rgiven(k,2)-rgiven(k,3)

         a1=w11*v1+w12*v2
         a3=w21*v1+w22*v2

c        A least-square value of the first order coefficient is found
c        for the situation in which the third order coefficient is
c        omitted. The least-square series is used only for error testing
c        and does not otherwise affect the interpolation values.

         a1lsq=(conjd1*v1+conjd2*v2)/(conjd1*d1+conjd2*d2)*.5

c        The combination of whole cycles of phase is chosen that
c        corresponds to the smallest magnitude of the third order
c        coefficient.

         szsqmn=9.e9
         b3i=a3/(0.,6.283185308)
         do m=1,25
            b3=b3i+w21*trin(1,m)+w22*trin(2,m)
            sizesq=REAL(b3)**2+AIMAG(b3)**2
            if (mrprnt .ne. 0)
     &         write(lwpcLOG_lun,
     &             '(''MF_INITOE: '',2f4.0,3x,1pe12.3)')
     &                 trin(1,m),trin(2,m),sizesq
            if (sizesq .lt. szsqmn) then
               szsq2d=szsqmn
               mmin=m
               szsqmn=sizesq
            else
               if (sizesq .lt. szsq2d) szsq2d=sizesq
            end if
         end do

c        Whole cylces of phase are added to or subtracted from logs of
c        the magneto-ionic reflection coefficients.

         if (mmin .ne. 1) then
            if (mrprnt .ne. 0)
     &         write(lwpcLOG_lun,'(''MF_INITOE: '')')
            nrt=nrt+1
            if (nrt .gt. maxnrt) then
               notlin=1
               if (mrprnt .ne. 0)
     &            write(lwpcLOG_lun,
     &                '(''MF_INITOE: '',
     &                  ''Cycles of log ROE cannot be set'')')
               RETURN
            end if
            rgiven(k,1)=rgiven(k,1)+trin(1,mmin)*(0.,6.283185308)
            rgiven(k,2)=rgiven(k,2)+trin(2,mmin)*(0.,6.283185308)
            go to 41
         end if

c        The magneto-ionic reflection coefficients are not used for this
c        search rectangle unless the choise of whole cycles of phase in
c        the logs of them is clear.

         if (szsq2d .eq. 0. .or. szsqmn/szsq2d .ge. 0.1) then
            notlin=1
            if (mrprnt .ne. 0)
     &         write(lwpcLOG_lun,
     &             '(''MF_INITOE: '',
     &               ''Ratio of smallest to next smallest element '',
     &               ''is too large'')')
            RETURN
         end if

         if (mrprnt .ne. 0)
     &      write(lwpcLOG_lun,
     &          '(''MF_INITOE: Set a0 and a2'')')
         nrt=0

c        Tentative values of the zero-th and second order interpolation
c        coefficients are found. Ambiguities ain cycles of phase of logs
c        must be resolved.
52       v1=rgiven(k,1)+rgiven(k,4)
         v2=rgiven(k,2)+rgiven(k,3)

         a0=g11*v1+g12*v2
         a2=g21*v1+g22*v2

c        The choice of whole cycles of phase is the one which
c        corresponds to the smallest magnitude of the second order
c        coefficient and that leaves A3 unchanged. The first values of
c        TRIN(2,M) are 0,1,2,-2,-1.

         szsqmn=9.e9
         b2i=a2/(0.,12.56637062)
         do m=1,5
            b2=b2i+g21*trin(2,m)
            sizesq=REAL(b2)**2+AIMAG(b2)**2
            if (mrprnt .ne. 0)
     &         write(lwpcLOG_lun,
     &             '(''MF_INITOE: '',f4.0,3x,1pe12.3)')
     &                 trin(2,m),sizesq
            if (sizesq .lt. szsqmn) then
               szsq2d=szsqmn
               mmin=m
               szsqmn=sizesq
            else
               if (sizesq .lt. szsq2d) szsq2d=sizesq
            end if
         end do

         if (mmin .ne. 1) then
            if (mrprnt .ne. 0)
     &         write(lwpcLOG_lun,'(''MF_INITOE: '')')
            nrt=nrt+1
            if (nrt .gt. maxnrt) then
               notlin=1
               if (mrprnt .ne. 0)
     &            write(lwpcLOG_lun,
     &                '(''MF_INITOE: '',
     &                  ''Cycles of log ROE cannot be set'')')
               RETURN
            end if

c           Add or subtract whole cycles of phase.
            rgiven(k,1)=rgiven(k,1)+trin(2,mmin)*(0.,6.283185308)
            rgiven(k,4)=rgiven(k,4)+trin(2,mmin)*(0.,6.283185308)
            go to 52
         end if

c        The magneto-ionic reflection coefficients are not used for this
c        search rectangle unless the choise of whole cycles of phase in
c        the logs of them is clear.

         if (szsq2d .eq. 0. .or. szsqmn/szsq2d .ge. 0.1) then
            notlin=1
            if (mrprnt .ne. 0)
     &         write(lwpcLOG_lun,
     &             '(''MF_INITOE: '',
     &               ''Ratio of smallest to next smallest element '',
     &               ''is too large'')')
            RETURN
         end if

c        Find the interpolation coefficients in the form used in the
c        main entry.

         r0(k)=a0
         drdt(k)=a1/tlengt
         d2rdt2(k)=2.*a2/tlengt**2
         d3rdt3(k)=6.*a3/tlengt**3
         drlsq(k)=a1lsq/tlengt
      end do

c     Store the interpolation coefficients for use in WGSORT.
      call MF_XFER (rseq,rlist(1,kb),nrlist)

      if (mrprnt .ne. 0) then

c        Two apparent complex heights of reflection are found based on
c        the values and derivatives of the logs of the diagonal elements
c        of the reflection matrix. This is discussed in section III and
c        examples are given in section VII of NOSC TR 1143. The heights
c        are found only for informative print out. Two apparent heights
c        of reflection are also formed based on the eigen values of the
c        reflection matrix and their derivatives. These are also only
c        for informative print out.

         do k=1,4
            roe(k)=EXP(r0(k))
            droedt(k)=drdt(k)*roe(k)
         end do

         s=SIN(theta0*0.01745329252)
         dcdt=-s*0.01745329252
         den=CMPLX(0.,2.*wavenr)*dcdt
         htrefl(1)=htntrp-drdt(1)/den
         htrefl(2)=htntrp-drdt(4)/den

         eigrfh(1)=-99.
         eigrfh(2)=-99.

         arg=(roe(1)-roe(4))**2+4.*roe(2)*roe(3)
         rt=SQRT(arg)
         if (ABS(rt) .ge. roemin) then
            sm=roe(1)+roe(4)
            if (ABS(sm+rt) .lt. ABS(sm-rt)) rt=-rt
            eigpls=sm+rt

            darg=2.*(roe(1)-roe(4))*(droedt(1)-droedt(4))
     &          +4.*(droedt(2)*roe(3)+roe(2)*droedt(3))
            drt=darg/(2.*rt)
            dsm=droedt(1)+droedt(4)
            deigpl=(dsm+drt)/eigpls
            eigrfh(1)=htntrp-deigpl/den

            eigmns=sm-rt
            if (ABS(eigmns) .ge. roemin) then
               deigml=(dsm-drt)/eigmns
               eigrfh(2)=htntrp-deigml/den
            end if
         end if
         write(lwpcLOG_lun,
     &       '(''MF_INITOE: theta0='',2f7.3)')
     &            theta0
         do k=1,4
            amp(k)=EXP(REAL(roe(k)))
            phs(k)=   AIMAG(roe(k))*57.296
         end do
         write(lwpcLOG_lun,
     &       '(''MF_INITOE: ROE'')')
         write(lwpcLOG_lun,
     &       '(''MF_INITOE: '',2f12.5,15x,2f12.5)')
     &           amp(1),amp(3),phs(1),phs(3)
         write(lwpcLOG_lun,
     &       '(''MF_INITOE: '',2f12.5,15x,2f12.5)')
     &           amp(2),amp(4),phs(2),phs(4)
         write(lwpcLOG_lun,
     &       '(''MF_INITOE: htrefl='',2(5x,2f12.4))')
     &           htrefl(1),htrefl(2)
         write(lwpcLOG_lun,
     &       '(''MF_INITOE: eigrfh='',2(5x,2f12.4))')
     &           eigrfh(1),eigrfh(2)
      end if

      etol=10.*rtol

      do l=1,4

         do k=1,4
            roe(k)=EXP(rgiven(k,l))
         end do

c        EIGPLS and EIGMNS are the two eigen values of the reflection
c        matrix at the L-th corner.

         arg=(roe(1)-roe(4))**2+4.*roe(2)*roe(3)
         rt=SQRT(arg)
         sm=roe(1)+roe(4)
         if (ABS(sm+rt) .lt. ABS(sm-rt)) rt=-rt
         eigpls=sm+rt
         eigmns=sm-rt

         rtgivn=rt
         delt=thetag(l)-theta0

c        EPCOMP and EMCOMP are the two eigen values of the least-squares
c        reflection matrix. For this matrix, the third-order coefficient
c        of each element is omitted and the first-order coefficient was
c        found by a least squares criterion.

         do k=1,4
            rcomp(k)=((d2rdt2(k)*.5)*delt+drlsq(k))*delt+r0(k)
            roe(k)=EXP(rcomp(k))
         end do

         arg=(roe(1)-roe(4))**2+4.*roe(2)*roe(3)
         rt=SQRT(arg)
         if (ABS(rt+rtgivn) .lt. ABS(rt-rtgivn)) rt=-rt
         sm=roe(1)+roe(4)
         epcomp=sm+rt
         emcomp=sm-rt

c        If the relative difference in the large eigen values is greater
c        than 10 times RTOL or if the difference in the smaller eigen
c        values is too large, no further attempt to use interpolated
c        reflection coefficients for this second rectangle is done.
c        Either routine LAGRNG will be used for it or the rectangle is
c        divided into two smaller ones by routine BVNDRYS and entry
c        INITOE is tried again.

         size=(ABS(eigpls)+ABS(epcomp))*.5
         delp=ABS(eigpls-epcomp)/size
         delm=ABS(eigmns-emcomp)/size

         if (delp .gt. etol .or. delm .gt. 3.*etol) notlin=1

         if (mrprnt .ne. 0) then
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: theta ='',2f7.3)')
     &              thetag(l)
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: rgiven='',4(f13.5,f12.5))')
     &             (rgiven(k,l),k=1,4)
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: rcomp ='',4(f13.5,f12.5))')
     &             (rcomp(k),k=1,4)
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: eig   ='',2(f13.5,f12.5))')
     &              eigpls,eigmns
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: ecomp ='',2(f13.5,f12.5))')
     &              epcomp,emcomp
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: diff  ='',  f13.5,f12.5 )')
     &              delp,delm
            write(lwpcLOG_lun,
     &          '(''MF_INITOE: etol  ='',f8.5,''  notlin='',i3)')
     &              etol,notlin
         end if
      end do
c     The reference height is always set to HTNTRP for interpolation in
c     this routine.
      d=htntrp

c     Set up heights for FSINTG
      z0=htntrp
      zz=d
      RETURN
      END      ! MF_INITOE