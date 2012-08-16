      SUBROUTINE MF_BNDRYS

c***********************************************************************

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxeigen=50,mxboxes=200)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      integer        pflag,pindex,autos,rprnt,xprnt
      real           lat,lon,lub
      complex        eigen,theta,c,s,csq,ssq,ngsq,stp,tp

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex
     &      /lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_area/gmax,bxrtio,tmesh,autos
     &      /mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

      data           alpha/3.14e-04/


      bndylt=ranger(1)
      rtbndy=ranger(2)
      bndylw=rangei(2)
      tpbndy=rangei(1)

      nbr=(rtbndy-bndylt)/gmax+0.99
      width=(rtbndy-bndylt)/nbr
      nureal=width/tmesh+0.99
      adjmsh=width/nureal
      unitr=adjmsh
      uniti=adjmsh/SQRT(3.)
      hmax=bxrtio*width
      nuimax=hmax/uniti+0.01
      if (nuimax .lt. 1) nuimax=1

      wdchng=0.45*width
      if (wdchng .lt. 1.1*adjmsh) wdchng=1.1*adjmsh

Change: 09 Oct 92
c     capk=1.-0.5*alpha*h
      capk=SQRT(1.-alpha*h)
Change: END
      capa=0.0055*atnmax/freq

      k=0

      trht=rtbndy
      tlft=rtbndy-width

      do kr=1,nbr
         ttop=tpbndy

         deli=tpbndy-bndylw

         if (autos .ne. 0) then
            argnum=capa
            argden=capk*COS(trht*0.01745329252)
            if (argnum .lt. 4.9*argden) then
               sinhti=argnum/argden
               thetai=-LOG(sinhti+SQRT(sinhti**2+1.))*57.296
               if (thetai .lt. bndylw) thetai=bndylw
               deli=tpbndy-thetai
            end if
         end if

         nuiall=deli/uniti+0.99
         nbi=1
         if (deli-lub .ge. hmax) then
            nbi=(nuiall-1)/nuimax+1
            if (nbi .lt. 1) nbi=1
         end if
         nuibal=nuiall
         do ki=1,nbi
            nuimag=(nuibal-1)/(nbi-ki+1)+1
            height=nuimag*uniti
            tbot=ttop-height
            if (k .eq. mxboxes) then
               write(lwpcLOG_lun,
     &             '(''MF_BNDRYS: Too many boxes'')')
               nodivd=2
               RETURN
            end if
            k=k+1
            boxrht(k)=trht
            boxlft(k)=tlft
            boxtop(k)=ttop
            boxbot(k)=tbot
            nur(k)=nureal
            nui(k)=nuimag
            ttop=tbot
            nuibal=nuibal-nuimag
         end do

         trht=tlft
         tlft=trht-width
      end do

      if (autos .ne. 0 .and. sigma .lt. 1.e-3) then
c        Check for Brewster mode
         ngsq=CMPLX(epsr,-1.7975e7*sigma/freq)
         capa=-freq/0.0055*capk
         stp=SQRT(ngsq/(ngsq+(1.,0.)))*capk
         tp=(0.,-1.)*LOG(SQRT((1.,0.)-stp*stp)+(0.,1.)*stp)
         tpr=INT( REAL(tp)*572.957795)/10.
         tpi=INT(AIMAG(tp)*572.957795)/10.-.1

c        Set up additional box around possible Brewster mode
         rtbndy=MIN(tpr+.5*width,90.)
         nuimag=MAX(nuimax/2,1)
         height=nuimag*uniti
         tpbndy=MIN(tpi+.5*height,0.)

         ttop=tpbndy
         tbot=ttop-height
         trht=rtbndy
         tlft=rtbndy-width

c        Get attenuation rate of lower left corner of possible box
         atten=capa*COS(tlft*0.01745329252)
     &             *sinh(tbot*0.01745329252)
         if (atten .gt. atnmax) then
c           Add one more search box because this box not covered
c           by other areas
            if (k .eq. mxboxes) then
               write(lwpcLOG_lun,
     &             '(''MF_BNDRYS: Too many boxes'')')
               nodivd=2
               RETURN
            end if
            k=k+1
            boxrht(k)=trht
            boxlft(k)=tlft
            boxtop(k)=ttop
            boxbot(k)=tbot
            nur(k)=nureal
            nui(k)=nuimag
         end if
      end if
      nrboxs=k

      if (msprnt .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_BNDRYS: nrboxs='',i3)')
     &            nrboxs
         write(lwpcLOG_lun,
     &       '(i3,4f10.3,7x,2i4)')
     &        (k,boxlft(k),boxrht(k),boxtop(k),boxbot(k),
     &         nur(k),nui(k),k=1,nrboxs)
      end if
      RETURN
      END      ! MF_BNDRYS