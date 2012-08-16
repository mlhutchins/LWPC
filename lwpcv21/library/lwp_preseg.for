      SUBROUTINE LWP_PRESEG
     &          (month,day,year,UT)

c***********************************************************************

c     Builds propagation path parameters using a presegmented list.
c     The coordinates of the position are always computed using the
c     distance specified.  If the parameters of the geomagnetic field
c     are all zero, then they are computed.  If the ground conductivity
c     index is zero, then the values are determined from the map.  If
c     the ground conductivity index is not zero, then the values are
c     simply retrieved from the list of ground conductivites used by
c     the program which means that the conductivity need not match that
c     of the same point on the earth's surface.  If the profile index
c     is zero, then the values are determined from the LWPM model based
c     on the solar zenith angle and the geomagnetic dip angle.  If the
c     index is not zero, then the beta and hprime is retrieved from the
c     list of beta and hprime used to define the LWPM model.

c  Change history:

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxsgmnt=201)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real          freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex

      real          dst,xla,xlo,azm,xdp,fld,sgm,eps,bta,hpr

      common/mf_sw_1/
     &              dst(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt

      real          chi

      common/mf_sw_4/
     &              chi(mxsgmnt)

      integer       print_swg
      real     *  4 drmin,drmax

      common/sw_path/
     &              drmin,drmax,mdir,lost,lx,print_swg

c     List of LWPM ground conductivities
      common/grnd$data/
     &              ss(10),ee(10)

c     List of LWPM ionospheres
      real          lwpm_zz,lwpm_dd,lwpm_bb,lwpm_hh

      common/ prfl$/lwpm_zz(41),lwpm_dd(41),lwpm_bb(41),lwpm_hh(41),
     &              lwpm_nrz,lwpm_nrd

      character* 12 prfl_file
      integer       month,day,year,UT
      real          lng,lng1

      data          alt/80./,dtr/.01745329252/,rtk/6366.197/


      if (print_swg .gt. 0) then
         write(lwpcLOG_lun,
     &       '(/''Path summary: '',
     &          ''bearng='',f5.1,'' range='',f6.0)')
     &             bearng,rhomx
         write(lwpcLOG_lun,
     &       '(/''  nr     rho    lat     lon   azim   dip'',
     &          '' nc  sigma    chi np  beta   h'''''')')
      end if

      tlng=tlon*dtr
      tclt=(90.-tlat)*dtr
      ctclt=COS(tclt)
      stclt=SIN(tclt)
      xtr=bearng*dtr
      sinxtr=SIN(xtr)

c     Load LWPM ground conductivity values
      call GROUND (tlon,tlat,ncode,sigma,epsr)

c     Get sub-solar point
      UT_hours=(UT-(UT/100)*40)/60.
      call ALMNAC (year,month,day,UT_hours,ssclt,sslng)
      cssclt=COS(ssclt)
      sssclt=SIN(ssclt)

      do nsgmnt=1,nrsgmnt

         rho   =dst(nsgmnt)
         azim  =azm(nsgmnt)
         dip   =xdp(nsgmnt)
         bfield=fld(nsgmnt)
         ncode =ncd(nsgmnt)
         sigma =sgm(nsgmnt)
         epsr  =eps(nsgmnt)
         pindex=npr(nsgmnt)
         beta  =bta(nsgmnt)
         hprime=hpr(nsgmnt)

         if (rho .eq. 0.) then

            lon=tlon
            lat=tlat
            bear=bearng

            gcd=0.
            lng=tlng
            clt=tclt
            cclt=ctclt
            sclt=stclt
         else

            gcd=rho/rtk
            call RECVR2 (tlng,tclt,ctclt,stclt,xtr,gcd,
     &                   lng,clt,cclt,sclt)

            lon=lng/dtr
            clat=clt/dtr
            lat=90.-clat

            call GCDBR2 (tlng-lng,tclt,ctclt,stclt,clt,cclt,sclt,
     &                   gcd,br,1)
            bear=br/dtr
         end if

c        Calculate signed solar zenith angle;
c        -180<CHI<0 is midnight to noon; 0<CHI<180 is noon to midnight.
         call GCDBR2 (lng-sslng,clt,cclt,sclt,
     &                ssclt,cssclt,sssclt,zn,br,0)

         call RECVR2 (tlng,tclt,ctclt,stclt,xtr,gcd+.01,
     &                lng1,clt1,cclt1,sclt1)

         call GCDBR2 (lng1-sslng,clt1,cclt1,sclt1,
     &                ssclt,cssclt,sssclt,zn1,br,0)

         if (zn1-zn .lt. 0.) zn=-zn
         if (sinxtr .lt. 0.) zn=-zn
         zenith=zn/dtr

         xla(nsgmnt)=lat
         xlo(nsgmnt)=lon
         chi(nsgmnt)=zenith

         if (azim .eq. 0. .and. dip .eq. 0. .and. bfield .eq. 0.) then

            call NEWMAG (0,alt,lng,clt,bmf,d,bfield,br,bp,bt)
            azim=bear-bmf/dtr
            dip=d/dtr
            if (azim .lt.   0.) then
               azim=azim+360.
            else
     &      if (azim .ge. 360.) then
               azim=azim-360.
            end if

            azm(nsgmnt)=azim
            xdp(nsgmnt)=dip
            fld(nsgmnt)=bfield

         end if
         if (ncode .eq. 0) then

            call GROUND (lon,lat,ncode,sigma,epsr)

            ncd(nsgmnt)=ncode
            sgm(nsgmnt)=sigma
            eps(nsgmnt)=epsr
         else
     &   if (ncode .gt. 0 .and. sigma .eq. 0. .and. epsr .eq. 0.) then

c           Extract LWPM ground conductivity based on conductivity index
            sigma=ss(ncode)
            epsr =ee(ncode)

            sgm(nsgmnt)=sigma
            eps(nsgmnt)=epsr
         end if
         if (pindex .eq. 0) then

            call PRFL_SPECIFICATION
     &          (.false.,0,
     &           prfl_file,prfl_id,pflag,
     &           freq,month,day,year,UT,
     &           lat,lon,rho,dip,zenith,
     &           hpr_mid,beta,hprime,pindex)

            npr(nsgmnt)=pindex
            bta(nsgmnt)=beta
            hpr(nsgmnt)=hprime
         else
     &   if (pindex .gt. 0 .and. beta .eq. 0. .and. hprime .eq. 0.) then

c           Extract LWPM ionospheric profile based on profile index
            beta  =lwpm_bb(pindex)
            hprime=lwpm_hh(pindex)

            bta(nsgmnt)=beta
            hpr(nsgmnt)=hprime
         end if

         if (print_swg .gt. 0) then
            write(lwpcLOG_lun,
     &          '(i4,f9.0,f7.2,f8.2,2f6.1,
     &            i3,1pe7.0,0pf7.1,
     &            i3,f6.2,f6.1)')
     &            nsgmnt,rho,lat,lon,azim,dip,
     &            ncode,sigma,zenith,
     &            pindex,beta,hprime
         end if
      end do

      if (print_swg .gt. 0) write(lwpcLOG_lun,'('' '')')

      RETURN
      END      ! LWP_PRESEG