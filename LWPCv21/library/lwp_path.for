      SUBROUTINE LWP_PATH
     &          (month,day,year,UT)

c***********************************************************************

c  Change history:

c     21 Oct 95     Added printing of path summary when PRINT_SWG > 0.

c     19 May 96     Modified to allow smaller minimum segment length to
c                   accommodate shorter terminators.

c     12 Nov 96     Modified to allow paths longer than 20000 km.

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

      character* 12 prfl_file
      logical       save
      integer       month,day,year,UT
      real          lng,lng1

      data          alt/80./,dtr/.01745329252/,rtk/6366.197/,drho/20./

      tlng=tlon*dtr
      tclt=(90.-tlat)*dtr
      ctclt=COS(tclt)
      stclt=SIN(tclt)

      nrsgmnt=0
      rho=0.
      gcd=0.
      lng=tlng
      clt=tclt
      cclt=ctclt
      sclt=stclt
      bear=bearng
      xtr=bear*dtr
      sinxtr=SIN(xtr)

c     Get sub-solar point
      UT_hours=(UT-(UT/100)*40)/60.
      call ALMNAC (year,month,day,UT_hours,ssclt,sslng)
      cssclt=COS(ssclt)
      sssclt=SIN(ssclt)

      last=0
      do while (last .lt. 2)
         lon=lng/dtr
         clat=clt/dtr
         lat=90.-clat

         call NEWMAG (0,alt,lng,clt,bmf,d,bfield,br,bp,bt)
         azim=bear-bmf/dtr
         dip=d/dtr
         if (azim .lt.   0.) then
            azim=azim+360.
         else
     &   if (azim .ge. 360.) then
            azim=azim-360.
         end if
         
         call GROUND (lon,lat,ncode,sigma,epsr)
         
c        Calculate signed solar zenith angle;
c        -180<CHI<0 is midnight to noon; 0<CHI<180 is noon to midnight.
         call GCDBR2 (lng-sslng,clt,cclt,sclt,ssclt,cssclt,sssclt,
     &                zn,br,0)

         call RECVR2 (tlng,tclt,ctclt,stclt,xtr,gcd+.01,
     &                lng1,clt1,cclt1,sclt1)

         call GCDBR2 (lng1-sslng,clt1,cclt1,sclt1,ssclt,cssclt,sssclt,
     &                zn1,br,0)

         if (zn1-zn .lt. 0.) zn=-zn
         if (sinxtr .lt. 0.) zn=-zn
         zenith=zn/dtr

         call PRFL_SPECIFICATION
     &       (.false.,0,
     &        prfl_file,prfl_id,pflag,
     &        freq,month,day,year,UT,
     &        lat,lon,rho,dip,zenith,
     &        hpr_mid,beta,hprime,pindex)

         if (nrsgmnt .eq. 0) then

            nrsgmnt=1
            num(1)=0
            dst(1)=0.
            xla(1)=lat
            xlo(1)=lon
            azm(1)=azim
            xdp(1)=dip
            fld(1)=bfield
            sgm(1)=sigma
            eps(1)=epsr
            ncd(1)=ncode
            chi(1)=zenith
            bta(1)=beta
            hpr(1)=hprime
            npr(1)=pindex

            azm1=azim
            dip1=dip
            ncd1=ncode
            hpr1=hprime
            ndx1=pindex

            if (print_swg .gt. 0) then

               write(lwpcLOG_lun,
     &             '(/''Path summary: '',
     &                ''bearng='',f5.1,'' range='',f6.0)')
     &                  bearng,rhomx
               write(lwpcLOG_lun,
     &             '(/''   nr    rho    lat     lon   azim   dip'',
     &                ''  sigma    chi  beta   h'''''')')
               write(lwpcLOG_lun,
     &             '(i4,f9.0,f7.2,f8.2,2f6.1,1pe7.0,0pf7.1,
     &               f6.2,f6.1)')
     &               nrsgmnt,rho,lat,lon,azim,dip,
     &               sigma,zenith,beta,hprime
            end if
         else

            save=.false.
            if (ncode .ne. ncd1 .or.
     &          hprime .ne. hpr1 .or. pindex .ne. ndx1) then

c              Conductivity or profile changed
               save=.true.
               if (nrsgmnt .gt. 2) then
c                 Check for a very short segment just before this one
                  if (rho-dst(nrsgmnt) .lt. 40.) then
                     if (sgm(nrsgmnt-1) .eq. sgm(nrsgmnt-2) .and.
     &                   hpr(nrsgmnt-1) .eq. hpr(nrsgmnt-2))
     &                  nrsgmnt=nrsgmnt-1
                  end if
               end if
            else

               atst=ABS(azim-azm1)
               dtst=ABS(dip-dip1)
               absd=ABS(dip)
               if (absd .lt. 80.) then
                  if (hprime .lt. hpr_mid) then
c                    Day-like case
                     if (dtst .gt. 15.) then
                        save=.true.
                     else
                        if (absd .ge. 70.) then
c                          70 le dip lt 80
                           if (atst .gt. 45.) save=.true.
                        else
     &                  if (absd .ge. 30.) then
c                          30 le dip lt 70
                           if (atst .gt. 30.) save=.true.
                        else
c                          0 le dip lt 30
                           if (atst .gt. 15.) save=.true.
                        end if
                     end if
                  else

c                    Night-like case
                     if (dtst .gt. 10.) then
                        save=.true.
                     else
                        if (absd .ge. 70.) then
c                          70 le dip lt 80
                           if (atst .gt. 20.) save=.true.
                        else
     &                  if (absd .ge. 30.) then
c                          30 le dip lt 70
                           if (atst .gt. 15.) save=.true.
                        else
c                          0 le dip lt 30
                           if (ABS(azim-190.) .lt. 20. .or.
     &                        azim .gt. 330. .or. azim .lt. 10.) then
c                             Modal degeneracy cone of azimuths
                              if (dtst .gt. 3.) save=.true.
                           else
     &                     if (dtst .gt. 5. .or. atst .gt. 15.) then
                              save=.true.
                           end if
                        end if
                     end if
                  end if
               end if
            end if

            if (print_swg .gt. 2) then

               write(lwpcLOG_lun,
     &             '(4x,f9.0,f7.2,f8.2,2f6.1,1pe7.0,0pf7.1,
     &               f6.2,f6.1)')
     &               rho,lat,lon,azim,dip,
     &               sigma,zenith,beta,hprime
            end if

            if (save .or. last .eq. 1) then

               nrsgmnt=nrsgmnt+1
               num(nrsgmnt)=0
               dst(nrsgmnt)=rho
               xla(nrsgmnt)=lat
               xlo(nrsgmnt)=lon
               azm(nrsgmnt)=azim
               xdp(nrsgmnt)=dip
               fld(nrsgmnt)=bfield
               sgm(nrsgmnt)=sigma
               eps(nrsgmnt)=epsr
               ncd(nrsgmnt)=ncode
               chi(nrsgmnt)=zenith
               bta(nrsgmnt)=beta
               hpr(nrsgmnt)=hprime
               npr(nrsgmnt)=pindex

               azm1=azim
               dip1=dip
               ncd1=ncode
               hpr1=hprime
               ndx1=pindex

               if (print_swg .gt. 0) then

                  write(lwpcLOG_lun,
     &                '(i4,f9.0,f7.2,f8.2,2f6.1,1pe7.0,0pf7.1,
     &                  f6.2,f6.1)')
     &                  nrsgmnt,rho,lat,lon,azim,dip,
     &                  sigma,zenith,beta,hprime
               end if
            end if
         end if

         rho=rho+drho

         if (rho .ge. rhomx) then
            if (last .eq. 0) then
               if (rhomx .eq. 40000.) then
                  last=2
               else
                  rho=rhomx
                  last=1
               end if
            else
               last=2
            end if
         end if
         if (rho .eq. 20000.) then

c           At the antipode of the transmitter
c           the bearing here is 540-bearing at the transmitter
            lng=tlng+3.141592653
            if (lng .gt. 6.2831853072) lng=lng-6.2831853072
            clt=3.141592653-tclt
            cclt=-ctclt
            sclt= stclt
            gcd=3.141592653
            br=9.4247779608-xtr
            if (br .gt. 6.2831853072) br=br-6.2831853072
         else

            gcd=rho/rtk
            call RECVR2 (tlng,tclt,ctclt,stclt,xtr,gcd,
     &                   lng,clt,cclt,sclt)
            call GCDBR2 (tlng-lng,tclt,ctclt,stclt,clt,cclt,sclt,
     &                   gcd,br,1)
         end if
         bear=br/dtr
      end do

      if (print_swg .gt. 0) write(lwpcLOG_lun,'('' '')')
      RETURN
      END      ! LWP_PATH