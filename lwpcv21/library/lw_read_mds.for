      SUBROUTINE LW_READ_MDS
     &          (lu_mds,begin_file_mds,end_file_mds)

c     Reads the file which contains mode parameters along a path

c     lu_mds        INTEGER  logical unit for mode parameters
c     begin_file_mds
c                   LOGICAL  beginning of input file
c     end_file_mds  LOGICAL  end of input file

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxprm1=21,
     &              mxeigen=50,mxprm2=5,
     &              mxsgmnt=201)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht
      complex  *  8 thetap,capt,fofr

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex
     &      /mf_sw_1/
     &              rng(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt
     &      /mf_sw_2/
     &              thetap(mxsgmnt,  mxeigen),
     &              capt  (mxsgmnt,4,mxeigen),
     &              fofr  (mxsgmnt,  mxeigen)

      logical       begin_file_mds,end_file_mds
      integer       print_mds/0/
      real     *  4 param1(mxprm1)
      complex  *  8 eigen(mxeigen),param2(mxprm2,mxeigen)


      lat=0.
      nsgmnt=0
      do while (lat .ne. 99.)
         call READ_MDS (lu_mds,print_mds,
     &                  bearng,rhomx,rlat,rlon,rrho,
     &                  mxprm1,nrprm1,param1,
     &                  mxeigen,nreigen,eigen,
     &                  mxprm2,nrprm2,param2,
     &                  begin_file_mds,end_file_mds)

         if (end_file_mds) RETURN

         lat=param1(1)

         if (lat .ne. 99.) then

            nsgmnt=nsgmnt+1
            xla(nsgmnt)=param1( 1)
            xlo(nsgmnt)=param1( 2)
            rng(nsgmnt)=param1( 3)
            azm(nsgmnt)=param1( 4)
            xdp(nsgmnt)=param1( 5)
            fld(nsgmnt)=param1( 6)
            sgm(nsgmnt)=param1( 7)
            eps(nsgmnt)=param1( 8)
            bta(nsgmnt)=param1( 9)
            hpr(nsgmnt)=param1(10)
            npr(nsgmnt)=param1(11)
            num(nsgmnt)=nreigen

            if (nreigen .gt. 0) then

               do ne=1,nreigen

                  thetap(nsgmnt,  ne)= eigen(  ne)
                  capt  (nsgmnt,1,ne)=param2(1,ne) ! T1
                  capt  (nsgmnt,2,ne)=param2(2,ne) ! T2
                  capt  (nsgmnt,3,ne)=param2(3,ne) ! T3
                  capt  (nsgmnt,4,ne)=param2(4,ne) ! T4
                  fofr  (nsgmnt,  ne)=param2(5,ne) ! Ey/Hy
               end do
            end if
         end if
      end do

      nrsgmnt=nsgmnt

      RETURN
      END      ! LW_READ_MDS