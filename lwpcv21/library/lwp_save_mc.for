      SUBROUTINE LWP_SAVE_MC
     &          (begin_file)

c***********************************************************************

c     Loops over path segments and writes to output file

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     12 Dec 96     Modified to use new generic flush unit routine.
c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxeigen=50,mxprm1=21,mxprm2=5,mxsgmnt=201)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex

      real     *  4 ranger,rangei,atnmax,lub,h
      complex  *  8 eigen

      common/lwpc_mf/
     &              ranger(2),rangei(2),atnmax,lub,h,
     &              eigen(mxeigen),nreigen

      common/mf_sw_1/
     &              dst(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt

      complex  *  8 save_tp,save_capt,save_fofr

      common/mf_sw_2/
     &              save_tp  (mxsgmnt,  mxeigen),
     &              save_capt(mxsgmnt,4,mxeigen),
     &              save_fofr(mxsgmnt,  mxeigen)

      integer       print_swg
      real     *  4 drmin,drmax

      common/sw_path/
     &              drmin,drmax,mdir,lost,lx,print_swg

      complex  *  8 tp,capt,fofr

      common/sw_wgou/
     &              tp(mxeigen),capt(4,mxeigen),fofr(mxeigen),lu_mds

      logical       begin_file
      real     *  4 param1(mxprm1)
      complex  *  8 param2(mxprm2,mxeigen)


      if (print_swg .gt. 0) write(lwpcLOG_lun,'(/''[LWP_SAVE_MC]'')')

      do nsgmnt=1,nrsgmnt
         lat    =xla(nsgmnt)
         lon    =xlo(nsgmnt)
         rho    =dst(nsgmnt)
         azim   =azm(nsgmnt)
         dip    =xdp(nsgmnt)
         bfield =fld(nsgmnt)
         sigma  =sgm(nsgmnt)
         epsr   =eps(nsgmnt)
         beta   =bta(nsgmnt)
         hprime =hpr(nsgmnt)
         pindex =npr(nsgmnt)
         nreigen=num(nsgmnt)

         param1( 1)=lat
         param1( 2)=lon
         param1( 3)=rho
         param1( 4)=azim
         param1( 5)=dip
         param1( 6)=bfield
         param1( 7)=sigma
         param1( 8)=epsr
         param1( 9)=beta
         param1(10)=hprime
         param1(11)=pindex
         nrprm1=11

         do neigen=1,nreigen
            eigen (  neigen)=save_tp  (nsgmnt,  neigen)
            param2(1,neigen)=save_capt(nsgmnt,1,neigen)
            param2(2,neigen)=save_capt(nsgmnt,2,neigen)
            param2(3,neigen)=save_capt(nsgmnt,3,neigen)
            param2(4,neigen)=save_capt(nsgmnt,4,neigen)
            param2(5,neigen)=save_fofr(nsgmnt,  neigen)
         end do
         nrprm2= 5

         call WRITE_MDS
     &       (lu_mds,print_swg,
     &        bearng,rhomx,rlat,rlon,rrho,
     &        mxprm1,nrprm1,param1,
     &        mxeigen,nreigen,eigen,
     &        mxprm2,nrprm2,param2,
     &        begin_file)
      end do

c     Write dummy record to indicate end of path
      param1(1)=99.
      nrprm1   =1
      nreigen  =0
      nrprm2   =0
      call WRITE_MDS
     &    (lu_mds,print_swg,
     &     bearng,rhomx,rlat,rlon,rrho,
     &     mxprm1,nrprm1,param1,
     &     mxeigen,nreigen,eigen,
     &     mxprm2,nrprm2,param2,
     &     begin_file)

      call FLUSH_UNIT (lu_mds)

      RETURN
      END      ! LWP_SAVE_MC