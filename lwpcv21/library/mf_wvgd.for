      SUBROUTINE MF_WVGD

c***********************************************************************

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     26 Apr 96     Initialize direction cosines.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxeigen=50,mxeigenb=25,mxboxes=200,mxcorners=400,
     &               mxkhts=401)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      integer        pflag,pindex,rprnt,xprnt
      real           lat,lon,lub
      complex        eigen,tcornr,rcornr,eigenb,
     &               f,dfdt,udfdt,g,q

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex
     &      /lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_crnr/tcornr(mxcorners),rcornr(4,mxcorners),nrcorner
     &      /mf_drcs/dcl,dcm,dcn,ec,g,q
     &      /mf_eigb/eigenb(mxeigenb),nreigenb
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_list/listno(mxboxes),listbx(mxeigen),listke(mxeigen)
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rkht/rkhts(mxkhts),nrkhts
     &      /mf_side/tlft,trht,ttop,tbot
 
 
c     Initialization
      dcl= COS(dip*0.01745329252)*COS(azim*0.01745329252)
      dcm= COS(dip*0.01745329252)*SIN(azim*0.01745329252)
      dcn=-SIN(dip*0.01745329252)
      if (dcl .eq. 0.) dcl=1.e-15
      if (dcm .eq. 0.) dcm=1.e-15
      if (dcn .eq. 0.) dcn=1.e-15

      call MF_INITEV
      call MF_BNDRYS

      nreigen=0
      nrcorner=0
      noevct=-1

      rkhts(1)=topht
      rkhts(2)=botht
      nrkhts=2

      unitr=adjmsh
c     UNITI is ADJMSH/SQRT(3)
      uniti=adjmsh*.577350269

      kb=1
20    trht=boxrht(kb)
      tlft=boxlft(kb)
      ttop=boxtop(kb)
      tbot=boxbot(kb)
      width= boxrht(kb)-boxlft(kb)
      height=boxtop(kb)-boxbot(kb)

c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_WVGD: Box'',i3,3x,''theta real='',f7.2,1x,
c     &         ''to'',f7.2,3x,''theta imag='',f7.2,'' to'',f7.2)')
c     &           kb,tlft,trht,ttop,tbot
c   MLH commented out for Matlab processing of log file

      call MF_WHICH
            
      if (noevct .ne. 0) go to 22

      call MF_INITOE

      if (nointg .ne. 0) go to 51

      if (notlin .eq. 0) go to 24

      if (width .lt. wdchng .and. height .lt. wdchng) noevct=1

      if (noevct .ne. 0) go to 21

      notlin=0
            
      call MF_SUBDIV
      if (nodivd .eq. 1) go to 51
      if (nodivd .eq. 2) go to 56
c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_WVGD: NOTLIN set - box divided'')')
c   MLH commented out for Matlab processing of log file

      go to 20

21    lprnt=lprnt
c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_WVGD: NOTLIN set - MF_LAGRNG instead'')')
22     call MF_INITLG
c      call MF_INITLG
      if (nointg .ne. 0) go to 51

24    listno(kb)=noevct

      kexact=0
      call MF_FZEROS

      if (nointg .ne. 0) go to 51

      if (nomesh .eq. 0) go to 26
      nomesh=0
      call MF_SUBDIV
      if (nodivd .eq. 1) go to 51
      if (nodivd .eq. 2) go to 56
      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_WVGD: NOMESH set - box divided'')')
      go to 20

26    if (nreigenb .eq. 0) go to 52

      if (noevct .ne. 0 .or. iexact .ne. 0) kexact=1
      if (kexact .ne. 0) then
c        Set up heights for FSINTG
         z0=botht
         zz=d
         call MF_FINAL
      end if

      if (nofinl .eq. 0) go to 28
      nofinl=0
      call MF_SUBDIV
      if (nodivd .eq. 1) go to 51
      if (nodivd .eq. 2) go to 56
c     MLH - taken out for consistant log reporting
c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_WVGD: NOFINL set - box divided'')')
      go to 20

28    d=(topht+botht)*.5
c     Set up heights for FSINTG
      if (kexact .eq. 0) then
         z0=htntrp
         zz=d
      else
         z0=botht
         zz=d
      end if

      jfirst=1
      do jb=1,nreigenb
         call MF_FDFDT (eigenb(jb),f,dfdt)
         udfdt=dfdt/ABS(dfdt)
Change 22 Feb 1994: Dropped printing list of eigens
c         if (lprnt .gt. 0 .or. mrprnt .gt. 0) then
c            if (noevct .ne. 0)
c     &         write(lwpcLOG_lun,
c     &             '(''MF_WVGD: ROE not used'')')
c            write(lwpcLOG_lun,
c     &          '(''MF_WVGD: '',2i3,2f8.3,10x,2f8.3)')
c     &              nreigen+1,jb,eigenb(jb),udfdt
c         end if
c   MLH commented out for Matlab processing of log file

         if (nreigen .eq. mxeigen) then
Change 22 Feb 1994: Test print flags before printing this message
            if (lprnt .gt. 0 .or. mrprnt .gt. 0)
     &         write(lwpcLOG_lun,
     &             '(''MF_WVGD: Search truncated - '',
     &               ''nreigen .GE. mxeigen'')')
            go to 58
         else
           nreigen=nreigen+1
           eigen(nreigen)=eigenb(jb)
           listbx(nreigen)=kb
         end if
      end do
      go to 54

51    if (lprnt .ne. 0 .or. mrprnt .ne. 0) then
c   MLH commented out for Matlab processing of log file

c         write(lwpcLOG_lun,
c     &       '(''MF_WVGD: Box'',i3,3x,''theta real='',f7.2,1x,
c     &         ''to'',f7.2,3x,''theta imag='',f7.2,'' to'',f7.2)')
c     &           kb,tlft,trht,ttop,tbot
         write(lwpcLOG_lun,
     &       '(''         Search in this box skipped - ''
     &         ''modes may be missed'')')
      
      end if
            
      nointg=0
      go to 54

52    lprnt=lprnt
c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''         No modes found in this box'')')
c   MLH commented out for Matlab processing of log file

54    kb=kb+1
      if (kb .le. nrboxs) go to 20
      go to 60

56    nodivd=nodivd
c       write(lwpcLOG_lun,
c     &    '(''         Search truncated - modes may be missed'')')
c      write(lwpcLOG_lun,
c     &    '(''MF_WVGD: Box'',i3,3x,''theta real='',f7.2,1x,
c     &      ''to'',f7.2,3x,''theta imag='',f7.2,'' to'',f7.2)')
c     &        kb,tlft,trht,ttop,tbot
c   MLH commented out for Matlab processing of log file

58    nodivd=0

60    if (nreigen .gt. 0) then
         if (nreigen .gt. 1) call MF_WGSORT
c        Store flag for use by MF_SAVEMC
         do j=1,nreigen
            if (listno(listbx(j)) .eq. 0 .and. iexact .eq. 0) then
               listke(j)=0
            else
               listke(j)=1
            end if
         end do
      else
         if (lprnt .ne. 0 .or. mrprnt .ne. 0)
     &      write(lwpcLOG_lun,
     &          '(''MF_WVGD: No modes in those boxes '',
     &            ''successfully searched'')')
      end if
      
      RETURN
      END      ! MF_WVGD