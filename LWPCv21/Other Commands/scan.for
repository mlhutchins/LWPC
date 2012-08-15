      PROGRAM    SCAN

c Prints a summary of the contents of the files produced by the LWPC.
c This program is run interactively with a minimum of input required.

c***********************************************************************

c File types:

c HDR:
      parameter    (mxpath=120)
c LWT:
c MDS:
      parameter    (mxeigen=50,mxprm1=21,mxprm2=5)
c LWF:
      parameter    (mxps=11,mxsgmnt=201,mxpts=1002)
c GRD:
c ITS:
c NTI:
c LNP:
      parameter    (mxlat=73,mxlon=145)

c HDR:
      character*  8 archive,prgm_id,
     &              mds_format/'Binary'/,
     &              lwf_format/'Binary'/,
     &              grd_format/'ASCII'/
      character* 20 xmtr_id,path_id,area_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id(3)

      logical       begin_file,end_file

      integer       nprint,
c LWT:
c MDS:
     &              nrprm1,nrprm2,nreigen,
c LWF:
     &              nrps,nrsgmnt,
     &              nrcmp,nrpt1,nrpt2,
c GRD:
c ITS:
c NTI:
c LNP:
     &              nrlon,nrlat,nrpath,nrgrd

      real     *  4 freq,txlat,txlon,
     &              oplat1,oplon1,oplat2,oplon2,
     &              bearing(mxpath),rhomax(mxpath),
     &              rxlat(mxpath),rxlon(mxpath),
     &              bearng,rhomx,rlat,rlon,rrho,
c LWT:
c MDS:
     &              param1(mxprm1),
c LWF:
     &              sgmnt(mxps,mxsgmnt),
     &              xy(mxpts),amp_lwf(mxpts),phs_lwf(mxpts),
c GRD:
c ITS:
c NTI:
c LNP:
     &              xlat1,xlon1,xlat2,xlon2,
     &              amp_grd(mxlon,mxlat),sig_grd(mxlon,mxlat)

      complex  *  8
c LWT:
c MDS:
     &              eigen(mxeigen),param2(mxprm2,mxeigen)

c LOCAL:
      character*100 file_name
      logical       loop/.true./
      integer       str_length
      data          nprint/1/

c***********************************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'
      include      'lwpc_lun.ini'

      character* 40 CMDList(3)
      character*120 CMDLine

c     Get the location of the LWPC data
      call LWPC_DAT_LOC

c     Get command line
      call GET_COMMAND_LINE (LenCMD,CMDLine)

c     CMDLine now contains the arguments of the command line;
c     for this program we assume that there is only the input file name
c     and a print flag.
      if (LenCMD .eq. 0) then
         nf=0
         mprint=0
         file_name='scan.log'
         OPEN (lwpcLOG_lun,file=file_name,status='unknown')
      else
         call DECODE_LIST_STR (CMDLine,3,NrCMD,CMDList)
         file_name=CMDList(1)
         nf=STR_LENGTH(file_name)
         if (NrCMD .eq. 2) then
            call DECODE_LIST_INT (CMDList(2),1,nl,mprint)
         else
            mprint=0
         end if

         file_name(nf+1:nf+4)='.inp'
         OPEN (lwpcINP_lun,file=file_name,status='old')

         file_name(nf+1:nf+4)='.log'
         OPEN (lwpcLOG_lun,file=file_name,status='unknown')
      end if

c     Initialize logical units
      lu_mds=lwpcMDS_lun ! storage of mode parameters   by LWPM
      lu_lwf=lwpcLWF_lun ! storage of mode signal       by LW_VS_D
      lu_grd=lwpcGRD_lun ! storage of op area grid data by OPA_GRID

      do while (loop)

         file_name=' '
         if (nf .eq. 0) then
            write(6,'(''Enter file name (with extension): '',$)')
            read (5,'(a)',end=99) file_name
         else
            read (lwpcINP_lun,'(a)',end=99) file_name
         end if
         if (file_name(1:1) .eq. ' ') STOP

         write(6,          '(''SCAN '',a)') file_name
         write(lwpcLOG_lun,'(''SCAN '',a)') file_name

         n=STR_LENGTH(file_name)
         do while (n .gt. 0 .and. file_name(n:n) .ne. '.')
            n=n-1
         end do

         begin_file=.true.
         if (file_name(n+1:n+3) .eq. 'mds' .or.
     &       file_name(n+1:n+3) .eq. 'MDS') then

            OPEN (lu_mds,file=file_name,status='old',form='unformatted')
            if (nf .eq. 0) then
               mprint=0
               write(6,'(''MDS Print levels: 0=minimum'')')
               write(6,'(''                  1=mode parameters'')')
               write(6,'(''Enter print level ['',i1,'']: '',$)') mprint
               read (5,'(i1)') mprint
            else
               read (lwpcINP_lun,'(i1)') mprint
            end if
            call READ_HDR
     &          (lu_mds,mprint+nprint,
     &           archive,file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,txlat,txlon,
     &           path_id,oplat1,oplon1,oplat2,oplon2,
     &           mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &           begin_file,end_file,
     &           mds_format)
            do while (.not.end_file)
               call READ_MDS
     &             (lu_mds,mprint+nprint,
     &              bearng,rhomx,rlat,rlon,rrho,
     &              mxprm1,nrprm1,param1,
     &              mxeigen,nreigen,eigen,
     &              mxprm2,nrprm2,param2,
     &              begin_file,end_file)
            end do
            CLOSE(lu_mds)
         else
     &   if (file_name(n+1:n+3) .eq. 'lwt' .or.
     &       file_name(n+1:n+3) .eq. 'LWT') then

            OPEN (lu_mds,file=file_name,status='old',form='unformatted')
            if (nf .eq. 0) then
               mprint=0
               write(6,'(''LWT Print levels: 0=minimum'')')
               write(6,'(''                  1=mode parameters'')')
               write(6,'(''Enter print level ['',i1,'']: '',$)') mprint
               read (5,'(i1)') mprint
            else
               read (lwpcINP_lun,'(i1)') mprint
            end if
            call READ_HDR
     &          (lu_mds,mprint+nprint,
     &           archive,file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,txlat,txlon,
     &           path_id,oplat1,oplon1,oplat2,oplon2,
     &           mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &           begin_file,end_file,
     &           mds_format)
            do while (.not.end_file)
               call READ_MDS
     &             (lu_mds,mprint+nprint,
     &              bearng,rhomx,rlat,rlon,rrho,
     &              mxprm1,nrprm1,param1,
     &              mxeigen,nreigen,eigen,
     &              mxprm2,nrprm2,param2,
     &              begin_file,end_file)
            end do
            CLOSE(lu_mds)
         else
     &   if (file_name(n+1:n+3) .eq. 'lwf' .or.
     &       file_name(n+1:n+3) .eq. 'LWF') then

            OPEN (lu_lwf,file=file_name,status='old',form='unformatted')
            if (nf .eq. 0) then
               mprint=0
               write(6,'(''LWF Print levels: 0=minimum'')')
               write(6,'(''                  1=vs range'')')
               write(6,'(''                  2=Rx only'')')
               write(6,'(''Enter print level ['',i1,'']: '',$)') mprint
               read (5,'(i1)') mprint
            else
               read (lwpcINP_lun,'(i1)') mprint
            end if
            call READ_HDR
     &          (lu_lwf,mprint+nprint,
     &           archive,file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,txlat,txlon,
     &           path_id,oplat1,oplon1,oplat2,oplon2,
     &           mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &           begin_file,end_file,
     &           lwf_format)
            do while (.not.end_file)
               npt2=0
               nrpt2=1
               do while (npt2 .lt. nrpt2)
                  ncmp=0
                  nrcmp=1
                  do while (ncmp .lt. nrcmp)
                     call READ_LWF
     &                   (lu_lwf,mprint+nprint,
     &                    bearng,rhomx,rlat,rlon,rrho,
     &                    mxps,nrps,mxsgmnt,nrsgmnt,sgmnt,
     &                    mxprm1,nrprm1,param1,nrcmp,nrpt2,
     &                    mxpts,nrpt1,
     &                    xy,amp_lwf,phs_lwf,
     &                    begin_file,end_file)
                     ncmp=ncmp+1
                  end do
                  npt2=npt2+1
               end do
            end do
            CLOSE(lu_lwf)
         else
     &   if (file_name(n+1:n+3) .eq. 'grd' .or.
     &       file_name(n+1:n+3) .eq. 'its' .or.
     &       file_name(n+1:n+3) .eq. 'nti' .or.
     &       file_name(n+1:n+3) .eq. 'lnp' .or.
     &       file_name(n+1:n+3) .eq. 'GRD' .or.
     &       file_name(n+1:n+3) .eq. 'ITS' .or.
     &       file_name(n+1:n+3) .eq. 'NTI' .or.
     &       file_name(n+1:n+3) .eq. 'LNP') then

            if (grd_format(1:1) .eq. 'A' .or.
     &          grd_format(1:1) .eq. 'a') then
               OPEN (lu_grd,file=file_name,
     &                      status='old',form='formatted')
            else
               OPEN (lu_grd,file=file_name,
     &                      status='old',form='unformatted')
            end if
            mprint=0
            call READ_HDR
     &          (lu_grd,mprint+nprint,
     &           archive,file_id,prgm_id,
     &           case_id,prfl_id,
     &           xmtr_id,freq,txlat,txlon,
     &           path_id,oplat1,oplon1,oplat2,oplon2,
     &           mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &           begin_file,end_file,
     &           grd_format)
            do while (.not.end_file)
               npt2=0
               nrpt2=1
               do while (npt2 .lt. nrpt2)
                  ncmp=0
                  nrcmp=1
                  do while (ncmp .lt. nrcmp)
                     call READ_GRD
     &                   (lu_grd,nprint,
     &                    mxprm1,nrprm1,param1,nrcmp,nrpt2,
     &                    area_id,xlat1,xlon1,xlat2,xlon2,
     &                    mxlat,nrlat,mxlon,nrlon,
     &                    amp_grd,sig_grd,
     &                    begin_file,end_file,
     &                    grd_format)
                     if (.not.end_file) then
                        if (nrprm1 .gt. 12) then
                           nrgrd=param1(13)
                           if (nrgrd .gt. 0) then
                              do ngrd=1,nrgrd
                                 call READ_GRD
     &                               (lu_grd,0,
     &                                mxprm1,nrprm1,param1,nrcmp,nrpt2,
     &                                area_id,xlat1,xlon1,xlat2,xlon2,
     &                                mxlat,nrlat,mxlon,nrlon,
     &                                amp_grd,sig_grd,
     &                                begin_file,end_file,
     &                                grd_format)
                              end do
                           end if
                        end if
                     end if
                     ncmp=ncmp+1
                  end do
                  npt2=npt2+1
               end do
            end do
            CLOSE(lu_grd)
         else

            write(6,
     &          '(''ERROR: The file name extension is wrong'')')
            write(lwpcLOG_lun,
     &          '(''ERROR: The file name extension is wrong'')')
         end if
      end do
99    STOP
      END      ! SCAN
