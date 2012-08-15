      SUBROUTINE PRFL_RTBL
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,rho,beta,hprime,pindex)

c***********************************************************************

c     Set up tabular profiles vs. range

c***********************************************************************

      include       'lwpc_cfg.cmn'

      real           hten,algen,htnu,algnu,charge,ratiom,
     &               select_hgts,hgts

      common/lwpc_pr/hten(51),algen(51,3),nrhten,ihten,
     &               htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &               charge(3),ratiom(3),nrspec,lu_prf,
     &               select_hgts(2),hgts(3)

c     Local
      parameter     (mxsgmnt=201)

      character*(*)  prfl_file,prfl_id
      character* 40  prfl_model
      character*120  string,
     &               file_name,directory,root_file,extension
      character*200  error_msg
      logical        initialize,flag
      integer        str_length,
     &               print_hgts,pindex,prev_ndx
      real           item(2)

      dimension      rng(mxsgmnt),ndx(mxsgmnt)


      if (initialize) then

c        Get file name
         file_name=prfl_file
         call DECODE_FILE_NAME
     &       (file_name,directory,root_file,extension)

c        Check for a file named "PRFL_FILE".NDX
         if (STR_LENGTH(directory) .eq. 0) then
            if (STR_LENGTH(lwpcNDX_loc) .eq. 0) then
               file_name=root_file(:STR_LENGTH(root_file))//'.ndx'
            else
               file_name=lwpcNDX_loc(:STR_LENGTH(lwpcNDX_loc))//
     &                   root_file(:STR_LENGTH(root_file))//'.ndx'
            end if
         else
            file_name=directory(:STR_LENGTH(directory))//
     &                root_file(:STR_LENGTH(root_file))//'.ndx'
         end if

         INQUIRE (file=file_name,exist=flag)
         if (flag) then

            OPEN (lu_prf,file=file_name,status='old')

            nrsgmnt=0
            do n=1,mxsgmnt

               read (lu_prf,'(a)',end=9) string

c              Check for comment line
               if (string(1:1) .ne. ';' .and.
     &             STR_LENGTH(string) .gt. 0) then

c                 Segment data should contain: range pindex
                  call DECODE_LIST_FLT (string,2,nritem,item)
                  if (nritem .ne. 2) then
                     write(error_msg,
     &                   '(''[PRFL_RTBL]: '',
     &                     ''Range and index not all input'')')
                     call LWPC_ERROR('ERROR', error_msg)
                  end if

                  nrsgmnt=nrsgmnt+1
                  rng(nrsgmnt)=item(1)
                  ndx(nrsgmnt)=item(2)
               end if
            end do

9           CLOSE(lu_prf)
         else

            write(error_msg,
     &          '(''[PRFL_RTBL]: '',
     &            ''Index file not found: '',a)')
     &              prfl_file(:MAX(1,STR_LENGTH(prfl_file)))
            call LWPC_ERROR('ERROR', error_msg)
         end if

c        Check for a file named "PRFL_FILE"000.PRF.
c        NOTE that if this file doesn't exist, then PRFL_INIT_TABLE
c        simply initializes the profile parameters.
         if (STR_LENGTH(directory) .eq. 0) then
            if (STR_LENGTH(lwpcPRF_loc) .eq. 0) then
               file_name=root_file(:STR_LENGTH(root_file))//'000.prf'
            else
               file_name=lwpcPRF_loc(:STR_LENGTH(lwpcPRF_loc))//
     &                   root_file(:STR_LENGTH(root_file))//'000.prf'
            end if
         else
            file_name=directory(:STR_LENGTH(directory))//
     &                root_file(:STR_LENGTH(root_file))//'000.prf'
         end if

         call PRFL_INIT_TABLE
     &       (lu_prf,file_name,
     &        string,prfl_model,
     &        nrspec,charge,ratiom,
     &        51,nrhten,hten,algen,
     &           nrhtnu,htnu,algnu)

c        Make sure that the model name is upper case
         call STR_UPPER (prfl_model,0,0)

c        Re-structure the profile identification originally obtained
c        from PRFL_INIT_TABLE.
         prfl_id='Rng Tbl: '//string(:STR_LENGTH(string))

c        Set previous index
         prev_ndx=0

      else

c        Find the profile index
         nsgmnt=1
         flag=.true.
         do while (flag)
            if (rng(nsgmnt) .le. rho .and.
     &         rho .lt. rng(nsgmnt+1)) then
               flag=.false.
            else
               nsgmnt=nsgmnt+1
               if (nsgmnt .eq. nrsgmnt) flag=.false.
            end if
         end do
         pindex=ndx(nsgmnt)

         if (pindex .ne. prev_ndx) then

c           Check for a file named "PRFL_FILE"nnn.PRF
            if (STR_LENGTH(directory) .eq. 0) then
               if (STR_LENGTH(lwpcPRF_loc) .eq. 0) then
                  write(file_name,'(a,i3.3,''.prf'')')
     &                  root_file(:STR_LENGTH(root_file)),pindex
               else
                  write(file_name,'(a,a,i3.3,''.prf'')')
     &                  lwpcPRF_loc(:STR_LENGTH(lwpcPRF_loc)),
     &                  root_file(:STR_LENGTH(root_file)),pindex
               end if
            else
               write(file_name,'(a,a,i3.3,''.prf'')')
     &               directory(:STR_LENGTH(directory)),
     &               root_file(:STR_LENGTH(root_file)),pindex
            end if

            INQUIRE (file=file_name,exist=flag)
            if (flag) then

               if (prfl_model(1:1) .eq. 'F') then

c                 Formatted profile
                  call PRFL_INIT_TABLE
     &                (lu_prf,file_name,
     &                 string,prfl_model,
     &                 nrspec,charge,ratiom,
     &                 51,nrhten,hten,algen,
     &                    nrhtnu,htnu,algnu)
               else
     &         if (prfl_model(1:1) .eq. 'U') then

c                 Unformatted profile
                  OPEN (lu_prf,file=file_name,status='old',
     &                         form='unformatted')

                  read (lu_prf) nrsp,nrhten,
     &                         (hten(n),
     &                         (algen(n,k),k=1,nrsp),n=1,nrhten)

                  CLOSE(lu_prf)
               else

                  write(error_msg,
     &                '(''[PRFL_RTBL]: '',
     &                  ''PRFL_MODEL: '',a,'' is invalid'')')
     &                    prfl_model(:MAX(1,STR_LENGTH(prfl_model)))
                  call LWPC_ERROR('ERROR', error_msg)
               end if
            else

               write(error_msg,
     &             '(''[PRFL_RTBL]: '',
     &               ''Profile file not found: '',a)')
     &                 prfl_file(:MAX(1,STR_LENGTH(prfl_file)))
               call LWPC_ERROR('ERROR', error_msg)
            end if

c           Get reference heights for current profile.
            call PRFL_HGTS (print_hgts)

            bb=99.
            hh=AINT( 10.*hgts(2)+.5)/10.

c           Set previous index
            prev_ndx=pindex

         end if

         beta=bb
         hprime=hh
      end if

      RETURN
      END      ! PRFL_RTBL