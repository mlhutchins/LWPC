      SUBROUTINE PRFL_REXP
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,rho,beta,hprime,pindex)

c***********************************************************************

c     Set up exponential profiles vs. range

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
      logical        initialize
      integer        print_hgts,pindex

      character*120  string,file_name,directory,root_file,extension
      character*200  error_msg
      logical        flag
      integer        str_length,
     &               prev_ndx
      real           item(3)

      dimension      rng(mxsgmnt),bta(mxsgmnt),hpr(mxsgmnt)


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

c                 Segment data should contain: range beta hprime
                  call DECODE_LIST_FLT (string,3,nritem,item)
                  if (nritem .ne. 3) then
                     write(error_msg,
     &                   '(''[PRFL_REXP]: '',
     &                     ''Range, beta and hprime not all input'')')
                     call LWPC_ERROR('ERROR', error_msg)
                  end if

                  nrsgmnt=nrsgmnt+1
                  rng(nrsgmnt)=item(1)
                  bta(nrsgmnt)=item(2)
                  hpr(nrsgmnt)=item(3)
               end if
            end do

9           CLOSE(lu_prf)
         else

            write(error_msg,
     &          '(''[PRFL_REXP]: '',
     &            ''Index file not found: '',a)')
     &              prfl_file(:MAX(1,STR_LENGTH(prfl_file)))
            call LWPC_ERROR('ERROR', error_msg)
         end if

c        Define a profile id
         prfl_id='Rng Exp: '//root_file(:STR_LENGTH(root_file))

c        Set previous index
         prev_ndx=0
         prev_bta=0.
         prev_hpr=0.

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
         pindex=nsgmnt
         beta  =bta(pindex)
         hprime=hpr(pindex)

         if (beta .ne. prev_bta .or. hprime .ne. prev_hpr) then

c           Set up profile.
            call PRFL_EXP (beta,hprime)

c           Get reference heights for this profile.
            call PRFL_HGTS (print_hgts)

c           Set previous index
            prev_ndx=pindex
            prev_bta=beta
            prev_hpr=hprime

         end if
      end if

      RETURN
      END      ! PRFL_REXP