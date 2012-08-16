      SUBROUTINE PRFL_READ_TABLE
     &          (lu_prf,type,nrspec,mxhts,nrhts,htlst,algxx)

c***********************************************************************
c     Reads ionospheric profiles

c        type=1:  electron and ion densities
c             2:  collision frequencies
c***********************************************************************

      character* 80 string
      character*200 error_msg
      integer       str_length,
     &              type
      real          htlst(mxhts),algxx(mxhts,3),xx(4)


      ht=0.
      nrhts=0
      if (type .eq. 1) then

c        Read charged particle profile
         kmax=nrspec
         do while (ht .ge. 0. .and. nrhts .le. mxhts)

            read (lu_prf,'(a)') string

c           Check for comment
            if (string(1:1) .ne. ';' .and.
     &          STR_LENGTH(string) .gt. 0) then

               xx(2)=0.
               xx(3)=0.
               xx(4)=0.
               call DECODE_LIST_FLT (string,4,nrlist,xx)

               ht=xx(1)
               if (ht .ge. 0.) then

                  if (nrhts .gt. 1 .and. ht .ge. htlst(nrhts-1)) then
                     write(error_msg,
     &                   '(''[PRFL_READ_TABLE]: '',
     &                     '' Heights out of order'')')
                     call LWPC_ERROR('ERROR', error_msg)
                  endif

c                 If ions specified, then set density of second
c                 species to preserve charge neutrality.
                  if (kmax .eq. 3) xx(4)=xx(3)-xx(2)

                  nrhts=nrhts+1
                  htlst(nrhts)=ht
                  do k=1,kmax
                     algxx(nrhts,k)=LOG(MAX(xx(k+1),1.e-20))
                  end do
               end if
            end if
         end do
      else

c        Read collision frequency profile
         kmax=3
         do while (ht .ge. 0. .and. nrhts .le. mxhts)

            read (lu_prf,'(a)') string

c           Check for comment
            if (string(1:1) .ne. ';' .and.
     &          STR_LENGTH(string) .gt. 0) then

               xx(2)=0.
               xx(3)=0.
               xx(4)=0.
               call DECODE_LIST_FLT (string,4,nrlist,xx)

               ht=xx(1)
               if (ht .ge. 0.) then

                  if (nrhts .gt. 1 .and. ht .ge. htlst(nrhts-1)) then
                     write(error_msg,
     &                   '(''[PRFL_READ_TABLE]: '',
     &                     '' Heights out of order'')')
                     call LWPC_ERROR('ERROR', error_msg)
                  endif

c                 If collision frequency for second ion not specified,
c                 then make it the same as the first specie.
                  if (kmax .eq. 3 .and. xx(4) .eq. 0.) xx(4)=xx(3)

                  nrhts=nrhts+1
                  htlst(nrhts)=ht
                  do k=1,kmax
                     algxx(nrhts,k)=LOG(MAX(xx(k+1),1.e-20))
                  end do
               end if
            end if
         end do
      end if
      if (ht .ge. 0.) then
         write(error_msg,
     &       '(''[PRFL_READ_TABLE]: Too many heights in profile'')')
         call LWPC_ERROR('ERROR', error_msg)
      endif

      RETURN
      END      ! PRFL_READ_TABLE