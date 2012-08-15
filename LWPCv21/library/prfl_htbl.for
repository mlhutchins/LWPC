      SUBROUTINE PRFL_HTBL
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,beta,hprime)

c***********************************************************************

c     Get tabular profile from specified file

c***********************************************************************

      include       'lwpc_cfg.cmn'

      real           hten,algen,htnu,algnu,charge,ratiom,
     &               select_hgts,hgts

      common/lwpc_pr/hten(51),algen(51,3),nrhten,ihten,
     &               htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &               charge(3),ratiom(3),nrspec,lu_prf,
     &               select_hgts(2),hgts(3)

c     Local
      character*(*)  prfl_file,prfl_id
      character* 40  prfl_model
      character*120  string,
     &               file_name,directory,root_file,extension
      logical        initialize
      integer        str_length,
     &               print_hgts


      if (initialize) then

c        Get file name
         file_name=prfl_file
         call DECODE_FILE_NAME
     &       (file_name,directory,root_file,extension)

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

c        Open originally specified file
         if (STR_LENGTH(directory) .eq. 0) then
            if (STR_LENGTH(lwpcPRF_loc) .eq. 0) then
               file_name=root_file(:STR_LENGTH(root_file))//'.prf'
            else
               file_name=lwpcPRF_loc(:STR_LENGTH(lwpcPRF_loc))//
     &                   root_file(:STR_LENGTH(root_file))//'.prf'
            end if
         else
            file_name=directory(:STR_LENGTH(directory))//
     &                root_file(:STR_LENGTH(root_file))//'.prf'
         end if

         call PRFL_INIT_TABLE
     &       (lu_prf,file_name,
     &        string,prfl_model,
     &        nrspec,charge,ratiom,
     &        51,nrhten,hten,algen,
     &           nrhtnu,htnu,algnu)

c        Re-structure the profile identification originally obtained
c        from PRFL_TABLE.
         prfl_id='Homogeneous profile: '//string(:STR_LENGTH(string))

c        Get an h' for this profile to be used by LWP_PATH.
         call PRFL_HGTS (print_hgts)

c        Store profile parameters for later use by PRFL_SPECIFICATION.
         bb=99.
         hh=AINT( 10.*hgts(2)+.5)/10.
      end if

      beta=bb
      hprime=hh

      RETURN
      END      ! PRFL_HTBL