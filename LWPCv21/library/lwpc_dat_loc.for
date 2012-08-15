      SUBROUTINE LWPC_DAT_LOC
**************************************************************
c This reads the location of the LWPC data file given in the file
c lwpcDAT.loc file

c 2/17/10 MLH Added comment area
c             

**************************************************************

c     LWPC parameters
      include      'lwpc_cfg.cmn'
      include      'lwpc_lun.cmn'

      character*  1 delimiter(3)
      integer       str_length

c     Get the location of the LWPC data
      open (lwpcDAT_lun,file='lwpcDAT.loc',status='old',err=900)
      read (lwpcDAT_lun,'(a)') lwpcDAT_loc
      close(lwpcDAT_lun)
c     Get delimiters used in the file names
      call GET_DELIMITER (delimiter)
c     Ensure lwpcDAT_loc is properly terminated
      n=STR_LENGTH(lwpcDAT_loc)
      if (lwpcDAT_loc(n:n) .ne. delimiter(3)) then
         n=n+1
         lwpcDAT_loc(n:n)=delimiter(3)
      end if

      RETURN

 900   call LWPC_ERROR ('Error','Could not open /lwpcDAT.loc')
      END      ! LWPC_DAT_LOC