      SUBROUTINE OPEN_DAT
     &          (logical_unit,fname,fform)

c***********************************************************************
c                         subroutine open_dat
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     26 Jan 1993

c  Function:
c     This routine opens a specified file that MUST be in lwpcDAT_loc or
c     in lwpcXTR_loc.

c  Parameters passed:
c     logical_unit   [i] number of the unit to be used to open the file
c     fname          [s] name of the file to be opened
c     fform          [s] FORMATTED or UNFORMATTED (only the first
c                        character is required)

c  Parameters returned:

c  Common blocks referenced:
c     lwpc_cfg

c  Functions and subroutines referenced:
c     index
c     inquire
c     open
c     rewind

c     decode_file_name
c     str_length
c     str_lower
c     str_upper

c  References:

c  Change History:

c     31 Jan 94     Added error checking on open statement.

c     05 Feb 98     Added ACTION='READ' to the open statement.

c*******************!***************************************************

      include      'lwpc_cfg.cmn'

      character*(*) fname,fform
      integer       logical_unit

      character* 12 open_format,file_format
      character* 80 directory,root_name,extension
      character*120 open_name,file_name,full_name
      character*200 error_msg
      logical       exists
      integer       str_length


c     Set local variables
      lu=logical_unit
      file_name=fname(:STR_LENGTH(fname))
      file_format=fform(:STR_LENGTH(fform))
      call STR_LOWER (file_name,0,0)
      call STR_UPPER (file_format,0,0)

c     Check unit status:
      INQUIRE (lu,opened=exists,name=open_name,form=open_format)
      if (exists) then
         call STR_LOWER (open_name,0,0)
         call DECODE_FILE_NAME (open_name,directory,root_name,extension)
         open_name=root_name(:STR_LENGTH(root_name))//'.'//extension

c        Check if the file name matches; note we assume that if the file
c        matches, then it must be the right one. We do not check the
c        directory because the logical translation obtained from INQUIRE
c        may not match the value used in lwpcDAT_loc.
         if (open_name(:STR_LENGTH(open_name)) .eq.
     &       file_name(:STR_LENGTH(file_name))) then

c           File is already open and the name is correct
            call STR_UPPER (open_format,0,0)
            if (open_format(1:1) .eq. file_format(1:1)) then

               REWIND (lu)
            else

c              File is already open but the format is incorrect
               if (file_format(1:1) .eq. 'F') then
                  write(error_msg,
     &                '(''[OPEN_DAT]: ''
     &                  ''Unit '',i2,'' is already opened as '',a,1x,
     &                  ''instead of FORMATTED'')')
     &                    lu,open_format
               else
                  write(error_msg,
     &                '(''[OPEN_DAT]: ''
     &                  ''Unit '',i2,'' is already opened as '',a,1x,
     &                  ''instead of UNFORMATTED'')')
     &                    lu,open_format
               end if
               call LWPC_ERROR ('ERROR',error_msg)
            end if
         else

c           File is already open but the name is incorrect
            write(error_msg,
     &          '(''[OPEN_DAT]: ''
     &            ''Unit '',i2,'' is already opened as '',a,1x,
     &            ''instead of '',a)')
     &              lu,open_name(:STR_LENGTH(open_name)),
     &                 file_name(:STR_LENGTH(file_name))
            call LWPC_ERROR ('ERROR',error_msg)
         end if
      else

c        The file has not been opened;
c        see if it is in lwpcDAT_loc

         full_name=lwpcDAT_loc(:STR_LENGTH(lwpcDAT_loc))//
     &             file_name(:STR_LENGTH(file_name))
         call STR_LOWER (full_name,0,0)
         INQUIRE (file=full_name,exist=exists)
         if (.not.exists) then

c           The file is not found in lwpcDAT_loc;
c           see if it is in lwpcXTR_loc
            if (STR_LENGTH(lwpcXTR_loc) .gt. 0) then
               full_name=lwpcXTR_loc(:STR_LENGTH(lwpcXTR_loc))//
     &                   file_name(:STR_LENGTH(file_name))
               call STR_LOWER (full_name,0,0)
               INQUIRE (file=full_name,exist=exists)
            end if
         end if

         if (exists) then

c           The file is found
            if (file_format(1:1) .eq. 'F') then
               OPEN (lu,file=full_name,
     &                  status='old',form='formatted',
     &                  iostat=iocheck,action='read',err=900)
            else
               OPEN (lu,file=full_name,
     &                  status='old',form='unformatted',
     &                  iostat=iocheck,action='read',err=900)
            end if
         else

c           The file is not found
            write(error_msg,
     &          '(''[OPEN_DAT]: ''
     &            ''File not found: '',a)')
     &              full_name(:STR_LENGTH(full_name))
            call LWPC_ERROR ('ERROR',error_msg)
         end if
      end if
      RETURN

c     Error
900   write(error_msg,
     &    '(''[OPEN_DAT]: '',
     &      ''I/O error '',i3,'' occurred trying to open '',
     &      ''file: '',a)')
     &      iocheck,full_name(:STR_LENGTH(full_name))
      call LWPC_ERROR ('Error',error_msg)

      END      ! OPEN_DAT