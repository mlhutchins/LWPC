      SUBROUTINE DECODE_FILE_NAME
     &          (data_string,
     &           directory,root_name,extension)

c***********************************************************************
c                   subroutine decode_file_name
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     03 Feb 1992

c  Function:
c     Reads parameters for file names
c     from a data string which contains:

c        drive:\directory\root_name.extension
c     or
c        drive:[directory]root_name.extension

c  Parameters passed:
c     parameter-name[t,n] description {t is type, n is dimension}

c     data_string   [c,*] string containing the data to be decoded.

c  Parameters returned:
c     parameter-name[t,n] description {t is type, n is dimension}

c     directory     [c,*] directory parameters including drive
c     root_name     [c,*] root of the file name
c     extension     [c,*] extension of the file name

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     index
c     str_length

c  References:

c  Change history:
c     24 Nov 1993   Corrected errors in subscripts.

c     16 Dec 1996   Added code to allow generalized delimiters.

c*******************!***************************************************

      character*(*) data_string,directory,root_name,extension

      character*  1 delimiter(3)
      character*200 error_msg
      logical       first
      integer       str_length

      data          first/.true./


      kl=STR_LENGTH(data_string)

      if (kl .eq. 0) then
        write(error_msg,
     &      '(''[DECODE_FILE_NAME]: Data string missing'')')
        call LWPC_ERROR('ERROR', error_msg)
      end if

c     If this is the first pass; get the delimiters
      if (first) call GET_DELIMITER (delimiter)

c     Look for directory specification
      kx=-1
      k1=kl
      do while (kx .lt. 0)
         if (data_string(k1:k1) .eq. delimiter(3)) then
            kx=k1
         else
            k1=k1-1
            if (k1 .eq. 0) kx=0
         end if
      end do

c     Look for extension
      kx=-1
      k2=kl
      do while (kx .lt. 0)
         if (k2 .eq. k1) then
            kx=0
            k2=kl
         else
            if (data_string(k2:k2) .eq. '.') then
               kx=k2
            else
               k2=k2-1
            end if
         end if
      end do

      if (k1 .eq. 0) then

c        There is no directory
         directory=' '
      else

c        There is a directory name
         directory=data_string(1:k1)
      end if
      if (k2 .eq. k1) then

c        There is no file name
         extension=' '
         root_name=' '
      else
     &if (k2 .eq. kl) then

c        There is no extension
         extension=' '
         root_name=data_string(k1+1:kl)
      else

c        There is a file name with extension
         root_name=data_string(k1+1:k2-1)
         extension=data_string(k2+1:kl  )
      end if

      RETURN
      END      ! DECODE_FILE_NAME