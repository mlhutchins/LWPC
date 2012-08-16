      SUBROUTINE SET_FILE_ID
     &          (logical_unit,prgm_id,file_id)

c***********************************************************************
c                         subroutine set_file_id
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 Apr 1990

c  Function:
c     Sets up the unique file identification string using the program
c     identification, the full file name, the date and a randomly
c     generated string of three alphabetic characters; the file
c     identification is of the form:

c        ddMMMyy ABC [...]name.ext

c     where ddMMMyy is the run date in the form: dayMONTHyear
c           ABC     is the randomly generated string
c           prgm    is the program name
c           [...]   is the directory (without the UIC)
c           name    is the file name
c           ext     is the extension

c  Parameters passed:
c     logical_unit     [i] logical unit where the file is openned
c     prgm_id          [s] identification of program

c  Parameters returned:
c     file_id          [s] file identification string

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     char
c     get_date
c     get_time
c     get_random

c     inquire
c     int
c     mod

c     str_length
c     str_lower

c  References:

c  Change History:

c     29 Jan 91     The following changes were made to use this
c                   subroutine on the PC using MS FORTRAN 5.0:
c               (1) The random number generation procedures were changed
c               (2) The calls to date and time routines were changed
c               (3) The determination of the filename was changed to
c                   reflect the filenaming convention of PC's, rather
c                   than VAX's
c                   The above changes produce exactly the same output as
c                   the VAX version of this subroutine


c     06 Dec 96     Modified to use new generic date, time and random

c                   number generator routines.


c*******************!***************************************************

      character*(*) prgm_id,file_id

      character*  1 char1,char2,char3
      character*  3 mon(12)
      character*  9 run_date
      character*256 file_name
      logical       first
      integer       logical_unit
      integer       str_length

      data          mon/'Jan','Feb','Mar','Apr','May','Jun',
     &                  'Jul','Aug','Sep','Oct','Nov','Dec'/
      data          first/.true./

      if (first) then

c        Set up random number for the unique three character string
c        First, set up the seed for the random number generator
         call GET_TIME (ihr,imin,isec)

         SEED=isec
         call GET_RANDOM (SEED,ranval)

c        Set up the unique three character string choosen from 1-9, A-Z.
c        This string is constant for any given run.
         call GET_RANDOM (SEED,ranval)
         n1=INT(ranval*35.)
         if (n1 .le. 25) then
            char1=CHAR(n1+65)
         else
            char1=CHAR(n1+23)
         end if

         call GET_RANDOM (SEED,ranval)
         n2=INT(ranval*35.)
         if (n2 .le. 25) then
            char2=CHAR(n2+65)
         else
            char2=CHAR(n2+23)
         end if

         call GET_RANDOM (SEED,ranval)
         n3=INT(ranval*35.)
         if (n3 .le. 25) then
            char3=CHAR(n3+65)
         else
            char3=CHAR(n3+23)
         end if

c        Get the date of the current run.
         call GET_DATE (iy,im,id)
         iy=MOD(iy,100)
         WRITE(run_date,'(i2,a3,i2)') id,mon(im),iy

         if (run_date(1:1) .eq. ' ') run_date(1:1)='0'

         first=.false.
      end if

c     Get the full file name including the directory.
      INQUIRE (unit=logical_unit,name=file_name)

c     Convert to lower case
      call STR_LOWER (file_name,1,STR_LENGTH(file_name))

      l1=1
      l2=STR_LENGTH (file_name)

c     Set up the file identification:
c     The form is "ddMMMyy ABC prgm name"

c     where ddMMMyy is the run date in the form:  dayMONTHyear
c           ABC     is the randomly generated string
c           prgm    is the program name
c           name    is the file name

      file_id=run_date(1:2)//run_date(3:5)//run_date(6:7)//' '//
     &        char1//char2//char3//' '//
     &        prgm_id(:STR_LENGTH(prgm_id))//' '//
     &        file_name(l1:l2)

      RETURN
      END      ! SET_FILE_ID