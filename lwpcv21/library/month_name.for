      SUBROUTINE MONTH_NAME
     &          (number,name)

c***********************************************************************
c                         subroutine month_name
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Returns name of the month given the number of the month; the
c     first character is upper case and remaining characters are lower
c     case; as many characters are returned as the output string allows.

c  Parameters passed:
c     number           [i] number of the month

c  Parameters returned:
c     name             [s] name of the month

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     str_length
c     str_lower
c     str_upper

c  References:

c  Change History:

c*******************!***************************************************

      character*(*) name

      character*12  month(12)
      character*200 error_msg
      integer       str_length

      data          month/'JANUARY','FEBRUARY','MARCH','APRIL','MAY',
     &                    'JUNE','JULY','AUGUST','SEPTEMBER','OCTOBER',
     &                    'NOVEMBER','DECEMBER'/


      if (number .lt. 1 .or. number .gt. 12) then
         write(error_msg,
     &       '(''[MONTH_NAME]: '',
     &         ''Invalid month number: '',i10)') number
         call LWPC_ERROR ('WARNING',error_msg)
         name=' '
         RETURN
      end if
      name=month(number)
      call STR_UPPER (name,1,1)
      if (STR_LENGTH(name) .gt. 1) call STR_LOWER (name,2,0)

      RETURN
      END      ! MONTH_NAME