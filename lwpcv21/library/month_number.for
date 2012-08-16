      SUBROUTINE MONTH_NUMBER
     &          (name,number)

c***********************************************************************
c                         subroutine month_number
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Returns number of the named month.

c  Parameters passed:
c     name             [s] name of the month

c  Parameters returned:
c     number           [i] number of the month

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     str_length
c     str_upper

c  References:

c  Change History:

c*******************!***************************************************

      character*(*) name

      character*  3 month_key
      character* 12 month(12)
      character*200 error_msg
      integer       str_length

      data          month/'JANUARY','FEBRUARY','MARCH','APRIL','MAY',
     &                    'JUNE','JULY','AUGUST','SEPTEMBER','OCTOBER',
     &                    'NOVEMBER','DECEMBER'/


      month_key=name
      call STR_UPPER (month_key,1,0)
      number=12
      ln=STR_LENGTH (month_key)
      do while (month_key(:ln) .ne. month(number)(:ln))
         number=number-1
         if (number .eq. 0) then
            write(error_msg,
     &          '(''[MONTH_NUMBER]: '',
     &            ''Invalid month name: '',a)') name
            call LWPC_ERROR ('WARNING',error_msg)
            RETURN
         end if
      end do

      RETURN
      END      ! MONTH_NUMBER