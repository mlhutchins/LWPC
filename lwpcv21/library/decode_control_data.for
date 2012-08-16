      SUBROUTINE DECODE_CONTROL_DATA
     &          (input_string,cntrl_string,data_string)

c***********************************************************************
c                         subroutine decode_control_data
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Returns a control string and its associated data string.

c  Parameters passed:
c     input_string     [s] input  string: CONTROL DATA

c  Parameters returned:
c     cntrl_string     [s] output string: CONTROL
c     data_string      [s] output string: DATA

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     len

c  References:

c  Change History:

c*******************!***************************************************

      character     input_string*(*),cntrl_string*(*),data_string*(*)


c     Find the length of the input string
      jl=LEN(input_string)
      do while (input_string(jl:jl) .eq. ' ')
        jl=jl-1
      end do

c     Find the end of the control string by starting at the beginning
c     of the string and looking for the first blank character
      jf=1
      do while (input_string(jf:jf) .ne. ' ')
         if (jf .eq. jl) then ! there is only the control string
            cntrl_string=input_string
            nn=LEN(data_string)
            do while (nn .gt. 0)
               data_string(nn:nn)=' '
               nn=nn-1
            end do
            RETURN
         end if
         jf=jf+1
      end do
      cntrl_string=input_string(1:jf)

c     Now, find the beginning of the data by looking for the first non-
c     blank location
      do while (input_string(jf:jf) .eq. ' ')
         if (jf .eq. jl) then ! there is no data
            nn=LEN(data_string)
            do while (nn .gt. 0)
               data_string(nn:nn)=' '
               nn=nn-1
            end do
            RETURN
         end if
         jf=jf+1
      end do
      data_string=input_string(jf:jl)

      RETURN
      END      ! DECODE_CONTROL_DATA