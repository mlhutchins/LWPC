      SUBROUTINE DECODE_RX_DATA
     &          (data_string,
     &           components,number_of_components,altitude)

c***********************************************************************
c                   subroutine decode_rx_data
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     29 Jan 1992

c  Function:
c     Reads parameters for a mode sum relating to the receiver
c     from a data string which contains:

c        components altitude

c     The COMPONENTS string is expected to be one of the following:

c        VERTICAL .or. Z
c           indicates that only the vertical fields (Ez) are to be
c           computed; this string may be shortened so long as it is
c           unique.

c        HORIZONTAL .or. Y
c           indicates that both the vertical and horizontal fields (Ez
c           and Ey) are to be computed; this string may be shortened so
c           long as it is unique.

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     components     [c,*] string which identifies the components
c     number_of_components
c                    [c,*] number of components
c     altitude       [r  ] altitude of the receiver; km

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     str_count_list
c     str_get_item
c     str_lower

c  References:

c  Change history:

c*******************!***************************************************

      character*(*) data_string,components
      character*200 error_msg
      integer       str_length


      if (STR_LENGTH(data_string) .eq. 0) then
         write(error_msg,
     &       '(''[DECODE_RX_DATA]: Data string missing'')')
         call LWPC_ERROR('ERROR', error_msg)
      endif

      call STR_COUNT_LIST (data_string,0,0,nritem)

c     Extract the component identification
      call STR_GET_ITEM (1,data_string,components,n1,n2)
      call STR_LOWER (components,0,0)

c     Determine how many components are to be used
      if (components .eq. 'v' .or. components .eq. 'z') then
         number_of_components=1
      else
     &if (components .eq. 'h' .or. components .eq. 'y') then
         number_of_components=2
      else
         write(error_msg,
     &       '(''[DECODE_RX_DATA]: '',
     &         ''Incorrect specification of components'')')
         call LWPC_ERROR('ERROR', error_msg)
      end if

c     Extract the receiver altitude
      if (nritem .gt. 1) then
         if (data_string(n2+1:n2+1) .eq. ',') n2=n2+1
         call DECODE_LIST_FLT (data_string(n2+1:),1,nrlist,altitude)
      end if

      RETURN
      END      ! DECODE_RX_DATA