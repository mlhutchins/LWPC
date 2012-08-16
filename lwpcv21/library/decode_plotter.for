      SUBROUTINE DECODE_PLOTTER
     &          (data_string,
     &           device,orientation)

c***********************************************************************
c                   subroutine decode_plotter
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     29 Jan 1992

c  Function:
c     Reads parameters for the plotter
c     from a data string which contains:

c        device orientation

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     device         [c,*] plot device name
c     orientation    [c,*] plot orientation

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     str_count_list
c     str_get_item

c  References:

c  Change history:

c*******************!***************************************************

      character*(*) data_string,device,orientation
      character*200 error_msg
      integer       str_length


      if (STR_LENGTH(data_string) .eq. 0) THEN
         write(error_msg,
     &    '(''[DECODE_PLOTTER]: Data string missing'')')
      endif

      call STR_COUNT_LIST (data_string,0,0,nritem)

      call STR_GET_ITEM (1,data_string,device,n1,n2)

      if (nritem .ge. 2)
     &   call STR_GET_ITEM (2,data_string,orientation,n1,n2)

      RETURN
      END      ! DECODE_PLOTTER