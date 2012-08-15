      SUBROUTINE DECODE_LIST_INT
     &          (data_string,
     &           mxlist,nrlist,list)

c***********************************************************************
c                   subroutine decode_list_int
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     25 Feb 1992

c  Function:
c     Reads a list of integer numbers from a data string

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.
c     mxlist         [i  ] maximum number of values which can be output

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     nrlist         [i  ] number of values which are output
c     list           [i  ] array of integer values to be output

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     str_count_list
c     str_get_item

c  References:

c  Change history:

c*******************!***************************************************

      character*(*) data_string
      character* 20 item,frmt,null/'                    '/
      character*200 error_msg
      integer       str_length,list(mxlist)


      if (STR_LENGTH(data_string) .eq. 0) then
         write(error_msg,
     &       '(''[DECODE_LIST_INT]: Data string missing'')')
         call LWPC_ERROR('ERROR', error_msg)
      end if

      call STR_COUNT_LIST (data_string,0,0,nrlist)

      if (nrlist .gt. mxlist) then
         nrlist=mxlist
      end if

      do n=1,nrlist
         item=null
         call STR_GET_ITEM (n,data_string,item,n1,n2)
         nl=n2-n1+1
         if (nl .lt. 10) then
            write(frmt,'(''(i'',i1,'')'')') nl
         else
            write(frmt,'(''(i'',i2,'')'')') nl
         end if
         read (item(:nl),frmt) list(n)
      end do

      RETURN
      END      ! DECODE_LIST_INT