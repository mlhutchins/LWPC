      SUBROUTINE DECODE_LIST_FLT
     &          (data_string,
     &           mxlist,nrlist,list)

c***********************************************************************
c                   subroutine decode_list_flt
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     25 Feb 1992

c  Function:
c     Reads a list of floating point numbers from a data string

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.
c     mxlist         [i  ] maximum number of values which can be output

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     nrlist         [i  ] number of values which are output
c     list           [r  ] array of floating point values to be output

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
      integer       str_length
      real          list(mxlist)


      if (STR_LENGTH(data_string) .eq. 0) then
         write(error_msg,
     &       '(''[DECODE_LIST_FLT]: Data string missing'')')
         call LWPC_ERROR('ERROR', error_msg)
      end if

      call STR_COUNT_LIST (data_string,0,0,nrlist)

      m=0
      n=0
      do while (n .lt. nrlist)
         m=m+1
         n=n+1
         item=null
         call STR_GET_ITEM (m,data_string,item,n1,n2)
         
         if (STR_LENGTH(item) .gt. 0) then

c           The item is specified
            call STR_LOWER (item,0,0)
            if (item(STR_LENGTH(item):STR_LENGTH(item)) .eq. '-' .or.
     &          item(STR_LENGTH(item):STR_LENGTH(item)) .eq. '+' .or.
     &          item(STR_LENGTH(item):STR_LENGTH(item)) .eq. 'e' .or.
     &          item(STR_LENGTH(item):STR_LENGTH(item)) .eq. 'd') then

c              The last character suggests that an exponent is not
c              complete so we append the next item to the current one.
               m=m+1
               call STR_GET_ITEM
     &             (m,data_string,item(STR_LENGTH(item)+1:),n3,n4)

c              Reduce the count of the number of items in the list
               nrlist=nrlist-1
            end if

            if (nrlist .gt. mxlist) then
               nrlist=mxlist
            end if

            nl=STR_LENGTH(item)
            if (nl .lt. 10) then
               write(frmt,'(''(f'',i1,''.0)'')') nl
            else
               write(frmt,'(''(f'',i2,''.0)'')') nl
            end if
            read (item(:nl),frmt) list(n)
         end if
      end do

      RETURN
      END      ! DECODE_LIST_FLT