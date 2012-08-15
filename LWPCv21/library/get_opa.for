      SUBROUTINE GET_OPA
     &         (file_name,area_id,
     &          op_lat1,op_lon1,op_lat2,op_lon2)

c***********************************************************************
c                         subroutine get_opa
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Gets op area parameters from data file if a match is found
c     between the input identification;
c     otherwise, adds the parameters to the end of the list.

c  Parameters passed:
c     file_name        [s] name of the data file
c     area_id          [s] op area identification

c     op_lat1          [r] lower bound of the op area; degrees N
c     op_lon1          [r] right bound of the op area; degrees W
c     op_lat2          [r] upper bound of the op area; degrees N
c     op_lon2          [r] left  bound of the op area; degrees W

c  Parameters returned:
c     op_lat1          [r] lower bound of the op area; degrees N
c     op_lon1          [r] right bound of the op area; degrees W
c     op_lat2          [r] upper bound of the op area; degrees N
c     op_lon2          [r] left  bound of the op area; degrees W

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     close
c     open
c     read
c     write

c     decode_list_flt
c     str_get_item
c     str_length
c     str_upper

c  References:

c  Change History:
c     23 Oct 90     Modified to check the parameters of the op area and
c                   return the name of the area if one is found which
c                   has the same parameters as specified by the user.

c     12 Jan 94     Added tests to ensure valid definition of
c                   op area.

c     31 Jan 94     Added error checking on open statement.

c     21 Oct 95     Changed to get the logical unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      character*(*) file_name,area_id

      character* 20 areax,test
      character*120 bcd
      character*200 error_msg
      integer       str_length

      dimension     values(4)


      OPEN (lwpcDAT_lun,file=file_name,status='old',
     &                  iostat=iocheck,err=900)

c     The data file records are to contain:

c     AREA_ID,OP_LAT1,OP_LON1,OP_LAT2,OP_LON2

c     where:
c        AREA_ID      area identification
c        OP_LAT1      lower bound of the op area; degrees N
c        OP_LON1      right bound of the op area; degrees W
c        OP_LAT2      upper bound of the op area; degrees N
c        OP_LON2      left  bound of the op area; degrees W


c     Check for a match between the input area-id and those found in the
c     area specification file.

      test=area_id
      call STR_UPPER (test,0,0)
      areax='   '
      do while (areax(1:3) .ne. 'END')
         read (lwpcDAT_lun,'(a)') bcd
         call STR_GET_ITEM (1,bcd,areax,n1,n2)
         call STR_UPPER (areax,0,0)

         if (areax .eq. test) then
            if (bcd(n2+1:n2+1) .eq. ',') n2=n2+1

            call DECODE_LIST_FLT (bcd(n2+1:),4,nrvalues,values)

            op_lat1=values(1)
            op_lon1=values(2)
            op_lat2=values(3)
            op_lon2=values(4)

            CLOSE(lwpcDAT_lun)
            RETURN
         end if
      end do

c     The named area was not found;
c     check for a match between the input area parameters and those
c     found in the area specification file.

      if (op_lat1 .eq. 0. .and. op_lat2 .eq. 0. .and.
     &    op_lon1 .eq. 0. .and. op_lon2 .eq. 0.) then
         write(error_msg,
     &       '(''[GET_OPA]: Op area coordinates are missing'')')
         call LWPC_ERROR ('ERROR',error_msg)
      end if

      REWIND (lwpcDAT_lun)

20    read (lwpcDAT_lun,'(a)',end=30) bcd
      call STR_GET_ITEM (1,bcd,areax,n1,n2)
      call STR_UPPER (areax,0,0)
      if (areax(1:3) .ne. 'END' .and.
     &    areax(1:7) .ne. 'AREA-ID' .and.
     &    areax(1:7) .ne. 'AREA_ID') then

         if (bcd(n2+1:n2+1) .eq. ',') n2=n2+1

         call DECODE_LIST_FLT (bcd(n2+1:),4,nrvalues,values)

         oplat1x=values(1)
         oplon1x=values(2)
         oplat2x=values(3)
         oplon2x=values(4)

         if (oplat1x .eq. op_lat1 .and. oplon1x .eq. op_lon1 .and.
     &       oplat2x .eq. op_lat2 .and. oplon2x .eq. op_lon2) then

            area_id=areax
            CLOSE(lwpcDAT_lun)
            RETURN
         end if
      end if
      go to 20

c     The specified area or its parameters were not found;
c     store the new area.

30    BACKSPACE(lwpcDAT_lun)
      BACKSPACE(lwpcDAT_lun)

      write(lwpcDAT_lun,'(a,1x,f5.0,f6.0,f5.0,f6.0)')
     &      area_id(:STR_LENGTH(area_id)),
     &      op_lat1,op_lon1,op_lat2,op_lon2
      write(lwpcDAT_lun,'(''end'')')
      CLOSE(lwpcDAT_lun)
      RETURN

c     Error
900   write(error_msg,
     &    '(''[GET_OPA]: '',
     &      ''I/O error '',i3,'' occurred trying to open '',
     &      ''file: '',a)') iocheck,file_name
      call LWPC_ERROR ('ERROR',error_msg)

      END      ! GET_OPA