      SUBROUTINE DECODE_OP_AREA
     &          (data_string,verify,
     &           identification,left_lon,right_lon,bottom_lat,top_lat)

c***********************************************************************
c                   subroutine decode_op_area
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     29 Jan 1992

c  Function:
c     Reads parameters of a geographic op area
c     from a data string which contains data in one of two forms:

c        identification lat1 lon1 lat2 lon2
c        identification op-area-specification-file

c     In the last form, OP-AREA-SPECIFICATION-FILE is the name of a
c     file which contains records containing the list of parameters for
c     the op area shown in the other forms; the file is searched for a
c     match with AREA-ID and the corresponding parameters are returned.

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.
c     verify         [l  ] =.TRUE. checks input with LWPC_DAT:AREA.LIS

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     identification [c,*] string which identifies the op area
c     left_lon       [r  ] longitude of left edge of the op area
c     right_lon      [r  ] longitude of right edge of the op area
c     bottom_lat     [r  ] latitude of the bottom of the op area
c     top_lat        [r  ] latitude of the top of the op area

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     get_opa
c     str_count_list
c     str_get_item
c     str_lower

c  References:

c  Change history:
c    27 Oct 1992    Added common block LWPC_CFG.cmn to permit
c                   more flexibility in selecting location
c                   of data files.  The variable 'lwpcDAT_loc'
c                   must be set to contain the location of the
c                   file 'AREA.LIS' prior to calling this routine.

c    12 Jan 1994    Added tests to ensure valid definition of
c                   op area.

c*******************!***************************************************

      include      'lwpc_cfg.cmn'

      character*(*) data_string,identification
      character*120 input_file
      character*200 error_msg
      logical       verify
      integer       str_length
      real          list(4),
     &              left_lon


      if (STR_LENGTH(data_string) .eq. 0) THEN
         write(error_msg,
     &       '(''[DECODE_OP_AREA]: Data string missing'')')
         call LWPC_ERROR ('ERROR', error_msg)
      end if

      call STR_COUNT_LIST (data_string,0,0,nritem)

c     Get op area identification
      call STR_GET_ITEM (1,data_string,identification,n1,n2)
      call STR_LOWER (identification,0,0)

      if (nritem .le. 2) then
         if (nritem .eq. 1) then

c           Only one parameter;
c           must be IDENTIFICATION, use default master file
            input_file=lwpcDAT_loc(1:STR_LENGTH(lwpcDAT_loc))//
     &                'area.lis'
         else

c           Only two parameters in the data string;
c           must be IDENTIFICATION and a file name
            call STR_GET_ITEM (2,data_string,input_file,n1,n2)
            nritem=-1
         end if

c        Get parameters from the specified file
         call GET_OPA
     &       (input_file,
     &        identification,bottom_lat,left_lon,top_lat,right_lon)
      else

c        Extra parameters in the data string;
c        must be a full set of coordinates
         if (data_string(n2+1:n2+1) .eq. ',') n2=n2+1

         call DECODE_LIST_FLT (data_string(n2+1:),4,nrlist,list)

         if (nrlist .lt. 4) then
            write(error_msg,
     &          '(''[DECODE_OP_AREA]: '',
     &            ''Insufficient number of parameters to define '',
     &            ''op area'')')
            call LWPC_ERROR ('ERROR', error_msg)
         end if

         bottom_lat=list(1)
         left_lon  =list(2)
         top_lat   =list(3)
         right_lon =list(4)
      end if

      if (verify) then

         if (nritem .gt. 1 .and. identification .ne. 'dummy') then

c           Check values against the master list
            value1x=bottom_lat
            value2x=left_lon
            value3x=top_lat
            value4x=right_lon

            input_file=lwpcDAT_loc(1:STR_LENGTH(lwpcDAT_loc))//
     &                'area.lis'
            call GET_OPA (input_file,
     &                    identification,
     &                    value1x,value2x,value3x,value4x)

            if (value1x .ne. bottom_lat .or. value2x .ne. left_lon .or.
     &          value3x .ne. top_lat .or. value4x .ne. right_lon) then

               write(error_msg,
     &             '(''[DECODE_OP_AREA]: '',
     &               ''Input does not match master file'')')
               call LWPC_ERROR ('ERROR', error_msg)
            end if
         end if
      end if
      RETURN
      END      ! DECODE_OP_AREA