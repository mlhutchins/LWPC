      SUBROUTINE DECODE_TX_DATA
     &          (data_string,verify,
     &           identification,freq,lat,lon,power,
     &           inclination,heading,altitude)

c***********************************************************************
c                   subroutine decode_tx_data
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     29 Jan 1992

c  Function:
c     Reads parameters of a transmitter
c     from a data string which contains data in one of two forms:

c        identification freq lat lon power inclination heading altitude
c        identification tx-specification-file

c     In the second form, TX-SPECIFICATION-FILE is the name of a
c     file which contains records containing the list of parameters for
c     the transmitter shown in the first form; the file is searched for
c     a match with TX-ID and the corresponding parameters are returned.

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.
c     verify         [l  ] =.TRUE. checks input with LWPC_DAT:XMTR.LIS

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     identification [c,*] string which identifies the transmitter
c     freq           [r  ] frequency; kHz
c     lat            [r  ] latitude; degrees North
c     lon            [r  ] longitude; degrees West
c     power          [r  ] power; kW
c     inclination    [r  ] inclination of the antenna; degrees below
c                          horizontal
c     heading        [r  ] heading of the antenna; degrees East of North
c     altitude       [r  ] altitude of the antenna; km

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     get_xmtr
c     str_count_list
c     str_get_item
c     str_lower

c  References:

c  Change history:
c    27 Oct 1992    Added common block LWPC_CFG.cmn to permit
c                   more flexibility in selecting location
c                   of data files.  The variable 'lwpcDAT_loc'
c                   must be set to contain the location of the
c                   file 'XMTR.LIS' prior to calling this routine.

c    12 Jan 1994    Added tests to ensure valid definition of
c                   transmitter.

c*******************!***************************************************

      include      'lwpc_cfg.cmn'

      character*(*) data_string,identification
      character*120 input_file
      character* 80 string
      character*200 error_msg
      logical       verify
      integer       str_length
      real          list(7),
     &              lat,lon,inclination,
     &              latx,lonx,inclinationx


      if (STR_LENGTH(data_string) .eq. 0) then
         write(error_msg,
     &       '(''[DECODE_TX_DATA]: Data string missing'')')
         call LWPC_ERROR ('ERROR', error_msg)
      end if

      call STR_COUNT_LIST (data_string,0,0,nritem)

      call STR_GET_ITEM (1,data_string,identification,n1,n2)

      call STR_LOWER (identification,0,0)

      if (nritem .le. 2) then
         if (nritem .eq. 1) then

c           Only one parameter in the data string;
c           must be identification, use default master file
c           Was STR_LENGTH(lwpcDAT_loc), changed to LEN_TRIM
                 input_file=lwpcDAT_loc(1:LEN_TRIM(lwpcDAT_loc))// !reads the loc from 1:length
     &                'xmtr.lis'

c           Get parameters from the specified file
            call GET_XMTR
     &          (input_file,
     &           identification,freq,lat,lon,power,
     &           inclination,heading,altitude)
         else

c           Only two parameters in the data string;
c           look for a number or a null in the first character
            string='0'
            call STR_GET_ITEM (2,data_string,string,k1,k2)

            if (ICHAR(string(1:1)) .lt. 48 .or.
     &          ICHAR(string(1:1)) .gt. 57) then

c              The first character is not a number;
c              the second parameter must be a file name
               call STR_GET_ITEM (2,data_string,input_file,k1,k2)

c              Get parameters from the specified file
               call GET_XMTR
     &             (input_file,
     &              identification,freq,lat,lon,power,
     &              inclination,heading,altitude)
               nritem=-1
            end if
         end if
      end if

      if (nritem .gt. 1) then

         if (data_string(n2+1:n2+1) .eq. ',') n2=n2+1

         call DECODE_LIST_FLT (data_string(n2+1:),7,nrlist,list)

         if ((identification .ne. 'dummy' .and. nrlist .lt. 3) .or.
     &       (identification .eq. 'dummy' .and. nrlist .lt. 1)) then
            write(error_msg,
     &          '(''[DECODE_TX_DATA]: '',
     &            ''Insufficient number of parameters to define '',
     &            ''transmitter'')')
            call LWPC_ERROR ('ERROR', error_msg)
         end if

c        Read numerical values
         freq=list(1)
         if (nrlist .gt. 1) lat        =list(2)
         if (nrlist .gt. 2) lon        =list(3)
         if (nrlist .gt. 3) power      =list(4)
         if (nrlist .gt. 4) inclination=list(5)
         if (nrlist .gt. 5) heading    =list(6)
         if (nrlist .gt. 6) altitude   =list(7)
      end if

      if (verify) then

         if (nritem .gt. 1 .and. identification .ne. 'dummy') then

c           Check values against the master list
c           NOTE: only the frequency and location must match
            freqx       =freq
            latx        =lat
            lonx        =lon
            powerx      =power
            inclinationx=inclination
            headingx    =heading
            altitudex   =altitude

            input_file=lwpcDAT_loc(1:STR_LENGTH(lwpcDAT_loc))//
     &                'xmtr.lis'
            call GET_XMTR (input_file,
     &                     identification,
     &                     freqx,latx,lonx,powerx,
     &                     inclinationx,headingx,altitudex)

            if (freqx .ne. freq .or.
     &          latx .ne. lat .or. lonx .ne. lon) then
               write(error_msg,
     &             '(''[DECODE_TX_DATA]: '',
     &               ''Input does not match master file'')')
               call LWPC_ERROR('ERROR', error_msg)
            end if
         end if
      end if
      RETURN
      END      ! DECODE_TX_DATA