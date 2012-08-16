      SUBROUTINE GET_XMTR
     &          (file_name,xmtr_id,
     &           freq,tlat,tlon,power,incl,headng,talt)

c***********************************************************************
c                         subroutine get_xmtr
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Gets transmitter parameters from data file if a match is found
c     between the input identification;
c     otherwise, adds the parameters to the end of the list.

c  Parameters passed:
c     file_name        [s] name of the data file
c     xmtr_id          [s] transmitter identification

c     freq             [r] frequency;           kHz
c     tlat             [r] latitude;            degrees N
c     tlon             [r] longitude;           degrees N
c     power            [r] power;               kW
c     incl             [r] antenna inclination; degrees from vertical
c     headng           [r] antenna heading;     degrees East of North
c     talt             [r] antenna altitude;    km

c  Parameters returned:
c     freq             [r] frequency;           kHz
c     tlat             [r] latitude;            degrees N
c     tlon             [r] longitude;           degrees N
c     power            [r] power;               kW
c     incl             [r] antenna inclination; degrees from vertical
c     headng           [r] antenna heading;     degrees East of North
c     talt             [r] antenna altitude;    km

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     close
c     open
c     print
c     read
c     write

c     decode_list_flt
c     str_get_item
c     str_length
c     str_upper

c  References:

c  Change History:
c     23 Oct 90     Modified to check the parameters of the transmitter
c                   and return the name of the transmitter if one is
c                   found which has the same parameters as specified by
c                   the user.

c     12 Jan 94     Added tests to ensure valid definition of
c                   transmitter.

c     31 Jan 94     Added error checking on open statement.

c*******************!***************************************************

      character*(*) file_name,xmtr_id

      character* 20 xmtrx,test
      character*120 bcd
      character*200 error_msg
      integer       str_length
      real          inclx,incl

      dimension     values(7)


      OPEN (2,file=file_name,
     &        status='old',iostat=iocheck,err=900)

c     The data file records are to contain:

c     XMTRID FREQUENCY LAT LON POWER INCLINATION HEADING ALTITUDE


c     Check for a match between the input xmtr-id and those found in the
c     transmitter specification file.

      test=xmtr_id
      call STR_UPPER (test,0,0)
      xmtrx='   '
      do while (xmtrx(1:3) .ne. 'END')
         read (2,'(a)') bcd
         call STR_GET_ITEM (1,bcd,xmtrx,n1,n2)
         call STR_UPPER (xmtrx,0,0)

         if (xmtrx .eq. test) then

            if (bcd(n2+1:n2+1) .eq. ',') n2=n2+1

            call DECODE_LIST_FLT (bcd(n2+1:),7,nrvalues,values)

            freq  =values(1)
            tlat  =values(2)
            tlon  =values(3)
            power =values(4)
            incl  =values(5)
            headng=values(6)
            talt  =values(7)

            CLOSE(2)
            RETURN
         end if
      end do

c     The named transmitter was not found;
c     check for a match between the input transmitter parameters and
c     those found in the transmitter specification file;
c     NOTE:  power, inclination, heading and altitude are not checked.

      if (freq .eq. 0. .and.
     &    tlat .eq. 0. .and. tlon .eq. 0.) then
         write(error_msg,
     &       '(''[GET_XMTR]: Transmitter coordinates are missing'')')
         call LWPC_ERROR ('ERROR', error_msg)
      end if

      REWIND (2)

20    read (2,'(a)',end=30) bcd
      call STR_GET_ITEM (1,bcd,xmtrx,n1,n2)
      call STR_UPPER (xmtrx,0,0)
      if (xmtrx(1:3) .ne. 'END' .and.
     &    xmtrx(1:7) .ne. 'XMTR-ID' .and.
     &    xmtrx(1:7) .ne. 'XMTR_ID') then

         if (bcd(n2+1:n2+1) .eq. ',') n2=n2+1

         call DECODE_LIST_FLT (bcd(n2+1:),7,nrvalues,values)

         freqx  =values(1)
         tlatx  =values(2)
         tlonx  =values(3)
         powerx =values(4)
         inclx  =values(5)
         headngx=values(6)
         taltx  =values(7)

         if (freqx  .eq. freq  .and.
     &       tlatx  .eq. tlat  .and. tlonx  .eq. tlon) then

            xmtr_id=xmtrx
            CLOSE(2)
            RETURN
         end if
      end if
      go to 20

c     The specified transmitter or its parameters were not found;
c     store the new transmitter.

30    BACKSPACE(2)
      BACKSPACE(2)

      write(2,'(a,f8.3,f8.3,f9.3,f7.1,f5.1,f8.3,f5.1)')
     &      xmtr_id(:STR_LENGTH(xmtr_id)),freq,tlat,tlon,
     &      power,incl,headng,talt
      write(2,'(''end'')')
      CLOSE(2)
      RETURN

c     Error
900   write(error_msg,
     &    '(''[GET_XMTR]: '',
     &      ''I/O error '',i3,'' occurred trying to open '',
     &      ''file: '',a)') iocheck,file_name
      call LWPC_ERROR ('Error',error_msg)

      END      ! GET_XMTR