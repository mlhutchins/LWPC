      SUBROUTINE DECODE_A_NOISE
     &          (data_string,
     &           model,
     &           month,day,year,UT,band_width)

c***********************************************************************
c                   subroutine decode_a_noise
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     29 Jan 1992

c  Function:
c     Reads parameters for calculation of the atmospheric noise
c     from a data string which contains:

c        MODEL MONTH/day/year hour:minute band_width

c     The parameters of the date and time are as follows:

c        MONTH  name of the month
c        day    day of the month
c        year   year
c        hour   hour of the day
c        minute minute of the hour

c     The day may be omitted by using two "/"; the year may be omitted
c     by dropping "/year"; the day and year may be omitted by dropping
c     "/day/year"; the year may be entered modulo (100). The minutes
c     may be omitted by dropping ":minute".

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     model          [c,*] string which identifies the noise model
c     month          [i  ] number of the month
c     day            [i  ] day of the month
c     year           [i  ] year modulo 100
c     UT             [i  ] Universal time in HHmm format
c     band_width     [r  ] noise band width; Hz

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     mod

c     decode_date
c     decode_time
c     month_number

c     str_count_list
c     str_get_item
c     str_lower

c  References:

c  Change history:

c*******************!***************************************************

      character*(*) data_string,model
      character*  3 month_string
      character*  5 time_string
      character* 22 date_string
      character*200 error_msg
      integer       str_length,day,year,UT,hour


      if (STR_LENGTH(data_string) .eq. 0) THEN
        write(error_msg,
     &      '(''[DECODE_A_NOISE]: Data string missing'')')
        call LWPC_ERROR('ERROR', error_msg)
      endif

      call STR_COUNT_LIST (data_string,0,0,nritem)

c     Get noise model identification
      call STR_GET_ITEM (1,data_string,model,n1,n2)
      call STR_LOWER (model,0,0)

      if (nritem .gt. 1) then

c        Get date
         call STR_GET_ITEM (2,data_string,date_string,n1,n2)
         call DECODE_DATE (date_string,month_string,day,year)

         call STR_UPPER (month_string,1,1)
         call STR_LOWER (month_string,2,0)
         call MONTH_NUMBER (month_string,month)

         year=MOD(year,100) ! store last two digits of the year

         if (nritem .gt. 2) then

c           Get time
            call STR_GET_ITEM (3,data_string,time_string,n1,n2)
            call DECODE_TIME (time_string,hour,minute)
            UT=100*hour+minute

            if (nritem .gt. 3) then

c              Get band width
               if (data_string(n2+1:n2+1) .eq. ',') n2=n2+1
               call DECODE_LIST_FLT
     &             (data_string(n2+1:),1,nrlist,band_width)
            end if
         end if
      end if

      RETURN
      END      ! DECODE_A_NOISE