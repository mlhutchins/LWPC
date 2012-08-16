      SUBROUTINE DECODE_IONOSPHERE
     &          (data_string,
     &           model,month,day,year,UT,
     &           profile_flag,beta,hprime,file_name)

c***********************************************************************
c                   subroutine decode_ionosphere
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     29 Jan 1992

c  Function:
c     Reads parameters for specification of the ionospheric profile
c     from a data string which contains data in one of the following
c     forms:

c        homogeneous table file_name
c        homogeneous exponential beta hprime
c        lwpm day
c        lwpm night
c        lwpm month/day/year hour:minute
c        range table file_name
c        range exponential file_name
c        grid table file_name
c        grid exponential file_name
c        chi table file_name
c        chi exponential file_name

c     These are used as follows:

c        HOMOGENEOUS TABLE FILE_NAME
c           indicates that a homogeneous ionosphere is to be used over
c           the whole path; the profile parameters are to be read from
c           a table found in the specified file.

c        HOMOGENEOUS EXPONENTIAL beta hprime
c           indicates that a homogeneous ionosphere is to be used over
c           the whole path; the profile parameters are exponential
c           with a slope BETA and a reference height HPRIME.

c        LWPM DAY
c           indicates that all day-time conditions are to be used over
c           the whole path; the parameters of the ionosphere are to be
c           set by the program.

c        LWPM NIGHT
c           indicates that all night-time conditions are to be used over
c           the whole path; the parameters of the ionosphere are to be
c           set by the program.

c        LWPM MONTH/day/year hour:minute
c           indicates that a specific date and time is to be used; the
c           parameters of the ionospheric profiles are to be set by the
c           program; the parameters of the date and time are as follows:

c              MONTH  name of the month
c              day    day of the month
c              year   year
c              hour   hour of the day
c              minute minute of the hour

c           The day may be omitted by using two "/"; the year may be
c           omitted by dropping "/year"; the day and year may be
c           omitted by dropping "/day/year"; the year may be entered
c           modulo (100). The minutes may be omitted by dropping
c           ":minute".

c        RANGE TABLE FILE_NAME
c           indicates that the ionosphere varies along the path;
c           the profile parameters are to be read from several files:

c              file_name.NDX and file_nameNNN.PRF

c           where NDX lists range and profile index (NNN);
c           each PRF file contains a table associated with index NNN.

c        RANGE EXPONENTIAL beta hprime
c           indicates that the ionosphere varies along the path;
c           the profile parameters are to be read from a file:

c              file_name.NDX

c           where NDX lists range, beta and hprime.

c        GRID TABLE FILE_NAME
c           indicates that the ionosphere varies over a geographic area;
c           the profile parameters are to be read from several files:

c              file_name.NDX and file_nameNNN.PRF

c           where NDX lists position and profile index (NNN);
c           each PRF file contains a table associated with index NNN.

c        GRID EXPONENTIAL beta hprime
c           indicates that the ionosphere varies over a geographic area;
c           the profile parameters are to be read from a file:

c              file_name.NDX

c           where NDX lists position, beta and hprime.

c        CHI TABLE FILE_NAME
c           indicates that the ionosphere varies with solar zenith angle;
c           the profile parameters are to be read from several files:

c              file_name.NDX and file_nameNNN.PRF

c           where NDX lists solar zenith angle and profile index (NNN);
c           each PRF file contains a table associated with index NNN.

c        CHI EXPONENTIAL beta hprime
c           indicates that the ionosphere varies with solar zenith angle;
c           the profile parameters are to be read from a file:

c              file_name.NDX

c           where NDX lists solar zenith angle, beta and hprime.

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     model          [c,*] string which identifies the ionospheric model
c     month          [i  ] number of the month
c     day            [i  ] day of the month
c     year           [i  ] year modulo 100
c     UT             [i  ] Universal time in HHmm format
c     profile_flag   [i  ] profile index
c     beta           [r  ] slope of exponential ionosphere; km(-1)
c     hprime         [r  ] reference height of exponential ionosphere;
c                          km
c     file_name      [c,*] string which identifies the name of the file
c                          containing a tabulated ionospheric profile

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     mod

c     decode_date
c     decode_time
c     month_number

c     str_count_list
c     str_get_item
c     str_lower
c     str_upper

c  References:

c  Change history:

c     Nov 09, 1993  Added solar zenith angle dependent profiles.

c*******************!***************************************************

      character*(*) data_string,model,file_name
      character*  3 month_string
      character*  5 time_string
      character* 22 date_string
      character* 80 string
      character*200 error_msg
      integer       str_length,day,year,UT,hour,profile_flag
      real          list(2)

c     Profile_flag  Profile specification
c     0             homogeneous table file_name
c     1             homogeneous exponential beta hprime
c     2             lwpm day
c     3             lwpm night
c     4             lwpm month/day/year hour:minute
c     5             range table file_name
c     6             range exponential file_name
c     7             grid table file_name
c     8             grid exponential file_name
c     9             chi table file_name
c     10            chi exponential file_name


      if (STR_LENGTH(data_string) .eq. 0) THEN
        write(error_msg,
     &      '(''[DECODE_IONOSPHERE]: Data string missing'')')
        call LWPC_ERROR('ERROR', error_msg)
      endif

      call STR_GET_ITEM (1,data_string,model,n1,n2)
      call STR_UPPER (model,0,0)

      if (model(1:1) .eq. 'H') then

c        Homogeneous ionosphere
         call STR_GET_ITEM (2,data_string,string,n1,n2)
         call STR_UPPER (string,0,0)

         if (string(1:1) .eq. 'T') then

c           Tabular profile
            profile_flag=0
            call STR_GET_ITEM (3,data_string,file_name,n1,n2)
         else
     &   if (string(1:1) .eq. 'E') then

c           Exponential profile
            profile_flag=1
            if (data_string(n2+1:n2+1) .eq. ',') n2=n2+1

            call DECODE_LIST_FLT (data_string(n2+1:),2,nrlist,list)
            beta  =list(1)
            hprime=list(2)
         else

            write(error_msg,
     &          '(''[DECODE_IONOSPHERE]: '',
     &            ''Homogeneous model type not recognized'')')
            call LWPC_ERROR('ERROR', error_msg)
         end if
      else
     &if (model(1:1) .eq. 'L') then

c        LWPM model
         call STR_GET_ITEM (2,data_string,date_string,n1,n2)
         call STR_UPPER (date_string,0,0)

         if (date_string(1:2) .eq. 'DA') then

c           All day
            profile_flag=2
         else
     &   if (date_string(1:2) .eq. 'NI') then

c           All night
            profile_flag=3
         else

c           Specific date and time
            profile_flag=4

            call STR_COUNT_LIST (data_string,0,0,nritem)
            call DECODE_DATE (date_string,month_string,day,year)

            call STR_UPPER (month_string,1,1)
            call STR_LOWER (month_string,2,0)
            call MONTH_NUMBER (month_string,month)

            year=MOD(year,100) ! store last two digits of the year

            if (nritem .gt. 2) then
               call STR_GET_ITEM (3,data_string,time_string,n1,n2)
               call DECODE_TIME (time_string,hour,minute)
               UT=100*hour+minute
            end if
         end if
      else
     &if (model(1:1) .eq. 'R') then

c        Ionosphere varies with range
         call STR_GET_ITEM (2,data_string,string,n1,n2)
         call STR_UPPER (string,0,0)

         if (string(1:1) .eq. 'T') then

c           Tabular profile
            profile_flag=5
            call STR_GET_ITEM (3,data_string,file_name,n1,n2)
         else
     &   if (string(1:1) .eq. 'E') then

c           Exponential profile
            profile_flag=6
            call STR_GET_ITEM (3,data_string,file_name,n1,n2)
         else

            write(error_msg,
     &          '(''[DECODE_IONOSPHERE]: '',
     &            ''Range model type not recognized'')')
            call LWPC_ERROR('ERROR', error_msg)
         end if
      else
     &if (model(1:1) .eq. 'G') then

c        Ionosphere varies with geographic location
         call STR_GET_ITEM (2,data_string,string,n1,n2)
         call STR_UPPER (string,0,0)

         if (string(1:1) .eq. 'T') then

c           Tabular profile
            profile_flag=7
            call STR_GET_ITEM (3,data_string,file_name,n1,n2)
         else
     &   if (string(1:1) .eq. 'E') then

c           Exponential profile
            profile_flag=8
            call STR_GET_ITEM (3,data_string,file_name,n1,n2)
         else

            write(error_msg,
     &          '(''[DECODE_IONOSPHERE]: '',
     &            ''Grid model type not recognized'')')
            call LWPC_ERROR('ERROR', error_msg)
         end if
      else
     &if (model(1:1) .eq. 'C') then

c        Ionosphere varies with solar zenith angle
         call STR_GET_ITEM (2,data_string,string,n1,n2)
         call STR_UPPER (string,0,0)

         if (string(1:1) .eq. 'T') then

c           Tabular profile
            profile_flag=9
            call STR_GET_ITEM (3,data_string,file_name,n1,n2)
         else
     &   if (string(1:1) .eq. 'E') then

c           Exponential profile
            profile_flag=10
            call STR_GET_ITEM (3,data_string,file_name,n1,n2)
         else

            write(error_msg,
     &          '(''[DECODE_IONOSPHERE]: '',
     &            ''Solar zenith angle model type not recognized'')')
            call LWPC_ERROR('ERROR', error_msg)
         end if
      else

         write(error_msg,
     &       '(''[DECODE_IONOSPHERE]: '',
     &         ''Ionopspheric model name not recognized'')')
         call LWPC_ERROR('ERROR', error_msg)
      end if

      RETURN
      END      ! DECODE_IONOSPHERE