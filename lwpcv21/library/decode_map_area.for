      SUBROUTINE DECODE_MAP_AREA
     &          (data_string,verify,
     &           identification,projection,
     &           left_lon,right_lon,size_lon,
     &           bottom_lat,top_lat,size_lat)

c***********************************************************************
c                   subroutine decode_map_area
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     29 Jan 1992

c  Function:
c     Reads parameters of a geographic map
c     from a data string which contains data in one of three forms:

c        identification rectangular   lat1  lon1  lat2  lon2  sizex sizey
c        identification mercator      lat1  lon1  lat2  lon2  sizex sizey
c        identification gnomonic      lat0  lon0  range sizex sizey
c        identification azimuthal     lat0  lon0  range sizex sizey
c        identification orthographic  lat0  lon0  range sizex sizey
c        identification stereographic lat0  lon0  range sizex sizey

c     In the last form, MAP-AREA-SPECIFICATION-FILE is the name of a
c     file which contains records containing the list of parameters for
c     the map shown in the other forms; the file is searched for
c     a match with MAP-ID and the corresponding parameters are returned.

c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}

c     data_string    [c,*] string containing the data to be decoded.
c     verify         [l  ] =.TRUE. checks input with LWPC_DAT:MAP.LIS

c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}

c     If RECTANGULAR or MERCATOR:

c     identification [c,*] string which identifies the map
c     projection     [c,*] name of the map projection
c     left_lon       [r  ] longitude of left edge of the mapped area
c     right_lon      [r  ] longitude of right edge of the mapped area
c     size_lon       [r  ] horizontal dimension of the plotted map
c     bottom_lat     [r  ] latitude of the bottom of the mapped area
c     top_lat        [r  ] latitude of the top of the mapped area
c     size_lat       [r  ] vertical dimension of the plotted map

c     If AZIMUTHAL EQUIDISTANT, GNOMONIC, ORTHOGRAPHIC or STEREOGRAPHIC:

c     identification [c,*] string which identifies the map
c     projection     [c,*] name of the map projection
c     left_lon       [r  ] longitude of the center of the mapped area
c     right_lon      [r  ] not used
c     size_lon       [r  ] horizontal dimension of the plotted map
c     bottom_lat     [r  ] latitude of the center of the mapped area
c     top_lat        [r  ] maximum range from the map center
c     size_lat       [r  ] vertical dimension of the plotted map

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     get_map
c     str_count_list
c     str_get_item
c     str_lower

c  References:

c  Change history:
c    27 Oct 1992    Added common block LWPC_CFG.CMN to permit
c                   more flexibility in selecting location
c                   of data files.  The variable 'lwpcDAT_loc'
c                   must be set to contain the location of the
c                   file 'MAP.LIS' prior to calling this routine.

c    12 Jan 1994    Added tests to ensure valid definition of
c                   map area.

c    06 Jul 1994    Added mercator and orthographic projections.

c    06 Mar 1995    Added SIZEX to gnomonic projection for compatibility
c                   with new geophysics routines.

c    16 Apr 1995    Modified for use with the new geophysics routines.
c                   The new routines allow the azim, gnom and ortho maps
c                   to have rectangular displays; the gnom definition is
c                   now based on a center coordinate and range like the
c                   azim and ortho maps.

c     18 Dec 1997   Added stereographic projection.
c 2/10/10 MLH	Changed OTHERWISE statements to CASE DEFAULT

c*******************!***************************************************

      include      'lwpc_cfg.cmn'

      character*(*) data_string,identification,projection
      character*  4 prjctn
      character*120 input_file
      character*200 error_msg
      logical       verify
      integer       str_length
      real          list(6),
     &              left_lon


      if (STR_LENGTH(data_string) .eq. 0) then
         write(error_msg,
     &       '(''[DECODE_MAP_AREA]: Data string missing'')')
         call LWPC_ERROR ('ERROR', error_msg)
      end if

      call STR_COUNT_LIST (data_string,0,0,nritem)

c     Get map identification
      call STR_GET_ITEM (1,data_string,identification,n1,n2)
      call STR_LOWER (identification,0,0)

      if (nritem .le. 2) then
         if (nritem .eq. 1) then

c           Only one parameter;
c           must be IDENTIFICATION, use default master file
            input_file=lwpcDAT_loc(1:STR_LENGTH(lwpcDAT_loc))//'map.lis'
         else

c           Only two parameters in the data string;
c           must be IDENTIFICATION and a file name
            call STR_GET_ITEM (2,data_string,input_file,n1,n2)
            nritem=-1
         end if

c        Get parameters from the specified file
         call GET_MAP
     &       (input_file,
     &        identification,projection,
     &        value1,value2,value3,value4,value5,value6)

         prjctn=projection
         call STR_UPPER (prjctn,0,0)

         SELECT CASE( prjctn )

         CASE( 'A','G','O','S' )

c           Azimuthal, Gnomonic, Orthographic or Stereographic
            bottom_lat=value1
            left  _lon=value2
            top   _lat=value3
            size  _lon=value4
            size  _lat=value5

         CASE( 'M','R' )

c           Mercator or Rectangular
            bottom_lat=value1
            left  _lon=value2
            top   _lat=value3
            right _lon=value4
            size  _lon=value5
            size  _lat=value6

         CASE DEFAULT

c           Unidentified projection
            write(error_msg,
     &          '(''[DECODE_MAP_AREA]: '',
     &            ''Projection: '',a'', is not available'')')
     &              projection
            call LWPC_ERROR ('ERROR', error_msg)

         END SELECT

      else

c        Extra parameters in the data string;
c        must be projection and a full set of coordinates and sizes
         call STR_GET_ITEM (2,data_string,projection,n1,n2)
         if (data_string(n2+1:n2+1) .eq. ',') n2=n2+1

         prjctn=projection
         call STR_UPPER (prjctn,0,0)

         SELECT CASE( prjctn )

         CASE( 'A' )

c           Azimuthal
            call DECODE_LIST_FLT (data_string(n2+1:),6,nrlist,list)

            if (nrlist .lt. 5) then
               write(error_msg,
     &             '(''[DECODE_MAP_AREA]: '',
     &               ''Insufficient number of parameters to define '',
     &               ''azimuthal equidistant map area'')')
               call LWPC_ERROR ('ERROR', error_msg)
            end if

            bottom_lat=list(1)
            left  _lon=list(2)
            top   _lat=list(3)
            size  _lon=list(4)
            size  _lat=list(5)

         CASE( 'G' )

c           Gnomonic
            call DECODE_LIST_FLT (data_string(n2+1:),6,nrlist,list)

            if (nrlist .lt. 5) then
               write(error_msg,
     &             '(''[DECODE_MAP_AREA]: '',
     &               ''Insufficient number of parameters to define '',
     &               ''gnomonic map area'')')
               call LWPC_ERROR ('ERROR', error_msg)
            end if

            bottom_lat=list(1)
            left  _lon=list(2)
            top   _lat=list(3)
            size  _lon=list(4)
            size  _lat=list(5)

         CASE( 'O' )

c           Orthographic
            call DECODE_LIST_FLT (data_string(n2+1:),6,nrlist,list)

            if (nrlist .lt. 5) then
               write(error_msg,
     &             '(''[DECODE_MAP_AREA]: '',
     &               ''Insufficient number of parameters to define '',
     &               ''orthographic map area'')')
               call LWPC_ERROR ('ERROR', error_msg)
            end if

            bottom_lat=list(1)
            left  _lon=list(2)
            top   _lat=list(3)
            size  _lon=list(4)
            size  _lat=list(5)

         CASE( 'S' )

c           Stereographic
            call DECODE_LIST_FLT (data_string(n2+1:),6,nrlist,list)

            if (nrlist .lt. 5) then
               write(error_msg,
     &             '(''[DECODE_MAP_AREA]: '',
     &               ''Insufficient number of parameters to define '',
     &               ''stereographic map area'')')
               call LWPC_ERROR ('ERROR', error_msg)
            end if

            bottom_lat=list(1)
            left  _lon=list(2)
            top   _lat=list(3)
            size  _lon=list(4)
            size  _lat=list(5)

         CASE( 'R' )

c           Rectangular
            call DECODE_LIST_FLT (data_string(n2+1:),6,nrlist,list)

            if (nrlist .lt. 6) then
               write(error_msg,
     &             '(''[DECODE_MAP_AREA]: '',
     &               ''Insufficient number of parameters to define '',
     &               ''rectangular map area'')')
               call LWPC_ERROR ('ERROR', error_msg)
            end if

            bottom_lat=list(1)
            left  _lon=list(2)
            top   _lat=list(3)
            right _lon=list(4)
            size  _lon=list(5)
            size  _lat=list(6)

         CASE( 'M' )

c           Mercator
            call DECODE_LIST_FLT (data_string(n2+1:),6,nrlist,list)

            if (nrlist .lt. 6) then
               write(error_msg,
     &             '(''[DECODE_MAP_AREA]: '',
     &               ''Insufficient number of parameters to define '',
     &               ''mercator map area'')')
               call LWPC_ERROR ('ERROR', error_msg)
            end if

            bottom_lat=list(1)
            left  _lon=list(2)
            top   _lat=list(3)
            right _lon=list(4)
            size  _lon=list(5)
            size  _lat=list(6)

         CASE DEFAULT

c           Unidentified projection
            write(error_msg,
     &          '(''[DECODE_MAP_AREA]: '',
     &            ''Projection: '',a'', is not available'')')
     &              projection
            call LWPC_ERROR ('ERROR', error_msg)

         END SELECT

      end if

      if (verify) then

         if (nritem .gt. 1 .and. identification .ne. 'dummy') then

c           Check map boundaries against the master list;
c           NOTE we do not check the physical dimensions.

            SELECT CASE( prjctn )

            CASE( 'A','G','O','S' )

c              Azimuthal, Gnomonic, Orthographic or Stereographic
               value1x=bottom_lat
               value2x=left  _lon
               value3x=top   _lat
               value4x=size  _lon
               value5x=size  _lat

            CASE( 'R','M' )

c              Rectangular or Mercator
               value1x=bottom_lat
               value2x=left  _lon
               value3x=top   _lat
               value4x=right _lon
               value5x=size  _lon
               value6x=size  _lat

            END SELECT

            input_file=lwpcDAT_loc(1:STR_LENGTH(lwpcDAT_loc))//'map.lis'
            call GET_MAP (input_file,
     &                    identification,projection,
     &                    value1x,value2x,value3x,value4x,
     &                    value5x,value6x)

            prjctn=projection
            call STR_UPPER (prjctn,0,0)

            SELECT CASE( prjctn )

            CASE( 'A','G','O','S' )

c              Azimuthal, Gnomonic, Orthographic or Stereographic
               if (value1x .ne. bottom_lat .or.
     &             value2x .ne. left  _lon .or.
     &             value3x .ne. top   _lat) then

                  write(error_msg,
     &                '(''[DECODE_MAP_AREA]: '',
     &                  ''Input does not match master file'')')
                  call LWPC_ERROR ('ERROR', error_msg)
               end if

            CASE( 'R','M' )

c              Rectangular or Mercator
               if (value1x .ne. bottom_lat .or.
     &             value2x .ne. left  _lon .or.
     &             value3x .ne. top   _lat .or.
     &             value4x .ne. right _lon) then

                  write(error_msg,
     &                '(''[DECODE_MAP_AREA]: '',
     &                  ''Input does not match master file'')')
                  call LWPC_ERROR ('ERROR', error_msg)
               end if

            CASE DEFAULT

c              Unidentified projection
               write(error_msg,
     &             '(''[DECODE_MAP_AREA]: '',
     &               ''Projection '',a'', is not available'')')
     &                 projection
               call LWPC_ERROR ('ERROR', error_msg)

            END SELECT

         end if
      end if
      RETURN
      END      ! DECODE_MAP_AREA