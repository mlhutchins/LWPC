      SUBROUTINE GET_MAP
     &          (file_name,map_id,prjctn,
     &           param1,param2,param3,param4,param5,param6)

c***********************************************************************
c                         subroutine get_map
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Gets map area parameters from data file if a match is found
c     between the input identification;
c     otherwise, adds the parameters to the end of the list.

c  Parameters passed:
c     file_name        [s] name of the data file
c     map_id           [s] map area identification

c     prjctn           [s] projection
c                          rect:  linear in latitude and longitude
c                          merc:  Mercator
c                          gnom:  gnomonic
c                          azim:  azimuthal equidistant
c                          ortho: orthographic

c     param1           [r] parameters as required by the projection
c     param2
c     param3
c     param4
c     param5
c     param6

c  Parameters returned:
c     prjctn           [s] projection
c                          rect:  linear in latitude and longitude
c                          merc:  Mercator
c                          gnom:  gnomonic
c                          azim:  azimuthal equidistant
c                          ortho: orthographic

c     param1           [r] parameters as required by the projection
c     param2
c     param3
c     param4
c     param5
c     param6

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     close
c     open
c     read
c     write

c     decode_list_flt
c     str_lower
c     str_upper

c  References:

c  Change History:
c     23 Oct 90     Modified to check the parameters of the map and
c                   return the name of the map if one is found which
c                   has the same parameters as specified by the user.

c     12 Jan 94     Added tests to ensure valid definition of
c                   map area.

c     31 Jan 94     Added error checking on open statement.

c     06 Jul 94     Added orthographic projection.

c     28 Sep 95     Modified for use with the new geophysics routines.
c                   The new routines allow the azim, gnom and ortho maps
c                   to have rectangular displays; the gnom definition is
c                   now based on a center coordinate and range like the
c                   azim and ortho maps.

c     21 Oct 95     Changed to get the logical unit from LWPC_LUN.CMN.

c  Notes:
c     If prjctn='rect' or 'merc':
c        param1,2,3,4,5,6 are lat1,lon1,lat2,lon2,sizex,sizey

c     If prjctn='gnom':
c        param1,2,3,4,5   are lat0,lon0,range,sizex,sizey
c        param6           is  undefined

c     If prjctn='azim':
c        param1,2,3,4,5   are lat0,lon0,range,sizex,sizey
c        param6           is  undefined

c     If prjctn='ortho':
c        param1,2,3,4,5   are lat0,lon0,range,sizex,sizey
c        param6           is  undefined

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      character*(*) file_name,map_id,prjctn

      character*  8 prjx
      character* 20 mapx,test
      character*120 bcd
      character*200 error_msg

      dimension     values(6)


      OPEN (lwpcDAT_lun,file=file_name,status='old',
     &                  iostat=iocheck,err=900)

c     The data file records are to contain:

c     MAP_ID,PROJECTION,PARAM1,PARAM2...

c     If prjctn='rect' or 'merc':
c        param1,2,3,4,5,6 are lat1,lon1,lat2,lon2,sizex,sizey

c     If prjctn='gnom':
c        param1,2,3,4,5   are lat0,lon0,range,sizex,sizey
c        param6           is not used

c     If prjctn='azim':
c        param1,2,3,4,5   are lat0,lon0,range,sizex,sizey
c        param6           is not used

c     If prjctn='ortho':
c        param1,2,3,4,5   are lat0,lon0,range,sizex,sizey
c        param6           is not used


c     Check for a match between the input map-id and those found in the
c     map specification file.

      test=map_id
      call STR_UPPER (test,0,0)
      mapx='   '
      do while (mapx(1:3) .ne. 'END')
         read (lwpcDAT_lun,'(a)') bcd
         call STR_GET_ITEM (1,bcd,mapx,n1,n2)
         call STR_UPPER (mapx,0,0)
         if (mapx .eq. test) then

            CLOSE(lwpcDAT_lun)

            call STR_GET_ITEM (2,bcd,prjx,n1,n2)

            if (bcd(n2+1:n2+1) .eq. ',') n2=n2+1

            prjctn=prjx
            call STR_LOWER (prjctn,0,0)
            if (prjctn(1:4) .ne. 'rect' .and.
     &          prjctn(1:4) .ne. 'merc' .and.
     &          prjctn(1:4) .ne. 'gnom' .and.
     &          prjctn(1:4) .ne. 'azim' .and.
     &          prjctn(1:4) .ne. 'orth') then

               write(error_msg,
     &             '(''[GET_MAP]:'',
     &               ''Projection name: '',a,1x,
     &               ''not recognized'')') prjctn
               call LWPC_ERROR ('ERROR',error_msg)
            else

               call DECODE_LIST_FLT (bcd(n2+1:),6,nrvalues,values)

               param1=values(1)
               param2=values(2)
               param3=values(3)
               param4=values(4)
               param5=values(5)
               param6=values(6)
               RETURN
            end if
         end if
      end do

c     The named map was not found;
c     check for a match between the input map parameters and those
c     found in the map specification file;
c     NOTE: The size of the maps is not checked.

      if (prjctn(1:4) .eq. 'merc' .or.
     &    prjctn(1:4) .eq. 'rect') then

         if (param1 .eq. 0. .and. param2 .eq. 0. .and.
     &       param3 .eq. 0. .and. param4 .eq. 0.) then
            write(error_msg,
     &          '(''[GET_MAP]: Map area coordinates are missing'')')
            call LWPC_ERROR ('ERROR', error_msg)
         end if
      else
     &if (prjctn(1:4) .eq. 'azim' .or.
     &    prjctn(1:4) .eq. 'gnom' .or.
     &    prjctn(1:4) .eq. 'orth') then

         if (param1 .eq. 0. .and. param2 .eq. 0. .and.
     &       param3 .eq. 0.) then
            write(error_msg,
     &          '(''[GET_MAP]: Map area coordinates are missing'')')
            call LWPC_ERROR ('ERROR', error_msg)
         end if
      else

         write(error_msg,
     &       '(''[GET_MAP]: '',
     &         ''Projection name: '',a,1x,
     &         ''not recognized'')') prjctn
         call LWPC_ERROR ('ERROR',error_msg)
      end if

      REWIND (lwpcDAT_lun)
      test=prjctn
      call STR_LOWER (test,0,0)

20    read (lwpcDAT_lun,'(a)',end=30) bcd
      call STR_GET_ITEM (1,bcd,mapx,n1,n2)
      call STR_UPPER (mapx,0,0)
      if (mapx(1:3) .ne. 'END' .and.
     &    mapx(1:6) .ne. 'MAP-ID' .and.
     &    mapx(1:6) .ne. 'MAP_ID') then

         call STR_GET_ITEM (2,bcd,prjx,n1,n2)
         call STR_LOWER (prjx,0,0)
         if (prjx(1:4) .eq. test(1:4)) then

            if (bcd(n2+1:n2+1) .eq. ',') n2=n2+1

            call DECODE_LIST_FLT (bcd(n2+1:),6,nrvalues,values)

            param1x=values(1)
            param2x=values(2)
            param3x=values(3)
            param4x=values(4)
            param5x=values(5)
            param6x=values(6)

            if (prjx(1:4) .eq. 'rect' .or.
     &          prjx(1:4) .eq. 'merc') then

c              Just check the boundaries
               if (param1x .eq. param1 .and. param2x .eq. param2 .and.
     &             param3x .eq. param3 .and. param4x .eq. param4) then

                  map_id=mapx
                  CLOSE(lwpcDAT_lun)
                  RETURN
               end if
            else
     &      if (prjx(1:4) .eq. 'azim' .or.
     &          prjx(1:4) .eq. 'gnom' .or.
     &          prjx(1:4) .eq. 'orth') then

c              Just check the center and the range
               if (param1x .eq. param1 .and. param2x .eq. param2 .and.
     &             param3x .eq. param3) then

                  map_id=mapx
                  CLOSE(lwpcDAT_lun)
                  RETURN
               end if
            end if
         end if
      end if
      go to 20

30    BACKSPACE (lwpcDAT_lun)
      BACKSPACE (lwpcDAT_lun)

      call STR_LOWER (prjctn,0,0)
      if (prjctn(1:4) .eq. 'rect' .or.
     &    prjctn(1:4) .eq. 'merc') then

         write(lwpcDAT_lun,
     &       '(a20,1x,a5,1x,f6.1,2x,f7.1,2x,f6.1,2x,f7.1,f5.1,f6.1)')
     &         map_id,prjctn,
     &         param1,param2,param3,param4,param5,param6
      else
     &if (prjctn(1:4) .eq. 'azim' .or.
     &    prjctn(1:4) .eq. 'gnom' .or.
     &    prjctn(1:4) .eq. 'orth') then

         write(lwpcDAT_lun,
     &       '(a20,1x,a5,1x,f8.3,f9.3,f8.1,7x,f5.1,f6.1)')
     &         map_id,prjctn,
     &         param1,param2,param3,param4,param5
      end if
      write(lwpcDAT_lun,'(''end'')')
      CLOSE(lwpcDAT_lun)
      RETURN

c     Error
900   write(error_msg,
     &    '(''[GET_MAP]: '',
     &      ''I/O error '',i3,'' occurred trying to open '',
     &      ''file: '',a)') iocheck,file_name
      call LWPC_ERROR ('ERROR',error_msg)

      END      ! GET_MAP