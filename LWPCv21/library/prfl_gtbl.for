      SUBROUTINE PRFL_GTBL
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,lat,lon,beta,hprime,pindex)

c***********************************************************************

c Set up tabular profiles vs. geographic position

c    lon_min         starting longitude value; units West
c    lon_inc         longitude increment; units
c    lon_factor      conversion factor from DEGREES to UNITS; i.e.,
c                       units = degrees * factor
c    nrlon           number of longitude intervals

c    lat_min         starting latitude  value; units North
c    lat_inc         latitude  increment; units
c    lat_factor      conversion factor from DEGREES to UNITS; i.e.,
c                       degrees = units / factor
c    nrlon           number of latitude  intervals

c    ndx             array of profile indices arranged as (lon,lat)

c********************!**************************************************

      include       'lwpc_cfg.cmn'

      real           hten,algen,htnu,algnu,charge,ratiom,
     &               select_hgts,hgts

      common/lwpc_pr/hten(51),algen(51,3),nrhten,ihten,
     &               htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &               charge(3),ratiom(3),nrspec,lu_prf,
     &               select_hgts(2),hgts(3)

c     Local
      parameter     (mxlon=144,mxlat=36,nrprflin=205)

      character*(*)  prfl_file,prfl_id
      character* 40  prfl_model
      character*120  string,file_name,directory,root_file,extension
      character*200  error_msg
      logical        initialize,flag
      integer        str_length,
     &               print_hgts,pindex,prev_ndx,
     &               ndx(mxlon,mxlat)
      real           lat,lon


      if (initialize) then

c        Get file name
         file_name=prfl_file
         call DECODE_FILE_NAME
     &       (file_name,directory,root_file,extension)

c        Check for a file named "PRFL_FILE".NDX
         if (STR_LENGTH(directory) .eq. 0) then
            if (STR_LENGTH(lwpcNDX_loc) .eq. 0) then
               file_name=root_file(:STR_LENGTH(root_file))//'.ndx'
            else
               file_name=lwpcNDX_loc(:STR_LENGTH(lwpcNDX_loc))//
     &                   root_file(:STR_LENGTH(root_file))//'.ndx'
            end if
         else
            file_name=directory(:STR_LENGTH(directory))//
     &                root_file(:STR_LENGTH(root_file))//'.ndx'
         end if

         INQUIRE (file=file_name,exist=flag)
         if (flag) then

            OPEN (lu_prf,file=file_name,status='old',
     &                   form='unformatted')

            read (lu_prf) lon_min,lon_inc,lon_factor,nrlon,
     &                    lat_min,lat_inc,lat_factor,nrlat,
     &                  ((ndx(i,j),i=1,nrlon),j=1,nrlat)

c           Set up boundaries of the geographic grid
            xlon_min=FLOAT(lon_min)/FLOAT(lon_factor)
            xlon_inc=FLOAT(lon_inc)/FLOAT(lon_factor)
            xlon_max=xlon_min*xlon_inc*(nrlon-1)

            xlat_min=FLOAT(lat_min)/FLOAT(lat_factor)
            xlat_inc=FLOAT(lat_inc)/FLOAT(lat_factor)
            xlat_max=xlat_min*xlat_inc*(nrlat-1)

            CLOSE(lu_prf)
         else

            write(error_msg,
     &          '(''[PRFL_GTBL]: '',
     &            ''Index file not found'',a)')
     &              prfl_file
            call LWPC_ERROR('ERROR', error_msg)
         end if

c        Check for a file named "PRFL_FILE"000.PRF.
c        NOTE that if this file doesn't exist, then PRFL_INIT_TABLE
c        simply initializes the profile parameters.
         if (STR_LENGTH(directory) .eq. 0) then
            if (STR_LENGTH(lwpcPRF_loc) .eq. 0) then
               file_name=root_file(:STR_LENGTH(root_file))//'000.prf'
            else
               file_name=lwpcPRF_loc(:STR_LENGTH(lwpcPRF_loc))//
     &                   root_file(:STR_LENGTH(root_file))//'000.prf'
            end if
         else
            file_name=directory(:STR_LENGTH(directory))//
     &                root_file(:STR_LENGTH(root_file))//'000.prf'
         end if

         call PRFL_INIT_TABLE
     &       (lu_prf,file_name,
     &        string,prfl_model,
     &        nrspec,charge,ratiom,
     &        51,nrhten,hten,algen,
     &           nrhtnu,htnu,algnu)

c        Make sure that the model name is upper case
         call STR_UPPER (prfl_model,0,0)

c        Re-structure the profile identification originally obtained
c        from PRFL_INIT_TABLE.
         prfl_id='Grd Tbl: '//string(:STR_LENGTH(string))

         if (prfl_model(1:1) .eq. 'P') then

c           Direct access file for PRFLSCN
            if (STR_LENGTH(directory) .eq. 0) then
               if (STR_LENGTH(lwpcPRF_loc) .eq. 0) then
                  file_name=root_file(:STR_LENGTH(root_file))//'.prf'
               else
                  file_name=lwpcPRF_loc(:STR_LENGTH(lwpcPRF_loc))//
     &                      root_file(:STR_LENGTH(root_file))//'.prf'
               end if
            else
               file_name=directory(:STR_LENGTH(directory))//
     &                   root_file(:STR_LENGTH(root_file))//'.prf'
            end if

            OPEN (lu_prf,file=file_name,status='old',
     &                   access='direct',recl=nrprflin)
         end if

c        Set previous index
         prev_ndx=0

      else

c        Find the profile index
         if (lon .gt. xlon_min) then
            tmp=lon-360.
         else
            tmp=lon
         end if
         nlon=((xlon_min-tmp)/xlon_inc)+1
         nlat=((lat-xlat_min)/xlat_inc)+1

         if (nlon .lt. 1 .or. nlon .gt. nrlon .or.
     &       nlat .lt. 1 .or. nlat .gt. nrlat) then

            write(error_msg,
     &          '(''[PRFL_GTBL]: '',
     &            ''Invalid position index '')')
            call LWPC_ERROR(' ', error_msg)
            write(error_msg,
     &          '(''   lon  lon  min  inc fctr'',
     &            ''   lat  lat  min  inc fctr index'')')
            call LWPC_ERROR(' ', error_msg)
            write(error_msg,
     &          '(f7.1,4i5,f7.1,4i5)')
     &            lon,nlon,lon_min,lon_inc,lon_factor,
     &            lat,nlat,lat_min,lat_inc,lat_factor
            call LWPC_ERROR(' ', error_msg)
            call LWPC_ERROR('ERROR',' ')
         end if

         pindex=ndx(nlon,nlat)

         if (pindex .ne. prev_ndx) then

            if (prfl_model(1:1) .eq. 'F' .or.
     &          prfl_model(1:1) .eq. 'U') then

c              Check for a file named "PRFL_FILE"nnn.PRF

               if (STR_LENGTH(directory) .eq. 0) then
                  if (STR_LENGTH(lwpcPRF_loc) .eq. 0) then
                     write(file_name,'(a,i3.3,''.prf'')')
     &                     root_file(:STR_LENGTH(root_file)),pindex
                  else
                     write(file_name,'(a,a,i3.3,''.prf'')')
     &                     lwpcPRF_loc(:STR_LENGTH(lwpcPRF_loc)),
     &                     root_file(:STR_LENGTH(root_file)),pindex
                  end if
               else
                  write(file_name,'(a,a,i3.3,''.prf'')')
     &                  directory(:STR_LENGTH(directory)),
     &                  root_file(:STR_LENGTH(root_file)),pindex
               end if

               INQUIRE (file=file_name,exist=flag)
               if (flag) then

                  if (prfl_model(1:1) .eq. 'F') then

c                    Formatted profile
                     call PRFL_INIT_TABLE
     &                   (lu_prf,file_name,
     &                    string,prfl_model,
     &                    nrspec,charge,ratiom,
     &                    51,nrhten,hten,algen,
     &                       nrhtnu,htnu,algnu)
                  else
     &            if (prfl_model(1:1) .eq. 'U') then

c                    Unformatted profile
                     OPEN (lu_prf,file=file_name,status='old',
     &                            form='unformatted')

                     read (lu_prf) nrsp,nrhten,
     &                            (hten(n),
     &                            (algen(n,k),k=1,nrsp),n=1,nrhten)

                     CLOSE(lu_prf)
                  end if
               else

                  write(error_msg,
     &                '(''[PRFL_GTBL]: '',
     &                  '' Profile file not found: '',a)')
     &                     prfl_file(:MAX(1,STR_LENGTH(prfl_file)))
                  call LWPC_ERROR('ERROR', error_msg)
               end if
            else
     &      if (prfl_model(1:1) .eq. 'P') then

c              Direct access file for PRFLSCN
               read (lu_prf,rec=pindex) hten,algen,nrhten
            else

               write(error_msg,
     &             '(''[PRFL_GTBL]: '',
     &               ''PRFL_MODEL: '',a,'' is invalid'')')
     &                 prfl_model(:MAX(1,STR_LENGTH(prfl_model)))
               call LWPC_ERROR('ERROR', error_msg)
            end if

c           Get reference heights for current profile.
            call PRFL_HGTS (print_hgts)

            bb=99.
            hh=AINT( 10.*hgts(2)+.5)/10.

c           Set previous index
            prev_ndx=pindex
         end if

         beta=bb
         hprime=hh
      end if

      RETURN
      END      ! PRFL_GTBL