      SUBROUTINE LWP_EOF_MDS
     &          (lu_mds,
     &           file_id,xmtridx,pathidx,
     &           mxpath,bearing,rhomax,rxlat,rxlon,
     &           mxprm1,param1,
     &           mxeign,eigen,
     &           mxprm2,param2,
     &           number_of_paths)

c     Input:

c     lu_mds        logical unit for mode file
 
c     mxpath        maximum number of paths
c     bearing       bearings of the paths
c     rhomax        maximum range of the paths
c     rxlat         receiver latitudes
c     rxlon         receiver longitudes
 
c     mxprm1        maximum number of path parameters
c     param1                          path parameters
 
c     mxeign        maximum number of eigen solutions
c     param1                          path parameters
 
c     mxprm2        maximum number of mode parameters
c     param2                          mode parameters

c     Output:

c     file_id       file id from the MDS file
c     xmtridx       xmtr id from the MDS file
c     pathidx       path id from the MDS file

c     number_of_paths
c                   number of complete paths found in the file

      character*  8 archive,prgmidx
      character* 20 xmtridx,pathidx
      character* 40 prflidx
      character* 80 caseidx
      character*120 file_id(3)
      logical       begin_file,end_file
      integer       print_mds/0/
      real          bearing(mxpath),rhomax(mxpath),
     &              rxlat(mxpath),rxlon(mxpath),
     &              param1(mxprm1)
      complex       eigen(mxeign),param2(mxprm2,mxeign)

c     Get header
      call READ_HDR (lu_mds,print_mds,
     &               archive,file_id,prgmidx,
     &               caseidx,prflidx,
     &               xmtridx,freq,txlat,txlon,
     &               pathidx,oplat1x,oplon1x,oplat2x,oplon2x,
     &               mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &               begin_file,end_file,'Binary')

      if (end_file) then

c        The file does not contain a complete header
         number_of_paths=0
         REWIND (lu_mds)
         RETURN
      else

c        Count the number of complete paths
         number_of_paths=0
         do while (.not.end_file)

            call READ_MDS  (lu_mds,print_mds,
     &                      bearng,rhomx,rlat,rlon,rrho,
     &                      mxprm1,nrprm1,param1,
     &                      mxeign,nreign,eigen,
     &                      mxprm2,nrprm2,param2,
     &                      begin_file,end_file)

c           Check for end of file before the end of path
            if (end_file .and. param1(1) .ne. 99.) go to 99

c           Check for end of path
            if (.not.end_file .and. param1(1) .eq. 99.) 

     &         number_of_paths=number_of_paths+1
         end do
      end if

c     At the end of the file; check for end of path
      if (param1(1) .eq. 99.) RETURN

c     Position the file at the end of the last complete path
99    REWIND (lu_mds)

      begin_file=.true.
      call READ_HDR
     &    (lu_mds,print_mds,
     &     archive,file_id,prgmidx,
     &     caseidx,prflidx,
     &     xmtridx,freq,txlat,txlon,
     &     pathidx,oplat1x,oplon1x,oplat2x,oplon2x,
     &     mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &     begin_file,end_file,'Binary')

      nr_path=0
      do while (nr_path .lt. number_of_paths)

         call READ_MDS
     &       (lu_mds,print_mds,
     &        bearng,rhomx,rlat,rlon,rrho,
     &        mxprm1,nrprm1,param1,
     &        mxeigen,nreigen,eigen,
     &        mxprm2,nrprm2,param2,
     &        begin_file,end_file)

         if (param1(1) .eq. 99.) nr_path=nr_path+1
      end do
      RETURN
      END      ! LWP_EOF_MDS