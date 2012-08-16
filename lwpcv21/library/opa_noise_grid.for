      SUBROUTINE OPA_NOISE_GRID
     &          (lu_grd,print_grd,
     &           prgm_id,
     &           area_id,xlat1,xlon1,xlat2,xlon2,
     &           n_model,freq,month,day,year,UT,bandw,ralt,
     &           mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &           mxprm,nrprm,param,
     &           nrcmp,mxlat,nrlat,mxlon,nrlon,
     &           xclt,xlng,amp_grd,sig_grd,Vsd_grd,
     &           data_format)

c***********************************************************************

c Fills geographic grid with values for atmospheric noise

c***********************************************************************

c     lu_grd            logical unit for noise grid file
c     print_grd         integer flag to print summary information

c     prgm_id           program id for labeling output file

c     area_id           op area identification; 8 characters
c     xlat1             lower bound of the op area; degrees N
c     xlon1             right bound of the op area; degrees W
c     xlat2             upper bound of the op area; degrees N
c     xlon2             left  bound of the op area; degrees W

c     n_model           noise model name:  ITSN or NTIA
c     freq              frequency; kHz
c     month             month number; January is 1
c     day               day of the month
c     year              year
c     UT                Universal Time; hours and minutes
c     bandw             band width; Hz
c     ralt              receiver altitude; km

c     mxpath
c     nrpath
c     bearing
c     rhomax
c     rxlat
c     rxlon

c     mxprm             maximum number of parameters
c     nrprm                     number of parameters
c     param                               parameters

c     nrcmp             number of components of noise to compute
c     mxlat             dimension of the grid arrays in latitude
c     nrlat             output number of points in latitude
c     mxlon             dimension of the grid arrays in longitude
c     nrlon             output number of points in longitude

c     xclt
c     xlng

c     amp_grd
c     sig_grd
c     Vsd_grd

c     data_format   [s] 'b' if data is in binary format
c                       'a' if data is in ASCII format

c***********************************************************************

c  Change history:

c     03 Jun 92     Removed references for the DECO noise model per
c                   JAF's direction that the DECO model would not be
c                   used on the PC; added grd_dir parameter to allow
c                   for the specification of the grid file location.

c     19 Jan 94     Changed grd_dir to character*(*) and put in tests
c                   for the length of the grid directory.

c     21 Jan 94     Dropped lwpcGRD_loc from argument list; does not
c                   open noise grid file; always calculates noise data.

c     29 Jan 94     Added ASCII/BINARY option.

c     06 Apr 94     Changed output to be dB; output the standard
c                   deviation as dB instead of weighting it by the
c                   signal amplitude.

c     30 Jun 94     Dropped PRFL_ID, CXCLT and SXCLT from argument list.

c     30 Jan 96     Added V sub d to the output grid file.

c*******************!***************************************************

      character*(*) data_format
      character*  4 n_model
      character*  8 archive,prgm_id
      character* 20 xmtridx,pathidx,area_id
      character* 40 prflidx
      character* 80 caseidx
      character*120 file_id(3)
      character*200 error_msg
      logical       begin_file
      integer       print_grd,
     &              day,year,UT
      real          param(mxprm),
     &              bearing(mxpath),rhomax(mxpath),
     &              rxlat(mxpath),rxlon(mxpath),
     &              xclt(mxlat),xlng(mxlon),
     &              amp_grd(mxlon,mxlat,2),sig_grd(mxlon,mxlat,2),
     &              Vsd_grd(mxlon,mxlat,2)


      if (n_model .eq. 'itsn' .or. n_model .eq. 'ITSN' .or.
     &    n_model .eq. 'ntia' .or. n_model .eq. 'NTIA') then

c        Calculate noise data
         hr=(UT-40*(UT/100))/60.
         do l=1,nrlat
            do k=1,nrlon
               if (n_model .eq. 'itsn' .or. n_model .eq. 'ITSN') then

                  call NOISE_ITSN
     &                (xclt(l),xlng(k),freq,bandw,month,hr,
     &                 Fam,sigm,Vd,sigv,Du,sigu,Dl,sigl,as,ss)

               else

                  call NOISE_NTIA
     &                (xclt(l),xlng(k),freq,bandw,month,hr,
     &                 Fam,sigm,Vd,sigv,Du,sigu,Dl,sigl,aa,ss)

               end if
               amp_grd(k,l,1)=aa
               sig_grd(k,l,1)=ss
               Vsd_grd(k,l,1)=Vd
               amp_grd(k,l,2)=aa
               sig_grd(k,l,2)=ss
               Vsd_grd(k,l,2)=Vd
            end do
         end do

         if (nrcmp .gt. 1) then

c           Get adjustment factor for TE noise
            adjny=0.

cxx            if (pflag .eq. 3) then
cxxc              Night
cxx               adjny=17.5-30.56*LOG10(freq/17.)
cxx            else
cxxc              Day or nuclear
cxx               adjny=31.0-21.59*LOG10(freq/17.)
cxx            end if

            do l=1,nrlat
               do k=1,nrlon
                  amp_grd(k,l,2)=amp_grd(k,l,2)-adjny
               end do
            end do
         end if

c        Save this noise data.
         nrgrd=1

Change 13 Dec 1990: Make sure that the parameters are set correctly
c                   for noise grids.
         param_01 =param( 1)
         param_02 =param( 2)
         param_03 =param( 3)
         param_04 =param( 4)
         param_05 =param( 5)

         param( 1)=0.
         param( 2)=0.
         param( 3)=0.
         param( 4)=0.
         param( 5)=0.

c        Save the noise parameters.
         param( 6)=ralt
         param( 7)=month
         param( 8)=day
         param( 9)=year
         param(10)=UT
         param(11)=bandw
         param(12)=adjny

c        Tell the READ/WRITE_GRD routines that there is an extra array
c        after the mean and standard deviation arrays.
         param(13)=1.
         nrprm=13

c        Set up unique file identification
         archive   ='***'
         file_id(1)='***'
         file_id(2)='***'
         call SET_FILE_ID (lu_grd,prgm_id,file_id(3))

         caseidx   ='Noise'
         prflidx   =n_model
         xmtridx   =' '
         pathidx   =' '
         txlat     =0.
         txlon     =0.
         nrpath    =1
         bearing(1)=0.
         rhomax (1)=0.
         rxlat  (1)=0.
         rxlon  (1)=0.

         call WRITE_HDR
     &       (lu_grd,print_grd,
     &        archive,file_id,prgm_id,
     &        caseidx,prflidx,
     &        xmtridx,freq,txlat,txlon,
     &        pathidx,oplat1,oplon1,oplat2,oplon2,
     &        mxpath,nrpath,bearing,rhomax,rxlat,rxlon,
     &        begin_file,
     &        data_format)

         do ncmp=1,nrcmp

c           First, write out the mean and standard deviation arrays
            call WRITE_GRD
     &          (lu_grd,print_grd,
     &           mxprm,nrprm,param,nrcmp,nrgrd,
     &           area_id,xlat1,xlon1,xlat2,xlon2,
     &           mxlat,nrlat,mxlon,nrlon,
     &           amp_grd(1,1,ncmp),sig_grd(1,1,ncmp),
     &           begin_file,
     &           data_format)

c           Next, write out the V sub d array
            call WRITE_GRD
     &          (lu_grd,print_grd,
     &           mxprm,nrprm,param,nrcmp,nrgrd,
     &           area_id,xlat1,xlon1,xlat2,xlon2,
     &           mxlat,nrlat,mxlon,nrlon,
     &           Vsd_grd(1,1,ncmp),sig_grd(1,1,ncmp),
     &           begin_file,
     &           data_format)
         end do

c        Re-store the parameters for the signal grid:
         param( 1)=param_01
         param( 2)=param_02
         param( 3)=param_03
         param( 4)=param_04
         param( 5)=param_05
      else

         write(error_msg,
     &       '(''[OPA_NOISE_GRD]: '',
     &         ''Noise model name ('',a,'') is invalid'')')
     &           n_model
         call LWPC_ERROR ('ERROR', error_msg)
      end if

      RETURN
      END      ! OPA_NOISE_GRID