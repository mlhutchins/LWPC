      SUBROUTINE PRFL_SPECIFICATION
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,pflag,
     &           freq,month,day,year,UT,
     &           lat,lon,range,dip,zenith,
     &           hpr_mid,beta,hprime,pindex)

c***********************************************************************

c     Pflag         Profile specification
c      0            homogeneous table prfl_file
c      1            homogeneous exponential beta hprime
c      2            lwpm day
c      3            lwpm night
c      4            lwpm month/day/year hour:minute
c      5            range table prfl_file
c      6            range exponential prfl_file
c      7            grid table prfl_file
c      8 RESERVED   grid exponential prfl_file
c      9            chi table prfl_file
c     10            chi exponential prfl_file

c***********************************************************************

      character*(*) prfl_id,prfl_file
      logical       initialize
      integer       print_hgts,pflag,pindex,
     &              day,year,UT
      real          lat,lon

      character*200 save_file,
     &              error_msg


      if (initialize) then

         select case (pflag)

         case (0)

c           Homogeneous tabular profile
            call PRFL_HTBL
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,beta,hprime)

            hpr_mid=80.
            pindex=0

c           Save the profile file specification
            save_file=' '

         case (1)

c           Homogeneous exponential profile
           call PRFL_EXP (beta,hprime)

            write(prfl_id,
     &          '(''Homogeneous profile: '',f7.5,''/'',f4.1)')
     &            beta,hprime

c           Get reference heights for current profile.
            call PRFL_HGTS (print_hgts)

            hpr_mid=80.
            pindex=0

c           Save the profile file specification
            save_file=' '

         case (2:4)

c           Profile varies according to LWPM
            call PRFL_LWPM
     &          (initialize,0,
     &           pflag,prfl_id,
     &           freq,month,day,year,UT,dip,zenith,
     &           hpr_mid,beta,hprime,pindex)

c           Save the profile file specification
            save_file=' '

         case (5)

c           Tabular profile varying along path
            call PRFL_RTBL
     &          (initialize,0,
     &           prfl_file,prfl_id,range,beta,hprime,pindex)

            hpr_mid=80.

c           Save the profile file specification
            save_file=prfl_file

         case (6)

c           Exponential profile varying along path
            call PRFL_REXP
     &          (initialize,0,
     &           prfl_file,prfl_id,range,beta,hprime,pindex)

            hpr_mid=80.

c           Save the profile file specification
            save_file=prfl_file

         case (7)

c           Tabular profile in a lat-lon grid
            call PRFL_GTBL
     &          (initialize,0,
     &           prfl_file,prfl_id,lat,lon,beta,hprime,pindex)

            hpr_mid=80.

c           Save the profile file specification
            save_file=prfl_file

         case (9)

c           Tabular profile varying with solar zenith angle
            call PRFL_CHI_TBL
     &          (initialize,0,
     &           prfl_file,prfl_id,zenith,beta,hprime,pindex)

            hpr_mid=80.

c           Save the profile file specification
            save_file=prfl_file

         case (10)

c           Exponential profile varying with solar zenith angle
            call PRFL_CHI_EXP
     &          (initialize,0,
     &           prfl_file,prfl_id,zenith,beta,hprime,pindex)

            hpr_mid=80.

c           Save the profile file specification
            save_file=prfl_file

         case default

            write(error_msg,
     &          '(''[PRFL_INIT]: '',
     &            ''PFLAG = '',i2,'' is invalid'')')
     &              pflag
            call LWPC_ERROR('ERROR', error_msg)

         end select
      else

         select case (pflag)

         case (0:1)

c           Homogeneous profile table (assumed already stored)
c              or
c           Homogeneous profile exponential
            beta=bb
            hprime=hh
            hpr_mid=hm
            pindex=nx

         case (2:4)

c           Profile varies according to LWPM
            call PRFL_LWPM
     &          (initialize,print_hgts,
     &           pflag,prfl_id,
     &           freq,month,day,year,UT,dip,zenith,
     &           hpr_mid,beta,hprime,pindex)

         case (5)

c           Tabular profile varying along path;
c           use the range and associated profile index
c           to retrieve the profile parameters.

c           Restore the profile file specification
            prfl_file=save_file

            call PRFL_RTBL
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,range,beta,hprime,pindex)

            hpr_mid=hm

         case (6)

c           Exponential profile varying along path;
c           use the range to retrieve the profile parameters.

c           Restore the profile file specification
            prfl_file=save_file

            call PRFL_REXP
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,range,beta,hprime,pindex)

            hpr_mid=hm

         case (7)

c           Tabular profile in a lat-lon grid;
c           use position to retrieve the profile parameters.

c           Restore the profile file specification
            prfl_file=save_file

            call PRFL_GTBL
     &          (initialize,print_hgts,
     &           prfl_file,prfl_id,lat,lon,beta,hprime,pindex)

            hpr_mid=hm

         case (9)

c           Tabular profile varying along with solar zenith angle;
c           use solar zenith angle to retrieve the profile parameters.

c           Restore the profile file specification
            prfl_file=save_file

            call PRFL_CHI_TBL
     &          (initialize,0,
     &           prfl_file,prfl_id,zenith,beta,hprime,pindex)

            hpr_mid=hm

         case (10)

c           Exponential profile varying with solar zenith angle;
c           use solar zenith angle to retrieve the profile parameters.

c           Restore the profile file specification
            prfl_file=save_file

            call PRFL_CHI_EXP
     &          (initialize,0,
     &           prfl_file,prfl_id,zenith,beta,hprime,pindex)

            hpr_mid=hm

         case default

            write(error_msg,
     &          '(''[PRFL_SPECIFICATION]: '',
     &            ''PFLAG = '',i2,'' is invalid'')')
     &              pflag
            call LWPC_ERROR('ERROR', error_msg)

         end select
      end if

c     Save values
      bb=beta
      hh=hprime
      hm=hpr_mid
      nx=pindex

      RETURN
      END      ! PRFL_SPECIFICATION