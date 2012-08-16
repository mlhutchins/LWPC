      SUBROUTINE PRFL_INIT_TABLE
     &          (lu_prf,prfl_file,
     &           prfl_id,prfl_model,
     &           nrspec,charge,ratiom,
     &           mxhts,
     &           nrhten,hten,algen,
     &           nrhtnu,htnu,algnu)

c***********************************************************************

c     Get tabular profile parameters from logical unit LU_PRF

c***********************************************************************

c CONTROL STRINGS:

c SPECIES     number-of-species
c CHARGE      charge-e  charge-i1  charge-i2
c MASS-RATIO  ratio-e   ratio-i1   ratio-i2
c COEFF-NU    nu0-e     nu0-i1     nu0-i2
c EXP-NU      exp-e     exp-i1     exp-i2
c COLLISION-FREQUENCY-TABLE
c DENSITY-TABLE
c MODEL-PRF   FORMATTED
c MODEL-PRF   UNFORMATTED
c MODEL-PRF   MODEL-NAME

c***********************************************************************

c SPECIES          Specifies the number of species (max of 3)

c    INPUT PARAMETERS:
c    number-of-species


c CHARGE           Specifies the charge of each species (max of 3)

c    INPUT PARAMETERS:
c    charge-e         electron charge (always -1)
c    charge-i1        charge of positive ion species
c    charge-i2        charge of negative ion species


c MASS-RATIO       Specifies the mass of each species relative to that
c                  of an electron

c    INPUT PARAMETERS:
c    ratio-e          electron ratio (always 1)
c    ratio-i1         ratio of positive ion species
c    ratio-i2         ratio of negative ion species


c COEFF-NU         Collision frequency at the ground in collisions per
c                  second for exponential model

c    INPUT PARAMETERS:
c    nu0-e            value for electrons
c    nu0-i1           value for positive ion species
c    nu0-i2           value for negative ion species


c EXP-NU           slope of exponential decay of collision frequency in
c                  inverse km

c    INPUT PARAMETERS:
c    exp-e            value for electrons
c    exp-i1           value for positive ion species
c    exp-i2           value for negative ion species


c COLLISION-FREQUENCY-TABLE
c                  This control string followed by a table of collision
c                  frequencies as a function of height. The values are
c                  input in order of descending height as:

c                     HEIGHT NU-E NU-I1 NU-I2

c                  If NU-I2 is not specified, then it is assumed to be
c                  the same as NU-I1.

c    height           height in km
c    nu-e             collisions per second for electrons
c    nu-i1            collisions per second for first ion
c    nu-i2            collisions per second for second ion


c DENSITY-TABLE    This control string followed by a table of charged
c                  particle densities as a function of height. The
c                  values are input in order of descending height as:

c                     HEIGHT DEN-E DEN-I1

c                  The value of DEN-I2 is calculated from DEN-E and
c                  DEN-I1 to preserve charge neutrality.

c    height           height in km
c    den-e            charge density per cc for electrons
c    den-i1           charge density per cc for first ion


c MODEL-PRF        Specifies the format of charge density tables found
c                  in files of the name PRFL-FILEnnn.PRF

c    INPUT PARAMETERS:

c    FORMATTED        indicates the the files are formatted in the same
c                     way as described above under CHARGE-DENSITY-TABLE.

c    UNFORMATTED      indicates the the files are unformatted with the
c                     data being stored as follows:

c                        nrspec,nrhts,
c                       (hten(i),(algen(i,k),k=1,nrspec),i=1,nrhts)

c    MODEL-NAME       indicates the files are formatted according to a
c                     specific model which requires a corresponding
c                     input processing routine.

c***********************************************************************

c Change history:

c********************!**************************************************

      character*(*)  prfl_file,prfl_id,prfl_model
      character*120  string,control_string,data_string
      character*200  error_msg
      logical        first/.true./,flag
      integer        str_length
      real     *  4  hten(mxhts),algen(mxhts,3),
     &               htnu(mxhts),algnu(mxhts,3),
     &               charge(3),ratiom(3),
     &               coeff_nu(3),exp_nu(3)


      if (first) then

c        Set defaults
         prfl_id='DEFAULTS'
         prfl_model='FORMATTED'
         nrspec=1
         nrhten=0
         nrhtnu=0

         charge(1)=-1.
         charge(2)= 1.
         charge(3)=-1.
         ratiom(1)=1.
         ratiom(2)=58000.
         ratiom(3)=58000.

         coeff_nu(1)=1.816e11
         coeff_nu(2)=4.54e9
         coeff_nu(3)=4.54e9
         exp_nu(1)=-.15
         exp_nu(2)=-.15
         exp_nu(3)=-.15

         first=.false.
      end if

      INQUIRE (file=prfl_file,exist=flag)
      if (.not.flag) go to 99

      OPEN (lu_prf,file=prfl_file,status='old')

c     The first line in the file must be the profile identification
      read (lu_prf,'(a)',end=99) prfl_id

c     Get control string
10    read (lu_prf,'(a)',end=99) string

      if (string(1:1) .eq. ';') go to 10
      if (STR_LENGTH(string) .eq. 0) go to 10

c     Get the limits of the data within the control string
      call DECODE_CONTROL_DATA (string,control_string,data_string)

c     Convert the control string to upper case
      call STR_UPPER (control_string,0,0)

c SPECIES
      if (control_string(1:1) .eq. 'S') then

         call DECODE_LIST_INT
     &       (data_string,1,nrlist,nrspec)
         go to 10
      end if

c CHARGE
      if (control_string(1:2) .eq. 'CH') then

         charge(2)=0.
         charge(3)=0.

         call DECODE_LIST_FLT
     &       (data_string,3,nrlist,charge)

         if (charge(3) .eq. 0.) charge(3)=-charge(2)
         go to 10
      end if

c MASS-RATIO
      if (control_string(1:2) .eq. 'MA') then

         ratiom(2)=0.
         ratiom(3)=0.

         call DECODE_LIST_FLT
     &       (data_string,3,nrlist,ratiom)

         if (ratiom(3) .eq. 0.) ratiom(3)=ratiom(2)
         go to 10
      end if

c COEFF-NU
      if (control_string(1:3) .eq. 'COE') then

         coeff_nu(2)=0.
         coeff_nu(3)=0.

         call DECODE_LIST_FLT
     &       (data_string,3,nrlist,coeff_nu)

         if (coeff_nu(3) .eq. 0.) coeff_nu(3)=coeff_nu(2)
         go to 10
      end if

c EXP-NU
      if (control_string(1:1) .eq. 'E') then

         exp_nu(2)=0.
         exp_nu(3)=0.

         call DECODE_LIST_FLT
     &       (data_string,3,nrlist,exp_nu)

         if (exp_nu(3) .eq. 0.) exp_nu(3)=exp_nu(2)
         go to 10
      end if

c MODEL-PRF
      if (control_string(1:2) .eq. 'MO') then

         if (data_string(1:1) .eq. ' ') then
            write(error_msg,
     &          '(''[PRFL_INIT_TABLE]: Data string missing'')')
            call LWPC_ERROR('ERROR', error_msg)
         endif

         prfl_model=data_string
         call STR_UPPER (prfl_model,0,0)
         go to 10
      end if

c COLLISION-FREQUENCY-TABLE
      if (control_string(1:3) .eq. 'COL') then

         call PRFL_READ_TABLE
     &       (lu_prf,2,nrspec,mxhts,nrhtnu,htnu,algnu)
         go to 10
      end if

c DENSITY-TABLE
      if (control_string(1:1) .eq. 'D') then

         call PRFL_READ_TABLE
     &       (lu_prf,1,nrspec,mxhts,nrhten,hten,algen)
         go to 10
      end if

99    CLOSE(lu_prf)

      if (nrhtnu .eq. 0) then

c        Set exponential collision frequency
         nrhtnu=2
         htnu(1)=200.
         htnu(2)=0.
         do k=1,nrspec
            algnu(2,k)=LOG(coeff_nu(k))
            algnu(1,k)=algnu(2,k)+htnu(1)*exp_nu(k)
         end do
      end if

      RETURN
      END      ! PRFL_INIT_TABLE