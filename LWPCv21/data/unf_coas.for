      PROGRAM UNFORMAT_COAST

c     reads    formatted file: coast.fmt
c     writes unformatted file: coast$d.dat

      parameter    (mxp=10650)

      TYPE    segment
         integer*2  ndx
         integer*2  nrp
         integer*2  nrc
         integer*2  lat(mxp)
         integer*2  lon(mxp)
      END TYPE
      TYPE(segment) line

      logical       eof


      OPEN (15,file='coast$d.dat',status='unknown',form='unformatted')
      OPEN (16,file='coast.fmt',  status='old')

      eof=.false.
      do while (.not.eof)

         read  (16,'(3i5/(10i6))')
     &          line%ndx,
     &          line%nrp,
     &          line%nrc,
     &         (line%lat(n),
     &          line%lon(n),n=1,line%nrp)

         write (15)
     &          line%ndx,
     &          line%nrp,
     &          line%nrc,
     &         (line%lat(n),
     &          line%lon(n),n=1,line%nrp)

         if (line%ndx .eq. 9000) go to 19
      end do
19    CLOSE (15)
      CLOSE (16)
      STOP
      END   ! UNFORMAT_COAST