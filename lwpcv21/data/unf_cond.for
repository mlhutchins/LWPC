      PROGRAM UNFORMAT_COND$F

c     reads    formatted file: cond$f.fmt
c     writes unformatted file: cond$f.dat

      parameter    (mxcoord=4000)

      integer  *  2 cond_lat(mxcoord)
      integer  *  2 cond_lon(mxcoord)
      integer  *  2 ndx
      integer  *  2 nrp

      logical       eof


      OPEN (15,file='cond$f.dat',status='unknown',form='unformatted')
      OPEN (16,file='cond$f.fmt',status='old')

      eof=.false.
      do while (.not.eof)

         read (16,'(2i5/(12i6))',end=19)
     &         ndx,nrp,
     &        (cond_lon(n),
     &         cond_lat(n),n=1,nrp)

         write(15)
     &         ndx,nrp,
     &        (cond_lon(n),
     &         cond_lat(n),n=1,nrp)
      end do
19    CLOSE (15)
      CLOSE (16)
      STOP
      END   ! UNFORMAT_COND$F