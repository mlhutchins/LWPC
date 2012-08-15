      PROGRAM UNFORMAT_ITSN

c     copies coefficients
c     from   formatted itsn.fmt
c     to   unformatted itsn$d.dat

      dimension grid(1518)


      OPEN (8,file='itsn.fmt',
     &        status='old')
      OPEN (9,file='itsn$d.dat',
     &        status='unknown',form='unformatted')

      do 14 ii=1,12
      read  (8,'(1p5e16.8)') grid
14    write (9)              grid

      STOP
      END   ! UNFORMAT_ITSN