      PROGRAM UNFORMAT_NTIA

c     copies coefficients
c     from   formatted ntia.fmt
c     to   unformatted ntia$d.dat

      dimension p(29,16,6),abp(2,6),dud(5,12,5),fam(14,12)


      OPEN (10,file='ntia.fmt',
     &         status='old')
      OPEN (11,file='ntia$d.dat',
     &         status='unknown',form='unformatted')

      do 14 j=1,4
      read (10,'(1p5e15.7)') abp,p,dud,fam
14    write(11)              abp,p,dud,fam

      STOP
      END   ! UNFORMAT_NTIA