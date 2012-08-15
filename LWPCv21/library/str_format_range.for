      SUBROUTINE STR_FORMAT_RANGE
     &          (minv,maxv,incv,frmt)

c***********************************************************************
c                   subroutine str_format_range
c***********************************************************************
c
c  Program Source:  Naval Ocean Systems Center - Code 542
c
c  Date:
c     28 Dec 1994
c
c  Function:
c     Creates a format specification for listing a range of numbers
c     given the minimum, maximum and increment.  The output string
c     is restricted to '(f99.97)'
c
c  Parameters passed:
c     parameter-name [t,n] description {t is type, n is dimension}
c     minv           [r]   minimum   value
c     maxv           [r]   maximum   value
c     incv           [r]   increment value
c
c  Parameters returned:
c     parameter-name [t,n] description {t is type, n is dimension}
c     frmt           [s]   format specification
c
c  Common blocks referenced:
c
c  Functions and subroutines referenced:
c
c  References:
c
c  Change History:
c
c*******************!***************************************************

      character*(*) frmt
      character* 80 test
      logical       loop
      integer       nd
      integer       nrdecimals
      integer       nrdigits
      real          minv,maxv,incv


c     Number of digits in front of the decimal point
      if (minv .eq. 0.) then
         nrdigits=INT(LOG10(ABS(maxv)))+1
      else
     &if (maxv .eq. 0.) then
         nrdigits=INT(LOG10(ABS(minv)))+1
         if (minv .lt. 0.) nrdigits=nrdigits+1
      else
         nrdigits=INT(MAX(LOG10(ABS(minv)),LOG10(ABS(maxv))))+1
         if (minv .lt. 0.) nrdigits=nrdigits+1
      end if

c     Check if the increment has a fraction
      if (incv-INT(incv) .ne. 0.) then
         loop=.true.
         nd=1
         do while (loop)
            if (nd .lt. 10) then
               write(frmt,
     &             '(''(f'',i2,''.'',i1,'')'')') nrdigits+1+nd,nd
            else
               write(frmt,
     &             '(''(f'',i2,''.'',i2,'')'')') nrdigits+1+nd,nd
            end if
            write(test,frmt) incv
            if (test(nrdigits+1+nd:nrdigits+1+nd) .eq. '0') then
               nd=nd+1
            else
               loop=.false.
            end if
         end do
         loop=.true.
         do while (loop)
            if (nd .lt. 10) then
               write(frmt,
     &             '(''(f'',i2,''.'',i1,'')'')') nrdigits+1+nd,nd
            else
               write(frmt,
     &             '(''(f'',i2,''.'',i2,'')'')') nrdigits+1+nd,nd
            end if
            write(test,frmt) incv
            if (test(nrdigits+1+nd:nrdigits+1+nd) .eq. '0') then
               loop=.false.
            else
               nd=nd+1
            end if
         end do
         nrdecimals=nd-1
      else

c        Output is whole numbers
         nrdecimals=0
      end if

c     Write the format specification
      if (nrdecimals .eq. 0) then
         if (nrdigits .lt. 10) then
            write(frmt,
     &          '(''(f'',i1,''.0)'')') nrdigits+2
         else
            write(frmt,
     &          '(''(f'',i2,''.0)'')') nrdigits+2
         end if
      else
         nrdigits=nrdigits+1+nrdecimals
         if (nrdigits .lt. 10) then
            write(frmt,
     &          '(''(f'',i1,''.'',i1,'')'')') nrdigits,nrdecimals
         else
            if (nrdecimals .lt. 10) then
               write(frmt,
     &             '(''(f'',i2,''.'',i1,'')'')') nrdigits,nrdecimals
            else
               write(frmt,
     &             '(''(f'',i2,''.'',i2,'')'')') nrdigits,nrdecimals
            end if
         end if
      end if
      RETURN
      END      ! STR_FORMAT_RANGE