      FUNCTION   PROB_TO_SIGMA (plvl)

c***********************************************************************
c                         function   prob_to_sigma
c***********************************************************************

c  Program Source:   Naval Ocean Systems Center - Code 542

c  Date:
c     12 Feb 1991

c  Function:
c     Determines the number of standard deviations to use for specific
c     probabilities

c  Parameters passed:
c     plvl           [r,1] probability level expressed as percentage

c  Parameters returned:
c     prob_to_sigma  [r,1] number of standard deviations from the mean
c                          corresponding to the probability

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs

c     erfc

c  References:

c  Change History:
c     22 Jan 1992    Reduced tolerance for terminating the iterations.

c     50 Jun 1995    Added tests for 0% and 100%.

c********************!**************************************************


      if (     plvl .eq.   0.) then
         t1=-100.
      else
     &if (plvl .eq.  50.) then
         t1=0.
      else
     &if (plvl .eq.  84.) then
         t1=1.
      else
     &if (plvl .eq.  90.) then
         t1=1.28
      else
     &if (plvl .eq.  99.) then
         t1=2.33
      else
     &if (plvl .eq. 100.) then
         t1=100.
      else
         t2=1.28
         f2=90.
         t1=1.
         f1=84.
Change:              This used to be .1
         do while (ABS(plvl-f1) .gt. .001)
            t=(t2-t1)*(plvl-f1)/(f2-f1)+t1
            t2=t1
            f2=f1
            t1=t
            f1=50.*(2.-ERFC(t1*.707107))
         end do
      end if
      prob_to_sigma=t1

      RETURN
      END      ! PROB_TO_SIGMA