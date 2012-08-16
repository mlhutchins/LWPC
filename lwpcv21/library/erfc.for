      FUNCTION   ERFC
     &          (arg)

c***********************************************************************
c                         function erfc
c***********************************************************************

c  Program Source:   Naval Ocean Systems Center - Code 542

c  Date:
c     03 Jul 1991

c  Function:
c     Returns the complement of the error function using a polynomial
c     approximation to ERF(x) with error < t 1.5e-7

c  Parameters passed:
c     x              [r]   argument to the function

c  Parameters returned:
c     erfc           [r]   complement of the error function

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs
c     exp

c  References:
c     Handbook of Mathematical Functions with Formulas, GRaphs, and
c     Mathematical Tables, Ed. M. Abramowitz and I. A. Stegun, 1970.

c  Change History:

c********************!**************************************************

      data           a1,a2,a3,a4,a5,p/  .254829592,-0.284496736,
     &                                 1.421413741,-1.453152027,
     &                                 1.061405429, 0.3275911/


      x=ABS(arg)
      t=1./(1.+p*x)
      erf=1.-(t*(a1+t*(a2+t*(a3+t*(a4+t*a5)))))*EXP(-x*x)
      if (arg .lt. 0.) erf=-erf
      erfc=1.-erf

      RETURN
      END      ! ERFC