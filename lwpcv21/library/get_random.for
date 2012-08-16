      SUBROUTINE GET_RANDOM
     &          (seed,ranval)

c***********************************************************************
c                         subroutine get_random
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     04 Dec 1996

c  Function:
c     Returns a random number between 0 and 1

c  Parameters passed:
c     seed             [i] the random number seed

c  Parameters returned:
c     ranval           [f] a random number between 0 and 1


c  Common blocks referenced:

c  Functions and subroutines referenced:

c     urand

c     ran


c  References:

c  Change History:

c*******************!***************************************************

      integer       seed
      real          ranval


c     WATCOM specific code

c      ranval=URAND(SEED)
c     WATCOM specific code


c     SUN SOLARIS specific code

c     Likes a large seed!

     ranval=RAN (SEED * 1738)
c     SUN SOLARIS specific code


      RETURN
      END      ! GET_RANDOM