      SUBROUTINE WF_MA2
     &          (z1,z2,z3,out)

      real     *  8 out(6)
      complex  * 16 z1,z2,z3,temp


      temp=z1
      if (ABS(temp) .eq. 0.d0) then
         out(1)=-99.
         out(2)=0.d0
      else
         temp=LOG(temp)
         out(1)=EXP(DREAL(temp))
         out(2)=DIMAG(temp)
      end if
      temp=z2
      if (ABS(temp) .eq. 0.d0) then
         out(3)=-99.
         out(4)=0.d0
      else
         temp=LOG(temp)
         out(3)=EXP(DREAL(temp))
         out(4)=DIMAG(temp)
      end if
      temp=z3
      if (ABS(temp) .eq. 0.d0) then
         out(5)=-99.
         out(6)=0.d0
      else
         temp=LOG(temp)
         out(5)=EXP(DREAL(temp))
         out(6)=DIMAG(temp)
      end if
      RETURN
      END      ! WF_MA2