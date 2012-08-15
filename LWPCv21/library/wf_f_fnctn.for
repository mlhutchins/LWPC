      FUNCTION   WF_F_FNCTN
     &          (index)

c     Computes the value of the modal equation.

      complex  * 16 wf_f_fnctn,
     &              r11,r22,r12,r21,rbar11,rbar22

      common/wf_rmtx/
     &              r11,r22,r12,r21,rbar11,rbar22


c     Get reflection coefficients
      call WF_R_MTRX (index)
      call WF_RBARS  (index)

c     Compute modal eqn. value
      wf_f_fnctn=(1.d0-r11*rbar11)*(1.d0-r22*rbar22)
     &          -r12*r21*rbar11*rbar22

      RETURN
      END      ! WF_F_FNCTN