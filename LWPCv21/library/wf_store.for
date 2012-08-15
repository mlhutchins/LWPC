      SUBROUTINE WF_STORE

c     Stores the solution vectors P.

      implicit real*8 (a-h,o-z)

c     Wave Fields
      real     *  8 p sav,a norm,b norm,ht,wfht1,wfht2
      complex  * 16 m31 sav,m32 sav,m33 sav,ortho

      common/wf_save/
     &              p sav(16),
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              a norm,b norm,ht,wfht1,wfht2,
     &              levl
     &      /wf_petc/
     &              p etc(27,65)


      if (levl .eq. 65) call LWPC_ERROR ('ERROR','[WF_STORE]: levl=65')

      levl=levl+1
      call WF_XFER (p sav,p etc(1,levl),27)
      ortho=0.d0
      a norm=1.d0
      b norm=1.d0
      if (levl .eq. 1) wfht1=ht
      wfht2=ht
      RETURN
      END      ! WF_STORE