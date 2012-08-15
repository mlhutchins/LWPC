      SUBROUTINE WF_LOAD

c     Retrieves the solution vectors P.

      implicit real*8 (a-h,o-z)

      real     *  8 p sav,a norm,b norm,ht,wfht1,wfht2
      complex  * 16 m31 sav,m32 sav,m33 sav,ortho

      common/wf_save/
     &              p sav(16),
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              a norm,b norm,ht,wfht1,wfht2,
     &              levl
     &      /wf_petc/
     &              p etc(27,65)


      if (levl .eq. 0) call LWPC_ERROR ('ERROR','[WF_LOAD]: levl=0')

      call WF_XFER (p etc(1,levl),p sav,27)
      levl=levl-1
      RETURN
      END      ! WF_LOAD