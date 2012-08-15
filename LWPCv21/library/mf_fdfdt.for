      SUBROUTINE MF_FDFDT
     &          (thetaz,f,dfdt)

c***********************************************************************

c     Routine for computing F-function values and their derivatives
c     wrt THETA at arbitrary values of THETA. The unmodified F function
c     would be the unit matrix subtracted form the product of the
c     ionospheric reflection matrix and the ground reflection matrix,
c     R bar. It has been modified by multiplication by C squared and by
c     by the denominator of each of the two non-zero elements of the
c     matrix, R bar. The modified F function thereby has no zero at
c     THETA = 90 and no pole.

c     This routine is called on mainly by the iterative for refinement
c     of MF_FZEROS and MF_FINAL.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      real           omega,wavenr
      complex        num11,num22

      common/mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rbrs/num11,num22,den11,den22,dnum11,dnum22,dden11,
     &               dden22,f1,f2,hg
     &      /mf_rntg/r11,r21,r12,r22,dr11,dr21,dr12,dr22,drdh(8),
     &               roe(8),rnum11,rnum21,rnum12,rnum22,rden


      theta=thetaz
      ideriv=1
      if (kexact .eq. 0) then
         if (noevct .eq. 0) then
            call MF_INTROE
         else
            call MF_LAGRNG
         end if
      else
         call MF_INTEG
         if (nointg .ne. 0) then
            write(lwpcLOG_lun,
     &          '(''MF_FDFDT: RMAG stop'')')
            RETURN
         end if
         call MF_FSINTG
      end if
      call MF_RBARS

      factr1=num11-r11*den11
      factr2=num22-r22*den22
      r12r21=r12*r21
      dn1dn2=den11*den22
      f=factr1*factr2-r12r21*dn1dn2

      dfdnm1=factr2
      dfdnm2=factr1
      dfddn1=-r11*factr2-r12r21*den22
      dfddn2=-r22*factr1-r12r21*den11
      dfdr11=-den11*factr2
      dfdr21=-r12*dn1dn2
      dfdr12=-r21*dn1dn2
      dfdr22=-den22*factr1

      df=dfdr11*dr11+dfdr21*dr21
     &  +dfdr12*dr12+dfdr22*dr22
     &  +dfdnm1*dnum11+dfdnm2*dnum22
     &  +dfddn1*dden11+dfddn2*dden22

      s=SIN(theta*0.01745329252)
      dcdt=-s*0.01745329252
      dfdt=df*dcdt
      RETURN
      END      ! MF_FDFDT