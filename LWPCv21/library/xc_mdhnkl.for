      SUBROUTINE XC_MDHNKL
     &          (z,h1,h1e,h2,h2e,dh1,dh1e,dh2,dh2e,theta,idbg)

c***********************************************************************
c                         subroutine xc_mdhnkl
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 Apr 1990

c  Function:
c     Calculates the modified Hankel functions of order 1/3 and their
c     derivatives

c  Parameters passed:
c     z                [c] argument of the functions

c     theta            [c] eigen angle used to compute the argument;
c                          label for diagnostic printout
c     idbg             [i] label for diagnostic printout

c  Parameters returned:
c     h1               [c] H1
c     h2               [c] H2

c     dh1              [c] dH1/dz
c     dh2              [c] dH2/dz

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs
c     exp
c     sqrt

c  References:

c  Change History:

c*******************!***************************************************

      implicit complex*16 (a-h,o-z)

      character*  4 idbg
      character*200 error_msg
      complex  * 16 i,mpower
      real     *  8 h1e,h2e,dh1e,dh2e,
     &              eze,sum4e,
     &              a(30),b(30),c(30),d(30),cap(30),
     &              part1(2),part2(2),zmag,ztlog,tol

      equivalence  (part1,term4),(part2,sum4)

      data      a/9.3043671692922944819d-01,  3.1014557230974314911d+01,
     &            2.0676371487316209897d+02,  5.7434365242545027449d+02,
     &            8.7021765519007617234d+02,  8.2877871922864397320d+02,
     &            5.4168543740434246542d+02,  2.5794544638302022111d+02,
     &            9.3458495066311674231d+01,  2.6626351870744066662d+01,
     &            6.1210004300561072794d+00,  1.1592803844803233472d+00,
     &            1.8401275944132116616d-01,  2.4833030963741048003d-02,
     &            2.8842080097260218300d-03,  2.9133414239656786138d-04,
     &            2.5827494893312753646d-05,  2.0256858739853140063d-06,
     &            1.4155736366074870734d-07,  8.8695090013000443124d-09,
     &            5.0110220346327933889d-10,  2.5658074934115685526d-11,
     &            1.1961806496091228666d-12,  5.0988092481207283185d-14,
     &            1.9948392989517716388d-15,  7.1886100863126905797d-17,
     &            2.3938095525516785112d-18,  7.3883010881224645255d-20,
     &            2.1194208514407528762d-21,  5.6653858632471341093d-23/
      data      b/6.7829872514427588456d-01,  1.1304978752404598033d+01,
     &            5.3833232154307609704d+01,  1.1962940478735024376d+02,
     &            1.5337103177865415841d+02,  1.2780919314887846509d+02,
     &            7.4742218215718400631d+01,  3.2355938621523117060d+01,
     &            1.0785312873841039006d+01,  2.8532573740320209005d+00,
     &            6.1360373635097223595d-01,  1.0937678009821251966d-01,
     &            1.6422939954686564465d-02,  2.1055051223957133911d-03,
     &            2.3316778764072130571d-04,  2.2528288660939256561d-05,
     &            1.9156708045016374595d-06,  1.4446989475879618839d-07,
     &            9.7286124416697769730d-09,  5.8854279743918795891d-10,
     &            3.2160808603234314644d-11,  1.5952782045255116351d-12,
     &            7.2151886229105003778d-14,  2.9876557444763976717d-15,
     &            1.1368553061173507104d-16,  3.9889659863766691603d-18,
     &            1.2946984700995355913d-19,  3.8985199340546088228d-21,
     &            1.0920223904914870636d-22,  2.8527230681595795812d-24/
      data      c/4.6521835846461472410d-01,  6.2029114461948629822d+00,
     &            2.5845464359145262382d+01,  5.2213059311404570392d+01,
     &            6.2158403942148298012d+01,  4.8751689366390821897d+01,
     &            2.7084271870217123228d+01,  1.1215019407957400909d+01,
     &            3.5945575025504490022d+00,  9.1815006450841609147d-01,
     &            1.9128126343925335199d-01,  3.3122296699437809740d-02,
     &            4.8424410379295043444d-03,  6.0568368204246458321d-04,
     &            6.5550182039227768583d-05,  6.1985987743950608612d-06,
     &            5.1654989786625507119d-07,  3.8220488188402150986d-08,
     &            2.5278100653705126277d-09,  1.5033066103898380141d-10,
     &            8.0822936042464409157d-12,  3.9473961437101054471d-13,
     &            1.7590891906016512675d-14,  7.1814214762263778920d-16,
     &            2.6957287823672589641d-17,  9.3358572549515461865d-19,
     &            2.9922619406895981315d-20,  8.9015675760511620701d-22,
     &            2.4644428505125033375d-23,  6.3656020935361057409d-25/
      data      d/6.7829872514427588456d-01,  4.5219915009618392131d+01,
     &            3.7683262508015326776d+02,  1.1962940478735024344d+03,
     &            1.9938234131225040548d+03,  2.0449470903820554375d+03,
     &            1.4201021460986496090d+03,  7.1183064967350857463d+02,
     &            2.6963282184602597492d+02,  7.9891206472896585111d+01,
     &            1.9021715826880139294d+01,  3.7188105233392256682d+00,
     &            6.0764877832340288572d-01,  8.4220204895828535644d-02,
     &            1.0026214868551016149d-02,  1.0363012784032058021d-03,
     &            9.3867869420580235442d-05,  7.5124345274574017960d-06,
     &            5.3507368429183773360d-07,  3.4135482251472901638d-08,
     &            1.9618093247972931935d-09,  1.0209780508963274472d-10,
     &            4.8341763773500352579d-12,  2.0913590211334783723d-13,
     &            8.2990437346566602039d-15,  3.0316141496462685641d-16,
     &            1.0228117913786331176d-17,  3.1967863459247792364d-19,
     &            9.2821903191776400453d-21,  2.5103962999804300309d-22/
      data    cap/1.0416666666666666663d-01,  8.3550347222222222116d-02,
     &            1.2822657455632716019d-01,  2.9184902646414046315d-01,
     &            8.8162726744375764874d-01,  3.3214082818627675264d+00,
     &            1.4995762986862554546d+01,  7.8923013011586517530d+01,
     &            4.7445153886826431887d+02,  3.2074900908906619004d+03,
     &            2.4086549640874004605d+04,  1.9892311916950979121d+05,
     &            1.7919020077753438063d+06,  1.7484377180034121023d+07,
     &            1.8370737967633072978d+08,  2.0679040329451551508d+09,
     &            2.4827519375935888472d+10,  3.1669454981734887315d+11,
     &            4.2771126865134715582d+12,  6.0971132411392560749d+13,
     &            9.1486942234356396792d+14,  1.4413525170009350101d+16,
     &            2.3788844395175757942d+17,  4.1046081600946921885d+18,
     &            7.3900049415704853993d+19,  1.3859220004603943141d+21,
     &            2.7030825930275761623d+22,  5.4747478619645573335d+23,
     &            1.1498937014386333524d+25,  2.5014180692753603969d+26/

      data         i/(0.d0,1.d0)/
     &             root3/(1.73205080756888d0,0.d0)/
     &             alpha/(8.53667218838951d-1,0.d0)/


      zmag=ABS(z)
      if (zmag .le. 6.1d0) then

c        Power series expansion
         if (zmag .le. 1.d-10) then

            sum1=a(1)
            sum2=b(1)
            sum3=c(1)
            sum4=d(1)
         else

            sum1=(0.d0,0.d0)
            sum2=(0.d0,0.d0)
            sum3=(0.d0,0.d0)
            sum4=(0.d0,0.d0)

            zterm=-z**3/(200.d0,0.d0)
            ztlog=LOG(zterm)

            m=1
            last=0
            zpower=(1.d0,0.d0)

            do while (last .eq. 0 .and. m .le. 30)
               sum1 =sum1+a(m)*zpower
               sum2 =sum2+b(m)*zpower
               sum3 =sum3+c(m)*zpower
               sum4 =sum4+d(m)*zpower

               term4=     d(m)*zpower
               if (ABS(part1(1)) .le. 1.d-17*ABS(part2(1)) .and.
     &             ABS(part1(2)) .le. 1.d-17*ABS(part2(2))) last=1

               if (REAL(LOG(zpower))+ztlog .lt. -30.d0) then

                  last=1
               else

                  zpower=zpower*zterm
               end if
               m=m+1
            end do
         end if
         gm2f =i*(sum2*z-(2.d0,0.d0)*sum1     )/root3
         gpmfp=i*(sum4  +(2.d0,0.d0)*sum3*z**2)/root3

         h1 =z*sum2+gm2f
         h2 =h1-(2.d0,0.d0)*gm2f

         dh1=sum4+gpmfp
         dh2=dh1-(2.d0,0.d0)*gpmfp

         h1e =0.0d0
         h2e =0.0d0
         dh1e=0.0d0
         dh2e=0.0d0

         call XC_NORME ( h1, h1e)
         call XC_NORME ( h2, h2e)
         call XC_NORME (dh1,dh1e)
         call XC_NORME (dh2,dh2e)
      else

c        Asymptotic expansion:

c        Functions to use in each quadrant:
c                          |
c           h1a(z), h2b(z) | h1a(z), h2a(z)
c        ------------------+------------------
c           h1b(z), h2a(z) | h1a(z), h2a(z)
c                          |
c        where
c           h1a(z)=a*z**(-1/4)* C1(z)*E(z)
c           h1b(z)=a*z**(-1/4)*[C1(z)*E(z)+C2(z)/E(z)/e]
c        and
c           h2a(z)=a*z**(-1/4)* C2(z)/E(z)
c           h2b(z)=a*z**(-1/4)*[C2(z)/E(z)+C1(z)*E(z)*e]

c        Thus:
c           h1a(z)=a*z**(-1/4)*C1(z)*E(z)
c           h1b(z)=h1a(z)+h2a(z)/e
c        and
c           h2a(z)=a*z**(-1/4)*C2(z)/E(z)
c           h2b(z)=h2a(z)+h1a(z)*e
c        where
c           C1(z)=1+SUM{Cm*[-i/z**(3/2)]**m}
c           C2(z)=1+SUM{Cm*[ i/z**(3/2)]**m}
c           E (z)=EXP[(2/3)*i*z**(3/2)-(5/12)*pi*i]
c           e    =EXP[(4/3)*pi*i]

c        Derivatives:
c           dh1a(z)=a*z**(-1/4)*E(z)
c                  *{[(-1/4)/z+i*z**(1/2)]*C1(z)+dC1(z)}
c           dh2a(z)=a*z**(-1/4)/E(z)
c                  *{[(-1/4)/z-i*z**(1/2)]*C2(z)+dC2(z)}
c        where
c           dC1(z)=(-3/2)*z**(-1)*SUM{m*Cm*[-i/z**(3/2)]**m}
c           dC2(z)=(-3/2)*z**(-1)*SUM{m*Cm*[ i/z**(3/2)]**m}

c        Get the power series
c            C1(z): 1+SUM{  Cm*[-i/z**(3/2)]**m}
c            C2(z): 1+SUM{  Cm*[ i/z**(3/2)]**m}

c           dC1(z):   SUM{m*Cm*[-i/z**(3/2)]**m}
c           dC2(z):   SUM{m*Cm*[ i/z**(3/2)]**m}

         sum1 =(1.d0,0.d0)
         sum2 =(1.d0,0.d0)
         sum3 =(0.d0,0.d0)
         sum4 =(0.d0,0.d0)

         term3=(1.d0,0.d0)

         rtz=SQRT(z)
         zterm=-i/(rtz*z)

         m=1
         last=0
         zpower=(1.d0,0.d0)
         mpower=(1.d0,0.d0)

         do while (last .eq. 0 .and. m .le. 30)

            zpower=zpower*( zterm)
            mpower=mpower*(-zterm)

            term1=cap(m)*zpower
            term2=cap(m)*mpower

            sum1 =sum1+term1
            sum2 =sum2+term2

            sum3 =sum3+term1*m
            sum4 =sum4+term2*m

            term4=     term2*m

            if (ABS(part1(1)) .le. 1.d-17*ABS(part2(1)) .and.
     &          ABS(part1(2)) .le. 1.d-17*ABS(part2(2))) last=1

            if (ABS(term2/term3) .ge. 1.d0) last=1

            term3=term2
            m=m+1
         end do

          C1z=sum1
          C2z=sum2

         dC1z=(-1.5d0,0.d0)/z*sum3
         dC2z=(-1.5d0,0.d0)/z*sum4

c         h1a(z)=a*z**(-1/4)*C1(z)*E(z)
c         h2a(z)=a*z**(-1/4)*C2(z)/E(z)

c        dh1a(z)=a*z**(-1/4)*E(z)*{[(-1/4)/z+i*z**(1/2)]*C1(z)+dC1(z)}
c        dh2a(z)=a*z**(-1/4)/E(z)*{[(-1/4)/z-i*z**(1/2)]*C2(z)+dC2(z)}

c        E(z)=EXP[(2/3)*i*z**(3/2)-(5/12)*pi*i]

         Ez=EXP((0.d0,0.666666666666666667d0)*rtz*z)
     &     *(2.58819045102522d-01,-9.65925826289067d-01)

         Eze=0.d0

         call XC_NORME (Ez,Eze)

          h1 = alpha/SQRT(rtz)*C1z*Ez
          h1e= Eze
          h2 = alpha/SQRT(rtz)*C2z/Ez
          h2e=-Eze

         dh1=  alpha/SQRT(rtz)*Ez*(((-0.25d0,0.d0)/z+i*rtz)*C1z+dC1z)
         dh1e= Eze
         dh2=  alpha/SQRT(rtz)/Ez*(((-0.25d0,0.d0)/z-i*rtz)*C2z+dC2z)
         dh2e=-Eze

         call XC_NORME ( h1, h1e)
         call XC_NORME ( h2, h2e)
         call XC_NORME (dh1,dh1e)
         call XC_NORME (dh2,dh2e)

         term4=z
         if (part1(1) .lt. 0.d0 .and. part1(2) .lt. 0.d0) then

c           h1b(z)=h1a(z)+h2a(z)/e

c            h1= h1+ h2/(-0.5d0,-0.866025403785d0)
c           dh1=dh1+dh2/(-0.5d0,-0.866025403785d0)

            call XC_ADD ( h1, h1e,
     &                    h1, h1e, h2/(-0.5d0,-0.866025403785d0), h2e)

            call XC_ADD (dh1,dh1e,
     &                   dh1,dh1e,dh2/(-0.5d0,-0.866025403785d0),dh2e)

            call XC_NORME ( h1, h1e)
            call XC_NORME (dh1,dh1e)
         end if
         if (part1(1) .lt. 0.d0 .and. part1(2) .ge. 0.d0) then

c           h2b(z)=h2a(z)+h1a(z)*e

c            h2= h2+ h1*(-0.5d0,-0.866025403785d0)
c           dh2=dh2+dh1*(-0.5d0,-0.866025403785d0)

            call XC_ADD ( h2, h2e,
     &                    h2, h2e, h1*(-0.5d0,-0.866025403785d0), h1e)

            call XC_ADD (dh2,dh2e,
     &                   dh2,dh2e,dh1*(-0.5d0,-0.866025403785d0),dh1e)

            call XC_NORME ( h2, h2e)
            call XC_NORME (dh2,dh2e)
         end if
      end if

c     Calculate Wronskian as partial check on validity;
c     this test is not valid when h1, h2, h1', h2'
c     are of the same order of magnitude.
      call XC_ADD (sum4,sum4e,h1*dh2,h1e+dh2e,-dh1*h2,dh1e+h2e)
      sum4=sum4*DEXP(sum4e)
      tol=1.d-8
      tol=1000.
      if (ABS(part2(1))                     .gt. tol .or.
     &    ABS(part2(2)+1.457495441040461d0) .gt. tol) then
         write(error_msg,
     &       '(''[XC_MDHNKL]: '',
     &         ''W='',1p2e15.6,'' for theta='',
     &           0p2f10.4,'' at '',a4)') sum4,theta,idbg
         call LWPC_ERROR ('WARNING',error_msg)
      end if

      RETURN
      END      ! XC_MDHNKL