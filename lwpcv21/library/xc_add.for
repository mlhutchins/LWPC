      SUBROUTINE XC_ADD
     &          (za,ze,z1a,z1e,z2a,z2e)

c***********************************************************************
c                         subroutine xc_add
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 Apr 1990

c  Function:
c     Adds two extended complex numbers:
c         z1=z1a*exp(z1e) and z2=z2a*exp(z2e)
c     to form the extended complex number:
c         z=za*exp(ze)

c  Parameters passed:
c     z1a              [c] complex amplitude of z1
c     z1e              [r] real exponent     of z1
c     z2a              [c] complex amplitude of z2
c     z2e              [r] real exponent     of z2

c  Parameters returned:
c     za               [c] complex amplitude of z
c     ze               [r] real exponent     of z

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     idint

c     xc_norme

c  References:

c  Change History:

c*******************!***************************************************

      implicit real*8 (a-h,o-z)

      parameter    (jmax=88,djmax=88.0d0)

      complex*   16 za,z1a,z2a,tz1a,tz2a

c     EM is an array containing values of exp(-j) for j=0 to jmax

c     NOTE: exp(-jmax) is the largest integer power of exp(-1.0)
c           that does not underflow

c     NOTE: djmax=float(jmax)

      dimension     em(0:jmax)

      data em/
     &  .100000000000000d+01,.367879441171442d+00,.135335283236613d+00,
     &  .497870683678639d-01,.183156388887342d-01,.673794699908547d-02,
     &  .247875217666636d-02,.911881965554516d-03,.335462627902512d-03,
     &  .123409804086680d-03,.453999297624849d-04,.167017007902457d-04,
     &  .614421235332821d-05,.226032940698105d-05,.831528719103568d-06,
     &  .305902320501826d-06,.112535174719259d-06,.413993771878517d-07,
     &  .152299797447126d-07,.560279643753727d-08,.206115362243856d-08,
     &  .758256042791191d-09,.278946809286892d-09,.102618796317019d-09,
     &  .377513454427910d-10,.138879438649640d-10,.510908902806332d-11,
     &  .187952881653908d-11,.691440010694020d-12,.254366564737692d-12,
     &  .935762296884017d-13,.344247710846998d-13,.126641655490942d-13,
     &  .465888614510340d-14,.171390843154201d-14,.630511676014699d-15,
     &  .231952283024357d-15,.853304762574407d-16,.313913279204803d-16,
     &  .115482241730158d-16,.424835425529159d-17,.156288218933499d-17,
     &  .574952226429356d-18,.211513103759108d-18,.778113224113380d-19,
     &  .286251858054939d-19,.105306173575538d-19,.387399762868719d-20,
     &  .142516408274094d-20,.524288566336346d-21,.192874984796392d-21,
     &  .709547416228470d-22,.261027906966770d-22,.960268005450868d-23,
     &  .353262857220081d-23,.129958142500750d-23,.478089288388547d-24,
     &  .175879220242431d-24,.647023492564546d-25,.238026640869440d-25,
     &  .875651076269652d-26,.322134028599252d-26,.118506486423398d-26,
     &  .435961000006308d-27,.160381089054864d-27,.590009054159706d-28,
     &  .217052201130364d-28,.798490424568698d-29,.293748211171080d-29,
     &  .108063927770728d-29,.397544973590865d-30,.146248622725123d-30,
     &  .538018616002114d-31,.197925987794690d-31,.728129017832164d-32,
     &  .267863696180808d-32,.985415468611126d-33,.362514091914356d-33,
     &  .133361481550226d-33,.490609473064928d-34,.180485138784542d-34,
     &  .663967719958073d-35,.244260073774053d-35,.898582594404938d-36,
     &  .330570062676073d-36,.121609929925283d-36,.447377930618112d-37,
     &  .164581143108227d-37,.605460189540119d-38/

      tz1a=z1a
      tz1e=z1e
      tz2a=z2a
      tz2e=z2e
      call XC_NORME (tz1a,tz1e)
      call XC_NORME (tz2a,tz2e)

c     add z1 and z2

      if (tz1e .ge. tz2e) then
         difze=tz1e-tz2e
         if (difze .ge. djmax) then
            za=tz1a
            ze=tz1e
         else
            ival=IDINT(difze)
            za=tz1a+tz2a*em(ival)
            ze=tz1e
         end if
      else
         difze=tz2e-tz1e
         if (difze .ge. djmax) then
            za=tz2a
            ze=tz2e
         else
            ival=IDINT(difze)
            za=tz2a+tz1a*em(ival)
            ze=tz2e
         end if
      end if

      RETURN
      END      ! XC-ADD