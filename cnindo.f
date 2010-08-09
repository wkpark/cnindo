C     For more informations about procedures and meaning of this code, please read
C     a book pointed in readme.
      BLOCK DATA
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ORB/ORB(9)
      COMMON/PERTBL/EL(18)
C     COMMON/OPTION/OPTION,OPNCLO,HUCKCL,CNDO,INDO,CLOSED,OPEN
C     INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
C     INTEGER ORB,EL
      DATA ORB/1HS,2HPX,2HPY,2HPZ,3HDZ2,3HDXZ,3HDYZ,4HDX-Y,3HDXY/
      DATA EL(1)/1HH/
      DATA EL(2)/2HHe/
      DATA EL(3)/2HLi/
      DATA EL(4)/2HBe/
      DATA EL(5)/1HB/
      DATA EL(6)/1HC/
      DATA EL(7)/1HN/
      DATA EL(8)/1HO/
      DATA EL(9)/1HF/
      DATA EL(10)/2HNe/
      DATA EL(11)/2HNa/
      DATA EL(12)/2HMg/
      DATA EL(13)/2HAl/
      DATA EL(14)/2HSi/
      DATA EL(15)/1HP/
      DATA EL(16)/1HS/
      DATA EL(17)/2HCl/
      DATA EL(18)/2HAr/
      END


      PROGRAM CNINDO
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ARRAYS/ABC(19200)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/PERTBL/EL(18)
      COMMON/ORB/ORB(9)
      COMMON/QAB/XYZ(2000)
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/OPTION/OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      COMMON/AUXINT/A(17),B(17)
      INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER AN,CHARGE,CZ,U,ULIM,OCCA,OCCB
C     NEW VARIABLES
C     MAX LENGHT OF FILE NAME IS 61
      INTEGER WYB
      CHARACTER*61 NAME
      CHARACTER*6  TEMP
      CHARACTER*21 TEMP2
      CHARACTER*20 COMMENT
      CHARACTER*6  ENDMOL
      CHARACTER*2  SIM
      CHARACTER*100 LINE

C     NEW MODIFICTION IN ORIGINAL CODE PSEDUDO-CONSTANS VALUES WERREN'T DEFINED
      OPEN = 0
      CLOSED = 1
      CNDO = 0
      INDO = 1

C     YOU CAN CHOOSE AN ORIGINAL OR EXTENDED OUTPUT REMOVING AND ADDIN C
c     INCLUDE "STDINPUT.FOR"
      INCLUDE "STD&HYPCHEMINPUT.FOR"

      IF (OPTION.EQ.CNDO)GO TO 6
      DO 5 I = 1,NATOMS
      IF (AN(I).LE.9) GO TO 4
      WRITE(6,3)
3     FORMAT(5X,46HTHIS PROGRAM DOES NOT DO INDO CALCULATIONS FOR,
     1  51H MOLECULES CONTAINING ELEMENTS HIGHER THAN FLUORINE)
      READ(*,*)
      STOP
4     CONTINUE
5     CONTINUE
6     CONTINUE
      CALL  COEFFT
      CALL  INTGRL
      IF   (OPNCLO.EQ.0) GO TO 90
      CALL HUCKCL
      CALL SCFCLO
      CALL CPRINT
      GO TO 100
90    CALL HUCKOP
      CALL SCFOPN
      CALL OPRINT
100   CONTINUE
60    FORMAT(/5X,I4,17H ATOMS CHARGE  = ,I4,18H  MULTIPLICITY  = ,I4/)
70    FORMAT(I4,3(3X,F12.7))
280   FORMAT(/)
      READ(*,*)
      CALL EXIT
      STOP
      END

      SUBROUTINE COEFFT
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ARRAYS/S(80,80),Y(9135),Z(765),XX(2900)
      DO 1 I = 1,9135
1     Y(I) = 0.0D0
      DO 2 I = 1,765
2     Z(I) = 0.0D0
C     LOAP NON-ZERO Y COEFFICIENTS
      Y(7039)=      64.D0
      Y(7040)=      64.D0
      Y(7049)=     -64.D0
      Y(7032)=    -128.D0
      Y(7041)=     -64.D0
      Y(7033)=    -128.D0
      Y(7042)=     128.D0
      Y(7025)=      64.D0
      Y(7034)=     128.D0
      Y(7026)=      64.D0
      Y(7035)=     -64.D0
      Y(7027)=     -64.D0
      Y(6904)=     -96.D0
      Y(6913)=      32.D0
      Y(6896)=    -192.D0
      Y(6905)=     192.D0
      Y(6906)=     288.D0
      Y(6915)=     -96.D0
      Y(6889)=     192.D0
      Y(6907)=    -192.D0
      Y(6890)=      96.D0
      Y(6899)=    -288.D0
      Y(6891)=    -192.D0
      Y(6900)=     192.D0
      Y(6892)=    -32.D0
      Y(6901)=     96.D0
      Y(2854)=    -16.D0
      Y(2863)=     16.D0
      Y(2847)=     32.D0
      Y(2856)=    -16.D0
      Y(2865)=    -16.D0
      Y(2840)=    -16.D0
      Y(2849)=    -16.D0
      Y(2858)=     32.D0
      Y(2842)=     16.D0
      Y(2851)=    -16.D0
      Y(2710)=     48.D0
      Y(2719)=    -48.D0
      Y(2711)=     48.D0
      Y(2720)=     -96.D0
      Y(2729)=      48.D0
      Y(2703)=     -48.D0
      Y(2712)=     -48.D0
      Y(2721)=      96.D0
      Y(2704)=     -48.D0
      Y(2713)=      48.D0
      Y(2722)=      48.D0
      Y(2731)=     -48.D0
      Y(2705)=      96.D0
      Y(2714)=     -48.D0
      Y(2723)=     -48.D0
      Y(2706)=      48.D0
      Y(2715)=     -96.D0
      Y(2724)=      48.D0
      Y(2707)=     -48.D0
      Y(2716)=      48.D0
      Y(5329)=      64.D0
      Y(5322)=    -128.D0
      Y(5340)=     -64.D0
      Y(5315)=      64.D0
      Y(5333)=     128.D0
      Y(5326)=     -64.D0
      Y(5185)=     -96.D0
      Y(5194)=      32.D0
      Y(5186)=     -96.D0
      Y(5195)=      64.D0
      Y(5204)=      32.D0
      Y(5178)=      96.D0
      Y(5187)=      32.D0
      Y(5196)=      64.D0
      Y(5179)=      96.D0
      Y(5188)=     -32.D0
      Y(5197)=      32.D0
      Y(5206)=     -96.D0
      Y(5180)=     -64.D0
      Y(5189)=     -32.D0
      Y(5198)=     -96.D0
      Y(5181)=     -32.D0
      Y(5190)=     -64.D0
      Y(5199)=      96.D0
      Y(5182)=     -32.D0
      Y(5191)=      96.D0
      Y(4375)=    -144.D0
      Y(4384)=      96.D0
      Y(4393)=     -16.D0
      Y(4368)=     144.D0
      Y(4386)=     -48.D0
      Y(4395)=      96.D0
      Y(4370)=     -96.D0
      Y(4379)=      48.D0
      Y(4397)=    -144.D0
      Y(4372)=      16.D0
      Y(4381)=     -96.D0
      Y(4390)=     144.D0
      Y(1900)=     144.D0
      Y(1909)=    -144.D0
      Y(1893)=    -144.D0
      Y(1920)=     144.D0
      Y(1895)=     144.D0
      Y(1922)=    -144.D0
      Y(1906)=    -144.D0
      Y(1915)=     144.D0
      Y(955)=      -16.D0
      Y(964)=       32.D0
      Y(973)=      -16.D0
      Y(948)=       16.D0
      Y(966)=      -48.D0
      Y(975)=       32.D0
      Y(950)=      -32.D0
      Y(959)=       48.D0
      Y(977)=      -16.D0
      Y(952)=       16.D0
      Y(961)=      -32.D0
      Y(970)=       16.D0
      Y(8155)=      64.D0
      Y(8156)=     -64.D0
      Y(8165)=     -64.D0
      Y(8148)=     -64.D0
      Y(8157)=      64.D0
      Y(8149)=      64.D0
      Y(8158)=      64.D0
      Y(8150)=     -64.D0
      Y(8020)=     -96.D0
      Y(8029)=      32.D0
      Y(8021)=     128.D0
      Y(8013)=      96.D0
      Y(8031)=     -96.D0
      Y(8014)=    -128.D0
      Y(8015)=     -32.D0
      Y(8024)=      96.D0
      Y(7084)=     -64.D0
      Y(7076)=    -128.D0
      Y(7085)=      64.D0
      Y(7086)=     128.D0
      Y(7069)=     128.D0
      Y(7070)=      64.D0
      Y(7079)=    -128.D0
      Y(7071)=     -64.D0
      Y(3205)=     -16.D0
      Y(3214)=      16.D0
      Y(3206)=      16.D0
      Y(3215)=     -16.D0
      Y(3198)=      16.D0
      Y(3216)=     -16.D0
      Y(3199)=     -16.D0
      Y(3217)=      16.D0
      Y(3200)=     -16.D0
      Y(3209)=      16.D0
      Y(3201)=      16.D0
      Y(3210)=     -16.D0
      Y(7579)=      64.D0
      Y(7580)=     -64.D0
      Y(7572)=    -128.D0
      Y(7573)=     128.D0
      Y(7565)=      64.D0
      Y(7566)=     -64.D0
      Y(5680)=      64.D0
      Y(5681)=     -64.D0
      Y(5673)=     -64.D0
      Y(5691)=     -64.D0
      Y(5674)=      64.D0
      Y(5692)=      64.D0
      Y(5684)=      64.D0
      Y(5685)=     -64.D0
      Y(7435)=     -96.D0
      Y(7444)=      32.D0
      Y(7436)=     -96.D0
      Y(7445)=     160.D0
      Y(7428)=      96.D0
      Y(7437)=     128.D0
      Y(7446)=     -96.D0
      Y(7429)=      96.D0
      Y(7438)=    -128.D0
      Y(7447)=     -96.D0
      Y(7430)=    -160.D0
      Y(7439)=      96.D0
      Y(7431)=     -32.D0
      Y(7440)=      96.D0
      Y(5545)=     -96.D0
      Y(5554)=      32.D0
      Y(5546)=      32.D0
      Y(5555)=      32.D0
      Y(5538)=      96.D0
      Y(5556)=      32.D0
      Y(5539)=     -32.D0
      Y(5557)=     -96.D0
      Y(5540)=     -32.D0
      Y(5549)=     -32.D0
      Y(5541)=     -32.D0
      Y(5550)=      96.D0
      Y(3070)=      48.D0
      Y(3079)=     -48.D0
      Y(3071)=     -48.D0
      Y(3080)=      48.D0
      Y(3063)=     -48.D0
      Y(3081)=      48.D0
      Y(3064)=      48.D0
      Y(3082)=     -48.D0
      Y(3065)=      48.D0
      Y(3074)=     -48.D0
      Y(3066)=     -48.D0
      Y(3075)=      48.D0
      Y(8200)=     -64.D0
      Y(8201)=      64.D0
      Y(8193)=      64.D0
      Y(8194)=     -64.D0
      Y(7615)=     -64.D0
      Y(7616)=     -64.D0
      Y(7625)=      64.D0
      Y(7608)=      64.D0
      Y(7617)=      64.D0
      Y(7609)=      64.D0
      Y(7618)=     -64.D0
      Y(7610)=     -64.D0
      Y(3250)=      16.D0
      Y(3259)=     -16.D0
      Y(3243)=     -16.D0
      Y(3261)=      16.D0
      Y(3245)=      16.D0
      Y(3254)=     -16.D0
      Y(5725)=     -64.D0
      Y(5718)=      64.D0
      Y(5736)=      64.D0
      Y(5729)=     -64.D0
C     LOAD NON-ZERO Z COEFFICIENTS
      Z(341)=             -1.D0
      Z(343)=              3.D0
      Z(345)=             -3.D0
      Z(347)=              1.D0
      Z(664)=             -1.D0
      Z(665)=              5.D0
      Z(666)=            -10.D0
      Z(667)=             10.D0
      Z(668)=             -5.D0
      Z(669)=              1.D0
      Z(154)=             -1.D0
      Z(156)=              5.D0
      Z(158)=            -10.D0
      Z(160)=             10.D0
      Z(162)=             -5.D0
      Z(164)=              1.D0
      Z(222)=             -1.D0
      Z(223)=              1.D0
      Z(224)=              4.D0
      Z(225)=              -4.D0
      Z(226)=             -6.D0
      Z(227)=              6.D0
      Z(228)=              4.D0
      Z(229)=             -4.D0
      Z(230)=             -1.D0
      Z(231)=              1.D0
      Z(307)=             -1.D0
      Z(308)=              2.D0
      Z(309)=              2.D0
      Z(310)=             -6.D0
      Z(312)=              6.D0
      Z(313)=             -2.D0
      Z(314)=             -2.D0
      Z(315)=              1.D0
      Z(409)=             -1.D0
      Z(410)=              3.D0
      Z(411)=             -1.D0
      Z(412)=             -5.D0
      Z(413)=              5.D0
      Z(414)=              1.D0
      Z(415)=             -3.D0
      Z(416)=              1.D0
      Z(528)=             -1.D0
      Z(529)=              4.D0
      Z(530)=             -5.D0
      Z(532)=              5.D0
      Z(533)=             -4.D0
      Z(534)=              1.D0
      Z(562)=             -1.D0
      Z(563)=              2.D0
      Z(565)=             -2.D0
      Z(566)=              1.D0
      Z(732)=             -1.D0
      Z(733)=              1.D0
      Z(545)=              1.D0
      Z(546)=             -3.D0
      Z(547)=              2.D0
      Z(548)=              2.D0
      Z(549)=             -3.D0
      Z(550)=              1.D0
      Z(579)=              1.D0
      Z(580)=             -1.D0
      Z(581)=             -1.D0
      Z(582)=              1.D0
      Z(596)=             -1.D0
      Z(598)=              1.D0
      Z(443)=             -1.D0
      Z(444)=              1.D0
      Z(445)=              2.D0
      Z(446)=             -2.D0
      Z(447)=             -1.D0
      Z(448)=              1.D0
      Z(698)=             -1.D0
      Z(699)=              3.D0
      Z(700)=             -3.D0
      Z(701)=              1.D0
      Z(324)=              1.D0
      Z(325)=             -1.D0
      Z(326)=             -3.D0
      Z(327)=              3.D0
      Z(328)=              3.D0
      Z(329)=             -3.D0
      Z(330)=             -1.D0
      Z(331)=              1.D0
      Z(460)=              1.D0
      Z(462)=             -2.D0
      Z(464)=              1.D0
      RETURN
      END

      SUBROUTINE   INTGRL
      IMPLICIT REAL*8 (A-H,O-Z)
C     ATOMIC INTEGRALS FOR CNDO CALCULATIONS
      COMMON/ARRAYS/S(80,80),Y(9,5,203),Z(17,45),XX(2900)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/GAB/XXX(400),GAMMA(35,35),T(9,9),PAIRS(9,9),TEMP(9,9)
     1 ,C1(3),C2(3),YYY(126)
      COMMON/AUXINT/A(17),B(17)
      COMMON/OPTION/OPTION,OPENCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      DIMENSION MU(18),NC(18),LC(9),MC(9),E(3)
      DIMENSION P(80,80)
C     Oryginalnie by�o EQUIVALENCE(P(1),Y(1))
      EQUIVALENCE (P(1,1),Y(1,1,1))
      REAL*8  MU,NUM,K1,K2
      INTEGER AN,ULIM,ULK,ULL,U,CHARGE,ANL,ANK,OCCA,OCCB,CZ
      INTEGER OPTION,HUCKEL,CNDO,INDO,CLOSED,OPEN,OPENCLO
C     DETFRMINATION OF SIZE OF lO  BASIS IN ANDCORE CHARGE CZ
      N = 0
      DO 60 I = 1,NATOMS
      LLIM(I) = N+1
      K = 1
      IF(AN(I).LT.11) GO TO 20
10    N = N+9
      CZ(I) = AN(I)-10
      GO TO 50
20    IF(AN(I).LT.3) GO TO 40
30    N = N+4
      CZ(I) = AN(I)-2
      GO TO 50
40    N = N+1
      CZ(I) = AN(I)
50    CONTINUE
      ULIM(I) = N
60    CONTINUE
C     FILL L ARRAY--=U(J) IDENTIFIES THE ATOM T0 WHICH ORBITIL J IS
C     ATTAChED E.G. ORBITAL 32 ATTACHED T0 ATOM 7,   ETC.
      DO 70 K = 1, NATOMS
      LLK = LLIM(K)
      ULK = ULIM(K)
      LIM = ULK+1-LLK
      DO 70 I = 1,LIM
      J = LLK+I-1
70    U(J) = K
C     ASSIGMENT OF ORBITAL EXPONENTS TO ATOMS BY SLATERS RULES
      MU(2) = 1.7D0
      MU(1) = 1.2D0
      NC(1) = 1
      NC(2) = 1
      DO 80 I = 3,10
      NC(I) = 2
80    MU(I) =0.325D0 * DFLOAT(I-1)
      DO 90 I = 11,18
      NC(I) = 3
90    MU(I) = (0.65D0 * DFLOAT(I) - 4.95D0)/3.D0
C     ASSIGMENT OF ANGULAR MOMENTUM OUANTUM  NOS. TO  ATOMIC  ORBITALS
      LC(1) = 0
      LC(2) = 1
      LC(3) = 1
      LC(4) = 1
      LC(5) = 2
      LC(6) = 2
      LC(7) = 2
      LC(8) = 2
      LC(9) = 2
      MC(1) = 0
      MC(2) = 1
      MC(3) =-1
      MC(4) = 0
      MC(5) = 0
      MC(6) = 1
      MC(7) =-1
      MC(8) = 2
      MC(9) =-2
C     STEP THRU PAIRS OF ATOHS
      DO 320 K = 1,NATOMS
      DO 320 L = K,NATOMS
      DO 100 I = 1,3
      C1(I) = C(K,I)
100   C2(I) = C(L,I)
C     CALCULATE UNIT VECTOR ALONG INTERATOM AXIS,E
      CALL RELVEC(R,E,C1,C2)
      LLK = LLIM(K)
      LLL = LLIM(L)
      ULK = ULIM(K)
      ULL = ULIM(L)
      NORBK = ULK - LLK+1
      NORBL = ULL - LLL+1
      ANK= AN(K)
      ANL= AN(L)
C     LOOP THRU PAIRS OF BASIS FUNCTIONS, ONE ON EACH ATOP
      DO 200 I = 1,NORBK
      DO 200 J = 1,NORBL
      IF (K.EQ.L) GO TO 160
110   IF(MC(I).NE.MC(J)) GO TO 150
120   IF(MC(I).LT.0) GO TO 140
130   PAIRS(I,J)=DSQRT((MU(ANK)*R)**(2*NC(ANK)+1)*(MU(ANL)*R)**(2*NC(ANL
     1)+1)/(FACT(2*NC(ANK))*FACT(2*NC(ANL))))*(-1.D0)**(LC(J)+MC(J))
     2*SS(NC(ANK),LC(I),MC(I),NC(ANL),LC(J),MU(ANK)*R,MU(ANL)*R)
      GO TO 190
140   PAIRS(I,J) = PAIRS(I-1,J-1)
      GO TO 190
150   PAIRS(I,J) = 0.0D0
      GO TO 190
160   IF (I.EQ.J) GO TO 170
180   PAIRS(I,J) = 0.0D0
      GO TO 190
170   PAIRS(I,J) = 1.0D0
190   CONTINUE
200   CONTINUE
      LCULK = LC(NORBK)
      LCULL = LC(NORBL)
      MAXL = MAX0(LCULK,LCULL)
      IF(R.GT.0.000001D0) GO TO 220
210   GO TO 250
C     ROTATF INTEGRALS FROH DIATOMIC BASIS TO MOLECULAR BASIS
220   CALL HARMTR(T,MAXL,E)
      DO 230 I = 1,NORBK
      DO 230 J = 1,NORBL
      TEMP(I,J) = 0.0D0
      DO 230 KK = 1,NORBL
      TEMP(I,J) = TEMP(I,J)+T(J,KK)*PAIRS(I,KK)
230   CONTINUE
      DO   240   I = 1,NORBK
      DO   240   J = 1,NORBL
      PAIRS(I,J) = 0.0D0
      DO   240   KK=1,NORBK
      PAIRS(I,J) = PAIRS(I,J) + T(I,KK) * TEMP(KK,J)
240   CONTINUE
C     FILL S MATRIX
250   CONTINUE
      DO   260 I = 1,NORBK
      LLKP = LLK + I - 1
      DO   260 J = 1,NORBL
      LLLP = LLL+J-1
260   S(LLKP,LLLP) = PAIRS(I,J)
C     COMPUTATION or 1-CENIER COULOMB INTEGRALS OVER SLATER S FUNCTIONS
      N1 = NC(ANK)
      N2 = NC(ANL)
      K1 = MU(ANK)
      K2 = MU(ANL)
      IF(K.NE.L) GO TO 290
270   TERM1 = FACT(2*N1-1)/((2.D0*K2)**(2*N1))
      TERM2 = 0.D0
      LIM = 2 * N1
      DO 280 J = 1,LIM
      NUM = DFLOAT(J) * (2.D0 *K1)** (2*N1-J)* FACT(4*N1-J-1)
      DEN = FACT(2*N1-J) * 2.D0*DFLOAT(N1) * (2.D0*(K1+K2))**(4*N1-J)
      TERM2 = TERM2 + NUM/DEN
280   CONTINUE
      GO TO 310
C     COMPUTATION OF 2-CENTER COULOMB INTEGRALS OVER SLATER S FUNCTIOMS
290   TERM1 = (R/2.D0)**(2*N2)*SS(0,0,0,2*N2-1,0,0.D0,2.D0*K2*R)
      TERM2 = 0.D0
      LIM = 2 * N1
      DO 300 J=1,LIM
300   TERM2 = TERM2 +(DFLOAT(J)*(2.D0*K1)**(2*N1-J)*(R/2.D0)**(2*
     1N1-J+2*N2))/(FACT(2*N1-J)*2.D0*DFLOAT(N1))*SS(2*N1-J,0,0,2*N2-1,0
     2,2.D0*K1*R,2.D0*K2*R)
310   GAMMA(K,L) = ((2.D0*K2)**(2*N2+1)/FACT(2*N2)) * (TERM1-TERM2)
320   CONTINUE
C     SYMEETTRIZATION OF OVERLAP AND COULOMB INTEGRAL MATRICES
      DO 330 I=1,N
      DO 330 J=I,N
330   S(J,I) = S(I,J)
      DO 340 I=1,NATOMS
      DO 340 J=I,NATOMS
340   GAMMA(J,I) = GAMMA(I,J)
      WRITE(6,350)
      WRITE(2,350)
350   FORMAT(1X,24H OVERLAP INTEGRAL MATRIX)
      CALL MATOUT(N,1)
C     TRANSFER GAMMA T0 80X80 MATRIX P FOR PRINTING
      DO 360 I=1,NATOMS
      DO 360 J=1,NATOMS
360   P(I,J) = GAMMA(I,J)
      WRITE(6,370)
      WRITE(2,370)
370   FORMAT(1X,24H COULOMB INTEGRAL MATRIX)
      CALL MATOUT(NATOMS,2)
      RETURN
      END

      FUNCTION SS(NN1,LL1,MM,NN2,LL2,ALPHA,BETA)
      IMPLICIT REAL*8(A-H,O-Z)
C     PROCEDURE FOR CALCULATING REDUCED OVERLAP INTEGRALS
      COMMON/ARRAYS/S(80,80),Y(9,5,203),Z(17,45),XX(2900)
      COMMON/AUXINT/A(17),B(17)
      INTEGER   ULIM
      N1=NN1
      L1=LL1
      M=MM
      N2=NN2
      L2=LL2
      P = (ALPHA + BETA)/2.D0
      PT = (ALPHA - BETA)/2.D0
      X  = 0.D0
      M = IABS(M)
C     REVERSE QUANTIIM NUMBERS IF NECESSARY
C     UWAGA!!!
      IF((L2.LT.L1).OR.((L2.EQ.L1).AND.(N2.LT.N1))) GO TO 20
10    GO TO 30
20    K = N1
      N1 = N2
      N2 = K
      K = L1
      L1 = L2
      L2 = K
      PT= -PT
30    CONTINUE
      K = MOD((N1+N2-L1-L2),2)
C     FIND   A   AND   B   INTEGRALS
      CALL AINTGS(P,N1+N2)
      CALL BINTGS(PT,N1+N2)
      IF((L1.GT.0).OR.(L2.GT.0)) GO TO 60
C     BEGIN SECTION USED FQR OVERLAP INTEGRALS INVOLVING S FUNCTIONS
C     FIND Z TABLE NUMBER L
40    L = (90-17*N1+N1**2-2*N2)/2
      ULIM = N1+N2
      LLIM = 0
      DO 50 I = LLIM,ULIM
      NNI1 = N1+N2-I+1
50    X = X+Z(I+1,L)*A(I+1)*B(NNI1)/2.D0
      SS = X
      GO TO 80
C     BEGIN SECTION USED FOR OVERLAPS lNVOLVING NON-S FUNCTIONS
C     FIND Y TABLE NUMBER L
60    L=(5-M)*(24-10*M+M**2)*(83-30*M+3*M**2)/120+
     1  (30-9*L1+L1**2-2*N1)*(28-9*L1+L1**2-2*N1)/8+
     2  (30-9*L2+L2**2-2*N2)/2
      LLIM = 0
      DO 70 I = LLIM,8
      ULIM = 4 - MOD(K+I,2)
      DO 70 J = LLIM,ULIM
      IIII = 2*J+MOD(K+I,2)+1
70    X = X+Y(I+1,J+1,L) * A(I+1) * B(IIII)
      SS = X*(FACT(M+1)/8.D0)**2*DSQRT(DFLOAT(2*L1+1)*FACT(L1-M)*
     1 DFLOAT(2*L2+1)*FACT(L2-M)/(4.D0*FACT(L1+M)*FACT(L2+M)))
80    CONTINUE
      RETURN
      END

      SUBROUTINE HARMTR(T,MAXL,E)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION T(9,9),E(3)
      COST = E(3)
      IF((1.D0-COST**2).GT.0.0000000001D0) GO TO 20
10    SINT = 0.D0
      GO TO 30
20    SINT = DSQRT(1.D0-COST**2)
30    CONTINUE
      IF(SINT.GT.0.000001D0) GO TO 50
40    COSP = 1.D0
      SINP = 0.D0
      GO TO 70
50    COSP = E(1)/SINT
60    SINP = E(2)/SINT
70    CONTINUE
      DO 80 I=1,9
      DO 80 J=1,9
80    T(I,J) = 0.D0
      T(1,1) = 1.D0
      IF (MAXL.GT.1) GO TO 100
90    IF (MAXL.GT.0) GO TO 110
      GO TO 120
100   COS2T = COST**2 - SINT**2
      SIN2T = 2.D0 * SINT * COST
      COS2P = COSP**2 - SINP**2
      SIN2P = 2.D0 * SINP * COSP
C     TRANSFORMATION MATRIX ELEMENTS FOR D FUNCTIONS
      SQRT3 = DSQRT(3.D0)
      T(5,5) =     (3.D0*COST**2-1.D0)/2.D0
      T(5,6) =    -SQRT3*SIN2T/2.D0
      T(5,8) =     SQRT3*SINT**2/2.D0
      T(6,5) =     SQRT3*SIN2T*COSP/2.D0
      T(6,6) =     COS2T*COSP
      T(6,7) =    -COST*SINP
      T(6,8) =    -T(6,5)/SQRT3
      T(6,9) =     SINT*SINP
      T(7,5) =     SQRT3*SIN2T*SINP/2.D0
      T(7,6) =     COS2T*SINP
      T(7,7) =     COST*COSP
      T(7,8) =    -T(7,5)/SQRT3
      T(7,9) =    -SINT*COSP
      T(8,5) =     SQRT3*SINT**2*COS2P/2.D0
      T(8,6) =     SIN2T*COS2P/2.D0
      T(8,7) =    -SINT*SIN2P
      T(8,8) =     (1.D0+COST**2)*COS2P/2.D0
      T(8,9) =    -COST*SIN2P
      T(9,5) =     SQRT3*SINT**2*SIN2P/2.D0
      T(9,6) =     SIN2T*SIN2P/2.D0
      T(9,7) =     SINT* COS2P
      T(9,8) =     (1.D0+COST**2)*SIN2P/2.D0
      T(9,9) =     COST*COS2P
110   CONTINUE
C     TRANSFORMATION MATRIX ELEMENTS FOR P FUNCTION
      T(2,2) =    COST*COSP
      T(2,3) =    -SINP
      T(2,4) =    SINT*COSP
      T(3,2) =    COST*SINP
      T(3,3) =    COSP
      T(3,4) =    SINT*SINP
      T(4,2) =    -SINT
      T(4,4) =    COST
120   CONTINUE
      RETURN
      END

      SUBROUTINE RELVEC(R,E,C1,C2)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION E(3),C1(3),C2(3)
      X = 0.D0
      DO 10 I=1,3
      E(I) = C2(I) - C1(I)
      X = X + E(I)**2
10    CONTINUE
      R = DSQRT(X)
      DO 40 I=1,3
      IF (R.GT..000001D0) GO TO 30
20    GO TO 40
30    E(I) =E(I)/R
40    CONTINUE
      RETURN
      END

      FUNCTION FACT(N)
      IMPLICIT REAL*8(A-H,O-Z)
      PRODT = 1.D0
20    DO 30 I=1,N
30    PRODT = PRODT * DFLOAT(I)
40    FACT = PRODT
      RETURN
      END

      SUBROUTINE  BINTGS(X,K)
      IMPLICIT REAL*8(A-H,O-Z)
C     FILLS   ARRAY   OF   B-INTEGRALS. NOTe THAT B(I) IS B( I-1) IN THE
C     USUAL   NOTATION
C     FOR X.GT.3                    EXPONENTIAL FORMULA IS USED
C     FOR 2.LT.X.LE.3 AND K.LE.1O   EXPONENTIAL FORMULA IS USED
C     FOR 2.LT.X.LE.3 AND K.GT.1O   15 TERM SERIES IS USED
C     FOR 1.LT.X.LE.2 AND K.LE.7    EXPONENTIAL FORMULA IS USED
C     FOR 1.LT.X.LE.2 AND K.GT.7    12 TERM SERIES IS USED
C     FOR .5.LT.X.LE.1 AND K.LE.5   EXPONENTIAL FORMULA IS USRD
C     FOR .5.LT.X.LF.1 AND K.GT.5   7 TERM SERIES IS USED
C     FOR X.LE..5                   6 TERM SERIES IS USED
C     * * * * * * * * * * * * * * * * * * * * * * * * * *
      COMMON/AUXINT/A(17),B(17)
      I0 = 0
      ABSX = DABS(X)
      IF(ABSX.GT.3.D0) GO TO 120
10    IF(ABSX.GT.2.D0) GO TO 20
40    IF(ABSX.GT.1.D0) GO TO 50
70    IF(ABSX.GT..5D0) GO TO 80
100   IF(ABSX.GT..000001D0) GO TO 110
      GO TO 170
110   LAST = 6
      GO TO 140
80    IF(K.LE.5) GO TO 120
90    LAST = 7
      GO TO 140
50    IF(K.LE.7) GO TO 120
60    LAST = 12
      GO TO 140
20    IF(K.LE.10) GO TO 120
30    LAST = 15
      GO TO 140

120   EXPX = DEXP(X)
      EXPMX = 1.D0/EXPX
      B(1) = (EXPX-EXPMX)/X
      DO 130 I=1,K
130   B(I+1) = (DFLOAT(I)*B(I)+(-1.D0)**I*EXPX-EXPMX)/X
      GO TO 190
140   DO 160 I = I0,K
      Y = 0.D0
      DO 150 M = I0,LAST
150   Y = Y + (-X)** M*(1.D0-(-1.D0)**(M+I+1))/(FACT(M)*DFLOAT(M+I+1))
160   B(I+1) = Y
      GO TO 190
170   DO 180 I = I0,K
180   B(I+1) = (1.D0-(-1.D0)** (I+1))/ DFLOAT(I+1)
190   CONTINUE
      RETURN
      END

      SUBROUTINE AINTGS(X,K)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/AUXINT/A(17),B(17)
      A(1)=DEXP(-X)/X
      DO 10 I=1,K
10    A(I+1)=(A(I)*DFLOAT(I)+DEXP(-X))/X
      RETURN
      END

      SUBROUTINE MATOUT(N,MATOP)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ARRAYS/A(80,80,3)
      DO 80 M = 1,N,11
      K = M+10
      IF (K.LE.N) GO TO 30
20    K = N
30    CONTINUE
      WRITE(6,40) (J,J=M,K)
      WRITE(2,40) (J,J=M,K)
40    FORMAT(/,7X,11(4X,I2,3X),//)
      DO 60 I=1,N
      WRITE(6,50) I,(A(I,J,MATOP),J=M,K)
      WRITE(2,50) I,(A(I,J,MATOP),J=M,K)
50    FORMAT(1X,I2,4X,50(F9.4))
60    CONTINUE
      WRITE(6,70)
      WRITE(2,70)
70    FORMAT()
80    CONTINUE
      RETURN
      END

      SUBROUTINE HUCKCL
      IMPLICIT REAL*8(A-H,O-Z)
C     EXTENDED HUCKEL THEORY FOR CLOSED SHELLS
C     OVERLAPS ARE IN MATRIX A, COULOMB INTEGRALS (GAMMA) ARE IN MATRIX G
      COMMON/ARRAYS/A(80,80),B(80,80),D(80,80)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/GAB/XXX(400),G(35,35),Q(80),YYY(80),ENERGY,XXY(214)
      COMMON/OPTION/OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      DIMENSION ENEG(18,3), BETA0(18)
      DIMENSION G1(18), F2(18)
      INTEGER CHARGE,OCCA,OCCB,UL,AN,CZ,U,ULIM,ANI
      INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      G1(3) = 0.092012 D0
      G1(4) = 0.1407 D0
      G1(5) = 0.199265 D0
      G1(6) = 0.267708 D0
      G1(7) = 0.346029 D0
      G1(8) = 0.43423 D0
      G1(9) = 0.532305 D0
      F2(3) = 0.049865 D0
      F2(4) = 0.089125 D0
      F2(5) = 0.13041 D0
      F2(6) = 0.17372 D0
      F2(7) = 0.219055 D0
      F2(8) = 0.266415 D0
      F2(9) = 0.31580 D0
      ENEG(1,1) = 7.1761 D0
      ENEG(3,1) = 3.1055 D0
      ENEG(3,2) = 1.258 D0
      ENEG(4,1) = 5.94557 D0
      ENEG(4,2) = 2.563 D0
      ENEG(5,1) = 9.59407 D0
      ENEG(5,2) = 4.001 D0
      ENEG(6,1) = 14.051 D0
      ENEG(6,2) = 5.572 D0
      ENEG(7,1) = 19.31637 D0
      ENEG(7,2) = 7.275 D0
      ENEG(8,1) = 25.39017 D0
      ENEG(8,2) = 9.111 D0
      ENEG(9,1) = 32.2724 D0
      ENEG(9,2) = 11.08 D0
      ENEG(11,1) = 2.804 D0
      ENEG(11,2) = 1.302 D0
      ENEG(11,3) = 0.150 D0
      ENEG(12,1) = 5.1254 D0
      ENEG(12,2) = 2.0516 D0
      ENEG(12,3) = 0.16195D0
      ENEG(13,1) = 7.7706 D0
      ENEG(13,2) = 2.9951 D0
      ENEG(13,3) = 0.22425D0
      ENEG(14,1) = 10.0327D0
      ENEG(14,2) = 4.1325 D0
      ENEG(14,3) = 0.337  D0
      ENEG(15,1) = 14.0327D0
      ENEG(15,2) = 5.4638 D0
      ENEG(15,3) = 0.500  D0
      ENEG(16,1) = 17.6496D0
      ENEG(16,2) = 6.989  D0
      ENEG(16,3) = 0.71325D0
      ENEG(17,1) = 21.5906D0
      ENEG(17,2) = 8.7081 D0
      ENEG(17,3) = 0.97695D0
      BETA0(1) = -9.     D0
      BETA0(3) = -9.     D0
      BETA0(4) = -13.    D0
      BETA0(5) = -17.    D0
      BETA0(6) = -21.    D0
      BETA0(7) = -25.    D0
      BETA0(8) = -31.    D0
      BETA0(9) = -39.    D0
      BETA0(11) = -7.7203 D0
      BETA0(12) = -9.4471 D0
      BETA0(13) = -11.3011D0
      BETA0(14) = -13.065 D0
      BETA0(15) = -15.070 D0
      BETA0(16) = -18.150 D0
      BETA0(17) = -22.330 D0
C     FIND NELECS AND FILL H CORE(DIAGONAL) WITH (I*A)/2
      NELECS = 0
      DO 60 I=1,NATOMS
      NELECS = NELECS+CZ(I)
      LL = LLIM(I)
      UL = ULIM(I)
      ANI = AN(I)
      L = 0
      DO 50 J=LL,UL
      L = L+1
      IF(L.EQ.1) GO TO 10
20    IF(L.LT.5) GO TO 40
30    A(J,J) = -ENEG(ANI,3)/27.21D0
      GO TO 50
40    A(J,J) = -ENEG(ANI,2)/27.21D0
      GO TO 50
10    A(J,J) = -ENEG(ANI,1)/27.21D0
50    CONTINUE
60    CONTINUE
      NELECS = NELECS-CHARGE
      OCCA = NELECS/2
C     FROM HUCKEL HAMILTONIAN IN A (OFF DIAGONAL TWO CENTER TERMS)
      DO 90 I=2,N
      K = U(I)
      L = AN(K)
      UL = I-1
      DO 90 J=1,UL
      KK = U(J)
      LL = AN(KK)
      IF   ((L.GT.9).OR.(LL.GT.9)) GO TO 70
80    A(I,J) = A(I,J)*(BETA0(L)+BETA0(LL))/54.42D0
      A(J,I) = A(I,J)
      GO TO 90
70    A(I,J) = 0.75D0*A(I,J)*(BETA0(L)+BETA0(LL))/54.42D0
      A(J,I) = A(I,J)
90    CONTINUE
      DO 100 I = 1,N
100   Q(I) = A(I,I)
      RHO = 1.D-6
      CALL EIGN(N,RHO)
C     EIGENVECTORS (IN  B) ARE CONVERTED INTO DENSITY MATRIX (IN B)
      DO 140 I = 1,N
      DO 120 J = I,N
      XXX(J) = 0.0D0
      DO 110 K = 1,OCCA
110   XXX(J) = XXX(J)+2.D0*B(I,K)*B(J,K)
120   CONTINUE
      DO 130 J = I,N
130   B(I,J) = XXX(J)
140   CONTINUE
      DO 150 I = 1,N
      DO 150 J = I,N
150   B(J,I) = B(I,J)
C     ADD   V(AB)   T0   HCORE--CNDO
      DO 170 I = 1,N
      J = U(I)
      Q(I) = Q(I) + 0.5D0 * G(J,J)
      DO 160 K = 1,NATOMS
160   Q(I) = Q(I) - DFLOAT(CZ(K)) * G(J,K)
170   CONTINUE
C     EXIT SEGMENTT IF ONLY CNDO APPROXMATIONS ARE DESIRED
      IF (OPTION.EQ.CNDO) GO TO 290
C     INDO MODIFICATION (CORRECTION T0 U(I,I))
180   DO 280 I = 1,NATOMS
      K = AN(I)
      J = LLIM(I)
      IF ((K.GT.1).AND.(K.LT.10)) GO TO 190
      GO TO 280
190   IF (K.LE.3) GO TO 210
200   Q(J) = Q(J) + (DFLOAT(CZ(I))-1.5D0) *G1(K) / 6.D0
210   IF(K.EQ.3) GO TO 220
230   IF(K.EQ.4) GO TO 240
250   TEMP = G1(K) /3.D0 +(DFLOAT(CZ(I))- 2.5D0)* 2.D0 * F2(K)/25.D0
      GO TO  260
240   TEMP = G1(K) / 4.D0
      GO   TO 260
220   TEMP = G1(K) / 12.D0
260   CONTINUE
      DO 270 L=1,3
270   Q(J+L) = Q(J+L) + TEMP
280   CONTINUE
290   CONTINUE
      DO 310   I=1,N
      DO 300   J=I,N
300   A(J,I) = A(I,J)
310   A(I,I) = Q(I)
      WRITE(6,320)
      WRITE(2,320)
320   FORMAT(1X,18H CORE HAMILTONIAN )
      CALL   SCFOUT(0,1)
      RETURN
      END

      SUBROUTINE SCFCLO
      IMPLICIT REAL*8(A-H,O-Z)
C     CNDO/INDO CLOSED SHELL SCF SEGMENT
C     GAMMA MATRIX CONTAINED IN G, CORE HAMILTONIAN CONTAINED IN Q AND
C     UPPER TRIANGLE OF A, AND INITIAL DENSITY MATRIX CONTAINED IN B
C     OPTIONS CNDO OR INDO
      COMMON/ARRAYS/A(80,80),B(80,80),D(80,80)
      COMMON/GAB/XXX(400),G(35,35),Q(80),YYY(80),ENERGY,XXY(214)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/OPTION/OPTION,OPENCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER OPTION,OPENCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN,W
      INTEGER CHARGE,OCCA,OCCB,UL,ULIM,U,AN,CZ,Z
      DIMENSION G1(18),F2(18)
      G1(3) = .092012D0
      G1(4) = .1407  D0
      G1(5) = .199265D0
      G1(6) = .267708D0
      G1(7) = .346029D0
      G1(8) = .43423D0
      G1(9) = .532305D0
      F2(3) = .049865D0
      F2(4) = .089125D0
      F2(5) = .13041D0
      F2(6) = .17372D0
      F2(7) = .219055D0
      F2(8) = .266415D0
      F2(9) = .31580D0
      Z = 0
      IT = 25
      RHO = 1.D-6
10    CONTINUE
      Z = Z+1
      ENERGY = 0.D0
C     TRANSFER CORE HAHILTONIAN T0 LOWER TRINAGLE OF A
      DO 20 I=1,N
      A(I,I) = Q(I)
      DO 20 J=I,N
20    A(J,I) = A(I,J)
      DO 30 I=1,N
      II = U(I)
      A(I,I) = A(I,I)- B(I,I)* G(II,II) * 0.5D0
      DO 30 K=1,N
      JJ = U(K)
30    A(I,I) = A(I,I) + B(K,K) * G(II,JJ)
      NM = N-1
      DO 40 I=1,NM
      II = U(I)
      LL = I+1
      DO 40 J=LL,N
      JJ = U(J)
40    A(J,I) = A(J,I) - B(J,I) * G(II,JJ) * 0.5D0
C     INDO MODIFICATION
      IF (OPTION.EQ.CNDO) GO TO 90
50    DO 80 II = 1,NATOMS
      K = AN(II)
      I = LLIM(II)
      IF (K.EQ.1) GO TO 80
60    PAA = B(I,I)+ B(I+1,I+1)+ B(I+2,I+2)+ B(I+3,I+3)
      A(I,I) = A(I,I) - (PAA-B(I,I))* G1(K)/6.D0
      DO 70 J = 1,3
      A(I+J,I+J) = A(I+J,I+J) - B(I,I) * G1(K)/6.D0 - (PAA-B(I,I))*7.D0*
     1F2(K)/50.D0+B(I+J,I+J)*11.D0 * F2(K)/50.D0
70    A(I+J,I) = A(I+J,I) + B(I,I+J) * G1(K)/2.D0
      I1 = I+1
      I2 = I+2
      I3 = I+3
      A(I2,I1) = A(I2,I1) + B(I2,I1)* 11.D0 * F2(K) / 50.D0
      A(I3,I1) = A(I3,I1) + B(I3,I1)* 11.D0 * F2(K) / 50.D0
      A(I3,I2) = A(I3,I2) + B(I3,I2)* 11.D0 * F2(K) / 50.D0
80    CONTINUE
90    CONTINUE
      DO 100 I=1,N
100   ENERGY = ENERGY + 0.5D0 * B(I,I) * (A(I,I)+ Q(I))
      DO 105 I=1,NM
      LL = I+1
      DO 105 J=LL,N
105   ENERGY = ENERGY + B(I,J) * (A(I,J) + A(J,I))
      WRITE(6,110) ENERGY
      WRITE(2,110) ENERGY
110   FORMAT(/,10X,22H ELECTRONIC ENERGY    ,F16.10,5H a.u.)
      IF(DABS(ENERGY-OLDENG).GE.0.000001D0) GO TO 150
120   Z=26
130   WRITE(6,140)
      WRITE(2,140)
140   FORMAT(5X,18H ENERGY SATISFIED ,/)
      GO TO 170
150   CONTINUE
160   OLDENG = ENERGY
170   CONTINUE
      IF(Z.LE.IT) GO TO 210
C     SYMMETRIZE F TOR PRINTING (MATRIX A)
180   DO 190 I=1,N
      DO 190 J=I,N
190   A(I,J)=A(J,I)
      WRITE(6,200)
      WRITE(2,200)
200   FORMAT(1X,27H HARTRFE-FOCK ENERG MATRIX )
      CALL   SCFOUT(0,1)
210   CONTINUE
      CALL   EIGN(N,RHO)
      IF (Z.LE.IT) GO TO 240
220   WRITE(6,230)
      WRITE(2,230)
230   FORMAT(1X,29H EIGENVALUES AND EIGENVECTORS)
      CALL   SCFOUT(1,2)
240   CONTINUE
C     EIGENVECTORS (IN B) ARE CONVERTED INTO DENSITIY MATRIX (IN  B)
      DO 280   I=1,N
      DO 260   J=I,N
      XXX(J) = 0.0D0
      DO 250 K=1,OCCA
250   XXX(J) = (XXX(J) + B(I,K) * B(J,K) * 2.0D0)
260   CONTINUE
      Do   270 J=I,N
270   B(I,J) = XXX(J)
280   CONTINUE
      DO   290   I=1,N
      DO   290   J=I,N
290   B(J,I) = B(I,J)
      IF (Z.LE.IT) GO  TO  10
300   CONTINUE
      RETURN
      END


      SUBROUTINE CPRINT
      IMPLICIT REAL*8(A-H,O-Z)
C     CNDO-INDO SCF CLOSED SHELL- PRINTOUT SEGMENT
      COMMON/ARRAYS/ A(80,80), B(80,80), D(80,80)
      COMMON/GAB/XXX(400),G(35,35),Q(80),YYY(80),ENERGY,XXY(214)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/PERTBL/EL(18)
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/OPTION/OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER CHARGE,AN,U,ULIM,FL,OCCA,OCCB,UL,CZ,ANI
      DIMENSION DPM(3), DM(3), DMSP(3), DMPD(3)
      DIMENSION ATENG(18)
      IF (OPTION.EQ.CNDO) GO TO 20
      ATENG(1) = -0.6387302462  D0
      ATENG(3) = -.2321972405   D0
      ATENG(4) = -1.1219620354  D0
      ATENG(5) = -2.8725750048   D0
      ATENG(6) = -5.9349548261  D0
      ATENG(7) = -10.6731741251 D0
      ATENG(8) = -17.2920850650 D0
      ATENG(9) = -26.2574377875 D0
      GO TO 30
20    CONTINUE
      ATENG(1) = -0.6387302462  D0
      ATENG(3) = -0.2321972405 D0
      ATENG(4) = -1.1454120355 D0
      ATENG(5) = -2.977423904 D0
      ATENG(6) = -6.1649936261  D0
      ATENG(7) = -11.0768746252 D0
      ATENG(8) = -18.0819658651 D0
      ATENG(9) = -27.5491302880 D0
      ATENG(11) = -.1977009568  D0
      ATENG(12) = -.8671913833  D0
      ATENG(13) = -2.0364557744 D0
      ATENG(14) = -3.8979034686 D0
      ATENG(15) = -6.7966009163 D0
      ATENG(16) = -10.7658174341D0
      ATENG(17) = -16.0467017940D0
30    CONTINUE
      K = NATOMS-1
      WRITE(6,40)
      WRITE(2,40)
40    FORMAT(1X,15H DENSITY MATRIX)
      CALL SCFOUT(0,2)
      DO 50 I=1,K
      L = I+1
      DO 50 J=L,NATOMS
      RAD = DSQRT((C(I,1)-C(J,1))**2+(C(I,2)-C(J,2))**2+
     1            (C(I,3)-C(J,3))**2)
50    ENERGY = ENERGY + (DFLOAT(CZ(I)) * DFLOAT(CZ(J)))/RAD
      WRITE(6,60) ENERGY
      WRITE(2,60) ENERGY
60    FORMAT(//,10X,16H TOTAL ENERGY = ,F16.10,5H a.u.)
      DO 70 I=1,NATOMS
      ANI = AN(I)
70    ENERGY = ENERGY-ATENG(ANI)
      WRITE(6,80)   ENERGY
      WRITE(2,80)   ENERGY
80    FORMAT(//,10X,16H BINDING ENERGY= ,F16.10,5H a.u.,/)
      DO   110   I=1,NATOMS
      TCHG = 0.D0
      LL = LLIM(I)
      UL = ULIM(I)
      DO 90 J = LL,UL
90    TCHG = TCHG+B(J,J)
      ANI = AN(I)
      WRITE(6,100)  I,EL(ANI),TCHG
      WRITE(2,100)  I,EL(ANI),TCHG
100   FORMAT(I3,A4,8X,F7.4)
      XXX(I)=TCHG
110   CONTINUE
      DO   120   I=1,3
      DM(I)=0.0D0
      DMSP(I)=0.0D0
120   DMPD(I)=0.0D0
      DO   200   J=1,NATOMS
      IF (AN(J).LT.3) GO   TO   180
130   IF (AN(J).LT.11) GO   TO   140
160   SLTR1 = (0.65D0 * DFLOAT(AN(J)) - 4.95D0)/3.D0
      FACTOR = 2.5416D0*7.D0/(DSQRT(5.D0)*SLTR1)
      INDEX = LLIM(J)
      DO 170 K=1,3
170   DMSP(K) = DMSP(K) - B(INDEX,INDEX+K) * 10.27175D0 / SLTR1
      DMPD(1) = DMPD(1) - FACTOR*(B(INDEX+2,INDEX+8)+B(INDEX+3,INDEX+5)
     1   +B(INDEX+1,INDEX+7)-1.D0/DSQRT(3.D0)*B(INDEX+1,INDEX+4))
      DMPD(2) = DMPD(2) - FACTOR*(B(INDEX+1,INDEX+8)+B(INDEX+3,INDEX+6)
     1   +B(INDEX+2,INDEX+7)-1.D0/DSQRT(3.D0)*B(INDEX+2,INDEX+4))
      DMPD(3) = DMPD(3) - FACTOR*(B(INDEX+1,INDEX+5)+B(INDEX+2,INDEX+6)
     1   +2.D0/DSQRT(3.D0)*B(INDEX+3,INDEX+4))
      GO TO 180
140   INDEX = LLIM(J)
      DO 150 K = 1,3
150   DMSP(K) = DMSP(K) -B(INDEX,INDEX+K)*7.33697D0/
     1  (.325*DFLOAT(AN(J)-1))
180   DO 190 I = 1,3
190   DM(I) = DM(I) + (DFLOAT(CZ(J))-XXX(J)) * C(J,I) * 2.5416D0
200   CONTINUE
      DO 210 I=1,3
210   DPM(I) = DM(I) + DMSP(I) + DMPD(I)
      WRITE(6,220)
      WRITE(2,220)
220   FORMAT(//,20X,16H  DIPOLE MOMENTS,/)
      WRITE(6,230)
      WRITE(2,230)
230   FORMAT(5X,11H COMPONENTS,3X,2H X,8X,2H Y,8X,2H Z)
      WRITE(6,240)DM(1),DM(2),DM(3)
      WRITE(2,240)DM(1),DM(2),DM(3)
240   FORMAT(5X,10H DENSITIES,3(1X,F9.5))
      WRITE(6,250)DMSP(1) ,DMSP(2),DMSP(3)
      WRITE(2,250)DMSP(1) ,DMSP(2),DMSP(3)
250   FORMAT(5X,4H S,P,6X,3(1X,F9.5))
      WRITE(6,260)DMPD(1),DMPD(2),DMPD(3)
      WRITE(2,260)DMPD(1),DMPD(2),DMPD(3)
260   FORMAT(5X,4h P,D,6X,3(1X,F9.5))
      WRITE(6,270)DPM(1),DPM(2),DPM(3)
      WRITE(2,270)DPM(1),DPM(2),DPM(3)
270   FORMAT(5X,6H TOTAL,4X,3(1X,F9.5),/)
      DP = DSQRT(DPM(1)**2+DPM(2)**2+DPM(3)**2)
      WRITE(6,280) DP
      WRITE(2,280) DP
280   FORMAT(3X,15H DIPOLE MOMENT=,F9.5,7H DEBYES//)
      RETURN
      END


      SUBROUTINE HUCKOP
      IMPLICIT REAL*8(A-H,O-Z)
C     EXTENDED HUCKEL THEORY FOR OPEN SHELLS
C     OVERLAP IS IN A, GAMMA MATRIX IS IN G
C     AN INITIAL F MATRIX IS FORMED FROM -(I+A)/2 AND S(U,V)*(1/2)*
C     (BETA0A+BETA0B). THIS F MATRIX IS USED TO GENERATE AN INITIAL
C     DENSITY MATRIX. AT THIS POINT, ADDITIONAL INTEGRALS AND CORE
C     REACTIONS ARE ADDED TO THE F MATRIX T0   FORM   EITHER THE   CNDO OR INDO
C     CORE HAMILTONIAN. THESE   AnDITIONS ARE THE INTEGRALS V(AB) FOR CNDO AND
C     CORRECTIONS TO U(I,I) FOR INDO.
      COMMON/ARRAYS/ A(80,80), B(80,80), Q(80,80)
      COMMON/GAB/XXX(400),G(35,35),FDIAG(80),PDIAG(80),ENERGY,YYY(214)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/OPTION/OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      DIMENSION ENEG(18,3),BETA0(18)
      DIMENSION G1(18),F2(18)
      INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER CHARGE,OCCA,OCCB,UL,AN,CZ,U,ULIM,ANI
      G1(3) = 0.092012 D0
      G1(4) = 0.1407   D0
      G1(5) = 0.199265 D0
      G1(6) = 0.267708 D0
      G1(7) = 0.346029 D0
      G1(8) = 0.43423  D0
      G1(9) = 0.532305 D0
      F2(3) = 0.049865 D0
      F2(4) = 0.089125 D0
      F2(5) = 0.13041  D0
      F2(6) = 0.17372  D0
      F2(7) = 0.219055 D0
      F2(8) = 0.266415 D0
      F2(9) = 0.31580  D0
      ENEG(1,1) = 7.1761 D0
      ENEG(3,1) = 3.1055 D0
      ENEG(3,2) = 1.258  D0
      ENEG(4,1) = 5.94557D0
      ENEG(4,2) = 2.563  D0
      ENEG(5,1) = 9.59407D0
      ENEG(5,2) = 4.001  D0
      ENEG(6,1) = 14.051 D0
      ENEG(6,2) = 5.572  D0
      ENEG(7,1) = 19.31637 D0
      ENEG(7,2) = 7.275 D0
      ENEG(8,1) = 25.39017 D0
      ENEG(8,2) = 9.111 D0
      ENEG(9,1) = 32.2724 D0
      ENEG(9,2) = 11.08   D0
      ENEG(11,1) = 2.804  D0
      ENEG(11,2) = 1.302  D0
      ENEG(11,3) = 0.150  D0
      ENEG(12,1) = 5.1254 D0
      ENEG(12,2) = 2.0516 D0
      ENEG(12,3) = 0.16195D0
      ENEG(13,1) = 7.7706 D0
      ENEG(13,2) = 2.9951 D0
      ENEG(13,3) = 0.22425D0
      ENEG(14,1) = 10.0327D0
      ENEG(14,2) = 4.1325 D0
      ENEG(14,3) = 0.337  D0
      ENEG(15,1) = 14.0327D0
      ENEG(15,2) = 5.4638 D0
      ENEG(15,3) = 0.500  D0
      ENEG(16,1) = 17.6496D0
      ENEG(16,2) = 6.989  D0
      ENEG(16,3) = 0.71325D0
      ENEG(17,1) = 21.5906D0
      ENEG(17,2) = 8.7081 D0
      ENEG(17,3) = 0.97695D0
      BETA0(1)= -9.     D0
      BETA0(3)= -9.     D0
      BETA0(4)= -13.    D0
      BETA0(5)= -17.    D0
      BETA0(6)= -21.    D0
      BETA0(7)= -25.    D0
      BETA0(8)= -31.    D0
      BETA0(9)= -39.    D0
      BETA0(11)= -7.7203 D0
      BETA0(12)= -9.4471 D0
      BETA0(13)= -11.3011D0
      BETA0(14)= -13.065 D0
      BETA0(15)= -15.070 D0
      BETA0(16)= -18.150 D0
      BETA0(17)= -22.330 D0
C     FIND NELECS AND FILL M CORE(DIAGONAL) MITH (I+A)/2
      NELECS = 0
      DO 60 I=1,NATOMS
      NELECS = NELECS + CZ(I)
      LL = LLIM(I)
      UL = ULIM(I)
      ANI = AN(I)
      L = 0
      DO 50 J=LL,UL
      L=L+1
      IF (L.EQ.1) GO TO 10
20    IF (L.LT.5) GO TO 40
30    A(J,J) = -ENEG(ANI,3)/27.21D0
      GO TO 50
40    A(J,J) = -ENEG(ANI,2)/27.21D0
      GO TO 50
10    A(J,J) = -ENEG(ANI,1)/27.21D0
50    CONTINUE
60    CONTINUE
      NELECS = NELECS-CHARGE
      OCCA = (NELECS+MULTIP-1)/2
      OCCB = (NELECS-MULTIP+1)/2
C     FROM HUCKEL HAMILTONIAN IN A (OFF DIAGONAL TWO CENTER TERMS)
      DO 90 I=2,N
      K = U(I)
      L = AN(K)
      UL = I-1
      DO 90 J=1,UL
      KK = U(J)
      LL = AN(KK)
      IF((L.GT.9).OR.(LL.GT.9)) GO TO 70
80    A(I,J) = A(I,J)*(BETA0(L)+BETA0(LL))/54.42D0
      A(J,I)=A(I,J)
      GO TO 90
70    A(I,J) = 0.75D0 * A(I,J) * (BETA0(L)+BETA0(LL)) / 54.42D0
      A(J,I) = A(I,J)
90    CONTINUE
      DO 100 I=1,N
      DO 100 J=1,N
100   Q(I,J) = A(I,J)
      RHO = 1.D-6
      CALL EIGN(N,RHO)
      DO 110 I = 1 ,N
      PDIAG(I) = 0.0D0
      DO 110 J = 1, N
      A(I,J) = B(I,J)
110   B(I,J) = 0.0D0
      DO 160 I = 1,N
      DO 120 K = 1,OCCA
120   B(I,I) = B(I,I) + A(I,K) * A(I,K)
      DO 130 K = 1,OCCB
130   PDIAG(I) = PDIAG(I) + A(I,K) * A(I,K)
      LL = I+1
      DO 160 J = LL,N
      DO 140 K = 1,OCCB
140   B(I,J) = B(I,J) + A(I,K) * A(J,K)
      DO 150 K = 1,OCCA
150   B(J,I) = B(J,I) + A(I,K) * A(J,K)
160   CONTINUE

C     ADD V(AB) T0 HCORE--CNDO
      DO  180  I=1,N
      J = U(I)
      Q(I,I) = Q(I,I)+ 0.5D0 * G(J,J)
      DO 170 K=1,NATOMS
170   Q(I,I) = Q(I,I) - DFLOAT(CZ(K)) * G(J,K)
180   CONTINUE

C     EXIT SEGMENT IF ONLY CNDO APPROXIMATIONS ARE DESIRED
      IF(OPTION.EQ.CNDO) GO TO 300

C     INDO MODIFICATION (CORRECTION TO U(I,I))
190   DO 290 I=1,NATOMS
      K = AN(I)
      J = LLIM(I)
      IF((K.GT.1).AND.(K.LT.10)) GO TO 200
      GO TO 290
200   IF (K.LE.3) GO TO 220
210   Q(J,J) = Q(J,J) + (DFLOAT(CZ(I)) - 1.5D0) * G1(K) / 6.D0
220   IF(K.EQ.3) GO TO 230
240   IF(K.EQ.4) GO TO 250
260   TEMP =(G1(K)/3.D0+(DFLOAT(CZ(I))-2.5D0) * 2.D0*F2(K)/25.D0)
      GO TO 270
250   TEMP = G1(K)/4.D0
      GO TO 270
230   TEMP = G1(K)/12.D0
270   CONTINUE
      DO 280 L =1 ,3
280   Q(J+L,J+L) = Q(J+L,J+L) + TEMP
290   CONTINUE
300   CONTINUE
      WRITE(6,310)
      WRITE(2,310)
310   FORMAT(1X,17H CORE HAMILTONIAN /)
      CALL SCFOUT(0,3)
      RETURN
      END


      SUBROUTINE SCFOPN
      IMPLICIT REAL*8(A-H,O-Z)
C     CNDO/INDO OPEN SHELL SCF SEGMENT
C     GAMMA MATRIX CONTAINED IN G. CORE HAHILTONIAN CONTAINED IN Q
C     INITIAL DENSITY MATRICES IN B
C     OPTIONS CNDO OR INDO
C     AND THE APPROPRIATE CORE HAMLTONIAN. THE TWO ELECTRON INTEGRALS
C     ARE ADDED TO THE F MATRIX (A) IN TMO PARTS - FIRST THE CNDO GAMMAS
C     ARE ADDED AND THEN THE INDO CORRECTIONS To THE ONE-CENTER INTEGRALS
C     THE PROCEDURE IS THAT F(ALPHA) AND F(BETA) ARE FORMED, THEN
C     THE ELECTRON C FNERGY IS COMPUTED EIGN IS CALLED TO DIAGONALIZE
C     THE TwO F MATRICES AND THE ALPHA AND BETA BONDORERS ARE FORMED.
C     THESE ARE USED TO FORM NEW F MATRICES AND THE CYCLE IS REPEATED
C     UNTIL THE ENERGY CONVERGES TO THE DESIRED VALUE (.000001 IN THIS
C     PROGRAM).
C     AK UPPER LIMIT OF 25 CYCLES IS INCLUDED (IT)
      COMMON/ARRAYS/A(80,80),B(80,80),Q(80,80)
      COMMON/GAB/XXX(400),G(35,35),FDIAG(80),PDIAG(80),ENERGY,YYY(214)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/OPTION/OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      DIMENSION G1(18), F2(18)
      INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER CHARGE,OCCA,OCCB,UL,AN,CZ,U,ULIM,Z
      G1(3) = 0.092012 D0
      G1(4) = 0.1407   D0
      G1(5) = 0.199265 D0
      G1(6) = 0.267708 D0
      G1(7) = 0.346029 D0
      G1(8) = 0.43423  D0
      G1(9) = 0.532305 D0
      F2(3) = 0.049865 D0
      F2(4) = 0.089125 D0
      F2(5) = 0.13041  D0
      F2(6) = 0.17372  D0
      F2(7) = 0.219055 D0
      F2(8) = 0.266415 D0
      F2(9) = 0.31580  D0
C     INITIALIZS COUNTER Z AND BEGIN SCF CYCLE AT 10
      Z = 0
      IT = 25
      RHO = 1.D-6
10     CONTINUE
      Z = Z+1
      ENERGY = 0.0D0
C     TRNSFER CORE HAMILTONIAM TO A
      DO 20 I=1,N
      FDIAG(I) = Q(I,I)
      DO 20 J=1,N
20    A(I,J) = Q(I,J)
      DO 30 I=1,N
      II = U(I)
      A(I,I) = A(I,I )- B(I,I) * G( II,II)
      FDIAG(I) = FDIAG(I) - PDIAG(I) * G(II,II)
      DO 30 K=1,N
      JJ = U(K)
      A(I,I) = A(I,I) + ( PDIAG(K)+ B (K,K))* G (II,JJ)
30    FDIAG(I) = FDIAG(I) + ( PDIAG(K) + B(K,K)) * G(II,JJ)
      NM = N-1
      DO 50 I=1,NM
      II = U(I)
      LL = I+1
      DO 40 J=LL,N
      JJ = U(J)
      A(I,J) = A(I,J) - B(I,J) * G(II,JJ)
40    A(J,I) = A(J,I) - B(J,I) * G(II,JJ)
50    CONTINUE
C     INDO  MODIFICATION
      IF (OPTION.EQ.CNDO) GO TO 100
60    DO 90 II=1,NATOMS
      K = AN(II)
      I = LLIM(II)
      IF (K.EQ.1) GO TO 90
70    PAA = B(I,I) + B(I+1,I+1) + B(I+2,I+2) + B(I+3,I+3)
      PAB = PDIAG(I) + PDIAG(I+1) + PDIAG(I+2)+ PDIAG(I+3)
      A(I,I) = A(I,I)-(PAA-B(I,I)) *G1(K)/3.D0
      FDIAG(I) = FDIAG(I) - (PAB-PDIAG(I)) * G1(K)/3.D0
      DO 80 J=1,3
      A(I+J,I+J)= A(I+J,I+J)+(B(I+J,I+J)-(PAA-B(I,I)))*F2(K)/5.D0-B(I,I)
     1*G1(K)/3.D0+(6.D0*PDIAG(I+J)-2.D0*(PAB- PDIAG(I))) * F2(K)/25.D0
      FDIAG(I+J) = FDIAG(I+J) + (PDIAG(I+J)-(PAB -PDIAG(I)))*F2(K)/5.D0
     1  -PDIAG(I)*G1(K)/3.D0+ (6.D0 * B(I+J,I+J) - 2.D0 * (PAA-B(I,I)))
     2  *F2(K)/25.D0
      A(I,I+J) = A(I,I+J) + (B(I,I+J) + 2.D0 * B(I+J,I)) * G1(K)/3.D0
      A(I+J,I) = A(I+J,I) + (B(I+J,I) + 2.D0 * B(I,I+J)) * G1(K)/3.D0
      DO 80 L=1,3
      IF (J.EQ.L) GO TO 80
75    A(I+L,I+J) = A(I+L,I+J) + (5.D0 * B(I+L,I+J) + 6.D0 * B(I+J,I+L))
     1  * F2(K)/25.D0
80    CONTINUE
90    CONTINUE
100   CONTINUE
      DO 110 I = 1,N
110   ENERGY = ENERGY+0.5D0*((A(I,I)+Q(I,I))*B(I,I)+(FDIAG(I)+Q(I,I))
     1  * PDIAG(I))
      DO 115 I = 1,NM
      LL = I+1
      DO 115 J = LL,N
115   ENERGY = ENERGY+((A(I,J)+Q(I,J))*B(I,J)+( A(J,I)+Q(J,I)) * B(J,I))
      WRITE(6,120) ENERGY
      WRITE(2,120) ENERGY
120   FORMAT(/,10X,22H ELECTRONIC ENERGY    ,F16.10,5H a.u.)
      IF(DABS(ENERGY - OLDENG).GE.1.D-6) GO TO 160
130   Z = 26
140   WRITE(6,150)
      WRITE(2,150)
150   FORMAT(5X,18H ENERGY SATISFIED /)
      GO TO 180
160   CONTINUE
170   OLDENG = ENERGY
180   CONTINUE
      IF(Z.LE.IT) GO TO 240
C     TRANSFER F(ALPHA) TO Q FOR PRINTING
190   DO 200 I=1,N
      DO 200 J=I,N
      Q(I,J) = A(J,I)
200   Q(J,I) = A(J,I)
      WRITE(6,210)
      WRITE(2,210)
210   FORMAT(/,1X,42H HARTREE-FOCK ENERGY MATRIX FOR ALPHA SPIN/)
      CALL SCFOUT(0,3)

c     TRANSFER F(BETA) TO Q FOR PRINTING
      DO 220 I = 1,N
      Q(I,I) = FDIAG(I)
      LL = I+1
      DO 220 J=LL,N
      Q(I,J) = A(I,J)
220   Q(J,I) = A(I,J)
      WRITE(6,230)
      WRITE(2,230)
230   FORMAT(/,1X,41H HARTREE-FOCK ENERGY MATRIX FOR BETA SPIN/)
      CALL SCFOUT(0,3)
240   CONTINUE
      CALL EIGN(N,RHO)
      IF (Z.LE.IT) GO TO 270
250   WRITE(6,260)
      WRITE(2,260)
260   FORMAT(/,1X,43HEIGENVALUES AND EIGENVECTORS FOR ALPHA SPIN/)
      CALL SCFOUT(1,2)
270   CONTINUE
C       TRANSFER F(BETA) T0 LOWER HALF OF A
      DO 280 I=1,N
      A(I,I) = FDIAG(I)
      FDIAG(I) = 0.0D0
      K = I+1
      DO 280 J = K,N
      A(J,I) = A(I,J)
280   A(I,J) = 0.0D0
C     FOR ALPHA BONDORDERS IN TOP HALF OF A AND IN FDIAG TEMPORARY
      DO 300 I=1,N
      LL = I+1
      DO 290 K=1,OCCA
290   FDIAG(I) = FDIAG(I) + B(I,K) * B(I,K)
      DO 300 J=LL,N
      DO 300 K=1,OCCA
300   A(I,J) = A(I,J)+B(I,K)*B(J,K)
      CALL EIGN(N,RHO)
      IF  (Z.LE.IT) GO TO 330
310   WRITE(6,320)
      WRITE(2,320)
320   FORMAT(/,1X,42HEIGENVALUES AND EIGENVECTORS FOR BETA SPIN/)
      CALL SCFOUT(1,2)
330   CONTINUE
C     FROM BETA BONDORDERS IN LOWER HALF OF A AND IN PDIAG
      DO 350 I=1,N
      LL = I+1
      PDIAG(I) = 0.0D0
      DO 340 K = 1,OCCB
340   PDIAG(I) = PDIAG(I) + B(I,K) * B(I,K)
      DO 350 J = LL,N
      A(J,I) = 0.0D0
      DO 350 K = 1,OCCB
350   A(J,I) = A(J,I) + B(I,K) * B(J,K)
C       TRANSFER BONDOHDERS FROM A TO B
      DO 370 I=1,N
      DO 360 J=1,N
360   B(I,J) = A(J,I)
370   B(I,I) = FDIAG(I)
      IF (Z.LE.IT) GO TO 10
380   CONTINUE
      RETURN
      END

      SUBROUTINE OPRINT
      IMPLICIT REAL*8(A-H,O-Z)
C     CNDO-INDO OPEN SHELL PRINTOUT SEGMENT
      COMMON/ARRAYS/A(80,80),B(80,80),Q(80,80)
      COMMON/GAB/XXX(400),G(35,35),FDIAG(80),PDIAG(80),ENERGY,YYY(214)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/OPTION/OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      COMMON/PERTBL/EL(18)
      DIMENSION CISO(10)
      DIMENSION DPM(3),DM(3),DMSP(3),DMPD(3)
      DIMENSION ATENG(18)
      INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER CHARGE,AN,U,ULIM,FL,OCCA,OCCB,UL,CZ,ANI
      IF (OPTION.EQ.CNDO) GO TO 20
      ATENG(1)= -0.6387302462  D0
      ATENG(3)= -0.2321972405 D0
      ATENG(4)= -1.1219620354 D0
      ATENG(5)= -2.8725750048 D0
      ATENG(6)= -5.9349548261 D0
      ATENG(7)= -10.6731741251 D0
      ATENG(8)= -17.2920850650 D0
      ATENG(9)= -26.2574377875 D0
      GO TO 30
20    CONTINUE
      ATENG(1)= -0.6387302462  D0
      ATENG(3)= -0.2321972405 D0
      ATENG(4)= -1.1454120355 D0
      ATENG(5)= -2.977423904 D0
      ATENG(6)= -6.1649936261  D0
      ATENG(7)= -11.0768746252 D0
      ATENG(8)= -18.0819658651 D0
      ATENG(9)= -27.5491302880 D0
      ATENG(11)= -.1977009568  D0
      ATENG(12)= -.8671913833  D0
      ATENG(13)= -2.0364557744 D0
      ATENG(14)= -3.8979034686 D0
      ATENG(15)= -6.7966009163 D0
      ATENG(16)= -10.7658174341D0
      ATENG(17)= -16.0467017940D0
30    CONTINUE
      K = NATOMS-1
C     BONDORDER HALF MATRICES ARE NOW BEGIN STORED IN FULL MATRICES FOR
C     PRINTING---ALPHA IN B AND BETA IN A
      DO 40 I=1,N
      A(I,I) = PDIAG(I)
      LL = I+1
      DO 40 J=LL,N
      A(I,J) = B(I,J)
      A(J,I) = B(I,J)
40    B(I,J) = B(J,I)
      WRITE(6,50)
      WRITE(2,50)
50    FORMAT(/,1X,23H ALPHA BONDORDER MATRIX/)
      CALL  SCFOUT(0,2)
      WRITE(6,60)
      WRITE(2,60)
60    FORMAT(/,1X,22H BETA BONDORDER MATRIX/)
      CALL  SCFOUT(0,1)
70    CONTINUE
      DO 80 I=1,N
      DO 80 J=1,N
      B(I,J) = A(I,J)+B(I,J)
80    A(I,J) = B(I,J)-2.D0*A(I,J)
      WRITE(6,90)
      WRITE(2,90)
90    FORMAT(/,1X,25H SCF TOTAL DENSITY MATRIX/)
      CALL SCFOUT(0,2)
      WRITE(6,100)
      WRITE(2,100)
100   FORMAT(/,1X,24H SCF SPIN DENSITY MATRIX/)
      CALL SCFOUT(0,1)
      DO 110 I = 1,K
      L = I+1
      DO 110 J = L,NATOMS
      RAD = DSQRT((C(I,1)-C(J,1))**2 + (C(I,2)-C(J,2))**2 +
     1            (C(I,3)-C(J,3))**2)
110   ENERGY = ENERGY + (DFLOAT(CZ(I))*DFLOAT(CZ(J)))/RAD
      WRITE(6,120) ENERGY
      WRITE(2,120) ENERGY
120   FORMAT(//10X,16H TOTAL ENERGY = ,F16.10,5H a.u.)
      DO 130 I=1,NATOMS
      ANI = AN(I)
130   ENERGY = ENERGY - ATENG(ANI)
      WRITE(6,140)   ENERGY
      WRITE(2,140)   ENERGY
140   FORMAT(//,10X,16H BINDING ENERGY= ,F16.10,5H a.u.,/)
      CISO(1) = 539.8635D0
      CISO(6) = 820.0959D0
      CISO(7) = 379.3557D0
      CISO(8) = 888.6855D0
      CISO(9) = 44829.2D0
      WRITE(6,150)
      WRITE(2,150)
150   FORMAT(15X,7HVALENCE,10X,9HS ORBITAL,10X,9HHYPERFINE)
      WRITE(6,160)
      WRITE(2,160)
160   FORMAT(8X,56H  ELECTRON DENSITY     SPIN DENSITY    COUPLING CONSTANT)
      WRITE(6,170)
      WRITE(2,170)
170   FORMAT(80X)
      DO 200 I=1,NATOMS
      TCHG = 0.0D0
      LL = LLIM(I)
      UL = ULIM(I)
      ANI = AN(I)
      HFC = CISO(ANI)*A(LL,LL)
      IF (OPTION.EQ.INDO) GO TO 2
1     HFC = 0.0D0
2     CONTINUE
      DO 180 J=LL,UL
180   TCHG = TCHG + B(J,J)
      WRITE(6,190) I,EL(ANI),TCHG,A(LL,LL),HFC
      WRITE(2,190) I,EL(ANI),TCHG,A(LL,LL),HFC
190   FORMAT(I3,A4,8X,F7.4,10X,F7.4,12X,F9.4)
      XXX(I) = TCHG
200   CONTINUE
C     NEW MODIFICATION - THERE ARE NO PHYSICAL MEANINING OF DIPOLE MOMENT IN MOLECULES WITH CHARGE
      IF (CHARGE.NE.0) GO TO 380
      DO 210 I=1,3
      DM(I) = 0.0D0
      DMSP(I) = 0.0D0
210   DMPD(I) = 0.0D0
      DO 290 J=1,NATOMS
      IF (AN(J).LT.3) GO TO 270
220   IF (AN(J).LT.11) GO TO 230
250   SLTR1 = (0.65D0 * DFLOAT(AN(J))-4.95D0)/3.D0
      FACTOR = 2.5416D0*7.D0/(DSQRT(5.D0)*SLTR1)
      INDEX = LLIM(J)
      DO 260 K = 1,3
260   DMSP(K) = DMSP(K)-B(INDEX,INDEX+K)* 10.27175D0/SLTR1
      DMPD(1) = DMPD(1)-FACTOR*(B(INDEX+2,INDEX+8)+B(INDEX+3,INDEX+5)
     1  + B(INDEX+1,INDEX+7)  -1.D0 / DSQRT(3.D0)*B(INDEX+1,INDEX+4))
      DMPD(2) = DMPD(2)-FACTOR*(B(INDEX+1,INDEX+8)+B(INDEX+3,INDEX+6)
     1  + B(INDEX+2,INDEX+7) -1.D0/DSQRT(3.D0)*B(INDEX+2,INDEX+4))
      DMPD(3) = DMPD(3)-FACTOR*(B(INDEX+1,INDEX+5)+B(INDEX+2,INDEX+6)
     1  + 2.D0 /DSQRT(3.D0)*B(INDEX+3,INDEX+4))
      GO   TO   270
230   INDEX = LLIM(J)
      DO 240 K = 1,3
240   DMSP(K) = DMSP(K) - B(INDEX,INDEX+K) * 7.33697D0/
     1  (0.325D0*DFLOAT(AN(J)-1))
270   DO 280 I=1,3
280   DM(I) = DM(I) + (DFLOAT(CZ(J))-XXX(J)) * C(J,I) * 2.5416D0
290   CONTINUE
      DO 300 I=1,3
300   DPM(I) = DM(I)+DMSP(I)+DMPD(I)
      WRITE(6,310)
      WRITE(2,310)
310   FORMAT(//20X,16H  DIPOLE MOMENTS,/)
      WRITE(6,320)
      WRITE(2,320)
320   FORMAT(5X,11H COMPONENTS,3X,2H X,8X,2H Y,8X,2H Z)
      WRITE(6,330) DM(1),DM(2),DM(3)
      WRITE(2,330) DM(1),DM(2),DM(3)
330   FORMAT(5X,10H DENSITIES,3(1X,F9.5))
      WRITE(6,340) DMSP(1),DMSP(2),DMSP(3)
      WRITE(2,340) DMSP(1),DMSP(2),DMSP(3)
340   FORMAT(5X,4H S.P,6X,3(1X,F9.5))
      WRITE(6,350) DMPD(1),DMPD(2),DMPD(3)
      WRITE(2,350) DMPD(1),DMPD(2),DMPD(3)
350   FORMAT(5X,4H P.D,6X,3(1X,F9.5))
      WRITE(6,360) DPM(1),DPM(2),DPM(3)
      WRITE(2,360) DPM(1),DPM(2),DPM(3)
360   FORMAT(5X,6H TOTAL,4X,3(1X,F9.5),/)
      DP = DSQRT(DPM(1)**2+DPM(2)**2+DPM(3)**2)
      WRITE(6,370) DP
      WRITE(2,370) DP
370   FORMAT(15H DIPOLE MOMENT=,F9.5,7H DEBYES,//)
380   RETURN
      END

      SUBROUTINE EIGN(NN,RHO)
      IMPLICIT REAL*8(A-H,O-Z)
C     RHO = UPPER LIMIT FOR OFF-DIAGONAL ELEMENT
C     NN = SIZE OF MATRIX
C     A = F MITRIX (ONLY LOWER TRIANGLE IS USED + THIS IS DESTROYED)
C     EIG = RETURNED EIGENVALUES IN ALGEBRAIC ASCENDING ORDER
C     VEC = RETURNED EIGENVECTORS lN COLUMNS
      COMMON/ARRAYS/A(80,80),VEC(80,80),X(80,80)
      COMMON/GAB/GAMMA(80),BETA(80),BETASQ(80),EIG(80),W(80),XYZ(1600)
C     THE FOLLOWING DIMENSIONED VARIABLES ARE EOUIVLENCED
      DIMENSION P(80),Q(80)
      EQUIVALENCE (P(1),BETA(1)),(Q(1),BETA(1))
      DIMENSION IPOSV(80),IVPOS(80),IORD(80)
      EQUIVALENCE(IPOSV(1),GAMMA(1)),(IVPOS(1),BETA(1)),
     1(IORD(1),BETASQ(1))
      RHOSQ = RHO * RHO
      N = NN
      IF (N.EQ.0) GO TO 640
10    N1 = N -1
      N2 = N -2
      GAMMA(1)=A(1,1)
      IF(N2)200,190,40
40    DO 180 NR=1,N2
      B = A(NR+1,NR)
      S = 0.D0
      DO 50  I=NR,N2
50    S = S+A(I+2,NR)**2
C     PREPARE FOR POSSIBLE BYPASS  OF TRANSFORMATION
      A(NR+1,NR) = 0.D0
      IF(S) 170,170,60
60    S = S + B * B
      SGN = 1.D0
      IF(B) 70,80,80
70    SGN = -1.D0
80    SQRTS = DSQRT(S)
      D = SGN/(SQRTS+SQRTS)
      TEMP = DSQRT(.5D0+B*D)
      W(NR) = TEMP
      A(NR+1,NR) = TEMP
      D = D/TEMP
      B = -SGN * SQRTS
C     D IS FACTOR or PROPONALTIY. NOT COMPUTE ANO SAVE Q VECTOR.
C     EXTRA SINGLY SUBSCRIPTED W VECTOR USED FOR SPEED.
      DO 90 I=NR,N2
      TEMP = D*A(I+2,NR)
      W(I+1) = TEMP
90    A(I+2,NR) = TEMP
C     PREMULTIPLY VECTOR W BY MATRIX A TO OBTAIN P VECTOR.
C     SIMULTANEOUSLY ACCUMULATE DOT PRODUCT WP,(THE SCALAR K)
      WTAW=0.D0
      DO 140 I=NR,N1
      SUM = 0.D0
      DO 100 J=NR,I
100   SUM = SUM+A(I+1,J+1)*W(J)
      I1 = I+1
      IF(N1-I1) 130,110,110
110   DO 120 J=I1,N1
120   SUM = SUM + A(J+1,I+1)* W(J)
130   P(I) = SUM
140   WTAW = WTAW+SUM*W(I)
C     P VECTOR AND SCALAR K NOW STORED. NEXT COMPUTE Q VECTOR
      DO 150 I=NR,N1
150   Q(I) = P(I)-WTAW*W(I)
C     NOW FORM PAP MATRIX, REOUIRED PART
      DO 160 J=NR,N1
      QJ = Q(J)
      WJ = W(J)
      DO 160 I=J,N1
160   A(I+1,J+1) = A(I+1,J+1)-2.D0*(W(I)*QJ+WJ*Q(I))
170   BETA(NR) = B
      BETASQ(NR) = B * B
180   GAMMA(NR+1) = A(NR+1,NR+1)
190   B = A(N,N-1)
      BETA(N-1) = B
      BETASQ(N-1) = B * B
      GAMMA(N) = A(N,N)
200   BETASQ(N) = 0.D0
C     ADJOIN AN IDENTITY MATRIX T0 BE POSTMULTIPED BY ROTATIONS.
      DO 220 I=1,N
      DO 210 J=1,N
210   VEC(I,J) = 0.D0
220   VEC(I,I) = 1.D0
      M = N
      SUM = 0.D0
      NPAS = 1
      GO TO 350
230   SUM = SUM + SHIFT
      COSA = 1.D0
      G = GAMMA(1)- SHIFT
      PP = G
      PPBS = PP*PP+BETASQ(1)
      PPBR = DSQRT(PPBS)
      DO 320 J=1,M
      COSAP = COSA
      IF(PPBS.GT.1.D-12) GO TO 250
240   SINA = 0.D0
      SINA2 = 0.D0
      COSA = 1.D0
      GO TO 290
250   SINA = BETA(J)/PPBR
      SINA2 = BETASQ(J)/PPBS
      COSA = PP/PPBR
C     POSTMLLTIPLY IDENTITY BY P-TRANSPOSE MATRIX
      NT = J + NPAS
      IF(NT.LT.N) GO TO 270
260   NT = N
270   DO 280 I=1,NT
      TEMP = COSA*VEC(I,J)+SINA*VEC(I,J+1)
      VEC(I,J+1) = -SINA*VEC(I,J)+COSA*VEC(I,J+1)
280   VEC(I,J) = TEMP
290   DIA = GAMMA(J+1)-SHIFT
      U = SINA2*(G+DIA)
      GAMMA(J) = G+U
      G = DIA-U
      PP = DIA*COSA- SINA* COSAP* BETA(J)
      IF(J.NE.M) GO TO 310
300   BETA(J) = SINA*PP
      BETASQ(J) = SINA2*PP*PP
      GO TO 330
310   PPBS = PP * PP + BETASQ(J+1)
      PPBR = DSQRT( PPBS )
      BETA(J) = SINA*PPBR
320   BETASQ(J) = SINA2*PPBS
330   GAMMA(M+1) = G
C     TEST FOR CONVFRGENCE OF LAST DIAGONAL ELEMENT
      NPAS = NPAS + 1
      IF(BETASQ(M).GT.RHOSQ) GO TO 370
340   EIG(M+1) = GAMMA(M+1)+SUM
350   BETA(M) = 0.D0
      BETASQ(M) = 0.D0
      M = M-1
      IF(M.EQ.0) GO TO 400
360   IF(BETASQ(M).LE.RHOSQ) GO TO 340
C     TAKE ROOT OF CORNER 2 BY 2 NEAREST T0 LOWER DIAGONAL IN VALUE
C     AS ESTIMATE OF EIGENVALUE TO USE FOR SHIFT
370   A2 = GAMMA(M+1)
      R2 = 0.5D0 * A2
      R1 = 0.5D0 * GAMMA(M)
      R12 = R1 + R2
      DIF = R1 - R2
      TEMP = DSQRT(DIF * DIF + BETASQ(M))
      R1 = R12 + TEMP
      R2 = R12 - TEMP
      DIF = DABS(A2 - R1) - DABS(A2 - R2)
      IF(DIF.LT.0.D0) GO TO 390
380   SHIFT = R2
      GO TO 230
390   SHIFT = R1
      GO TO 230
400   EIG(1) = GAMMA(1) + SUM
C     INITIALIZE AUXILARY TABLES REQUIRED FOR REARING THE VECTORS
      DO 410 J = 1,N
      IPOSV(J) = J
      IVPOS(J) = J
410   IORD(J) = J
C     TRANSPOSITION SORT T0 ORDER TME EIGENVALUES
      M = N
      GO TO 450
420   DO 440 J=1,M
      IF(EIG(J).LE.EIG(J+1)) GO TO 440
430   TEMP = EIG(J)
      EIG(J) = EIG(J+1)
      EIG(J+1) = TEMP
      ITEMP = IORD(J)
      IORD(J) = IORD(J+1)
      IORD(J+1) = ITEMP
440   CONTINUE
450   M = M-1
      IF(M.NE.0) GO TO 420
460   IF(N1.EQ.0) GO TO 510
470   DO 500 L=1,N1
      NV = IORD(L)
      NP = IPOSV(NV)
      IF (NP.EQ.L) GO TO 500
480   LV = IVPOS(L)
      IVPOS(NP) = LV
      IPOSV(LV) = NP
      DO 490 I=1,N
      TEMP = VEC(I,L)
      VEC(I,L) = VEC(I,NP)
490   VEC(I,NP) = TEMP
500   CONTINUE
510   CONTINUE
C     BACK TRANSFORM THE VECTORS OF THE TRIPLE  DIAGONAL MATRIX
      DO 570 NRR=1,N
      K = N1
520   K = K -1
      IF(K.LE.0)GO TO 560
530   SUM  = 0.D0
      DO 540 I = K,N1
540   SUM = SUM + VEC(I+1,NRR)*A(I+1,K)
      SUM = SUM+ SUM
      DO 550 I=K,N1
550   VEC(I+1,NRR) = VEC(I+1,NRR)-SUM*A(I+1,K)
      GO TO 520
560   CONTINUE
570   CONTINUE
640   RETURN
      END

      SUBROUTINE SCFOUT(OP,MOP)
      IMPLICIT REAL*8(A-H,O-Z)
C     THIS ROUTINE PRINTS THE ARRAY IN COMMON/ARRAYS/ WHICH IS DESIGNATE
C     MOP. IF OP = 1  THE EIGENVALUES CONTAINED IN COMMON/1/ ARE ALSO
C     PRINTED. IF OP = 0 THE EIGENVALUES ARE NOT PRINTED
      COMMON/ARRAYS/A(80,80,3)
      COMMON/QAB/XXX(2000)
      COMMON/INFO/NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON/INFO1/CZ(35),U(80),ULIM(35),LLIM(35),NELECS,OCCA,OCCB
      COMMON/ORB/ORB(9)
      COMMON/PERTBL/EL(18)
      INTEGER OP,CHARGE,AN,ANII,OBB,CZ,U,ULIM,OCCA,OCCB
      DO 120 M=1,N,11
      K = M + 10
      IF(K.LE.N) GO TO 30
20    K = N
30    CONTINUE
      IF(OP.EQ.1) GO TO 40
      GO TO 50
40    CALL EIGOUT(M,K)
50    CONTINUE
      WRITE(6,60) (I,I = M,K)
      WRITE(2,60) (I,I = M,K)
60    FORMAT(13X,50I9)
      DO 110 I = 1,N
      II = U(I)
      ANII = AN(II)
      L = I - LLIM(II) + 1
70    WRITE(6,80) I,II,EL(ANII),ORB(L),(A(I,J,MOP),J = M,K)
      WRITE(2,80) I,II,EL(ANII),ORB(L),(A(I,J,MOP),J = M,K)
80    FORMAT(1X,I2,I3,A4,1X,A4,50(F9.4))
      IF (I.EQ.ULIM(II)) GO TO 90
      GO TO 110
90    WRITE(6,100)
      WRITE(2,100)
100   FORMAT(1X)
110   CONTINUE
120   CONTINUE
      WRITE(6,100)
      WRITE(6,100)
      RETURN
      END

      SUBROUTINE EIGOUT(M,K)
      IMPLICIT REAL*8 (A-H,O-Z)
C     THIS ROUTINE IS CALLED IN SCFOUT TO PRINT THE EIGENVALUES M TO K
      COMMON/GAB/XXX(240),EPSILN(80),YYY(1680)
      WRITE(6,10) (EPSILN(I), I = M,K)
      WRITE(2,10) (EPSILN(I), I = M,K)
10    FORMAT(/,16H  EIGENVALUES---,20(F9.4),/)
      RETURN
      END
