C
C     Read MOPAC style input file.
C
      Subroutine ReadIn
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /PERTBL/EL(18)
      COMMON /INFO/ NATOMS,CHARGE,MULTIP,AN(35),C(35,3),N
      COMMON /OPTION/ OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      COMMON /KEYWRD/ KEYWRD

      INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER AN,CHARGE,CZ,U,ULIM,OCCA,OCCB
      CHARACTER KEYWRD*241, ELEM*4, EL*4
      CHARACTER LINE*80, SPACE*1, NINE*1, ZERO*1, COMMA*1
      DIMENSION VALUE(40)
      Integer AtoI
      LOGICAL LEADSP
      DATA COMMA,SPACE,NINE,ZERO/',',' ','9','0'/

      Read(*,'(A80)') KEYWRD
      Read(*,'(A80)') LINE ! title comment
      Read(*,'(A80)') LINE ! title comment

      Call UpCase(KEYWRD)

      If (INDEX(KEYWRD,'CNDO').NE.0) OPTION = CNDO
      If (INDEX(KEYWRD,'INDO').NE.0) OPTION = INDO
      If (INDEX(KEYWRD,'OPEN').NE.0) OPNCLO = 0
      If (INDEX(KEYWRD,'CLOSED').NE.0) OPNCLO = 1
      If (INDEX(KEYWRD,'CHARGE=').NE.0) THEN
          CHARGE = AtoI(KEYWRD(INDEX(KEYWRD,'CHARGE=')+7:))
      End If
      IF(INDEX(KEYWRD,'MULTIP=').NE.0) THEN
          MULTIP = AtoI(KEYWRD(INDEX(KEYWRD,'MULTIP=')+7:))
      End If

      ILOWA = IChar('a')
      ILOWZ = IChar('z')
      ICAPA = IChar('A')
      ICAPZ = IChar('Z')

      NATOMS = 0
      Do J = 1, 80
          READ(5,'(A)') LINE
          LEADSP = .TRUE.
          IFOUND = 0
          Do I = 1, 80
              If (LEADSP.AND.LINE(I:I).NE.SPACE) THEN
                  ! found atom name
                  ILINE = IChar(LINE(I:I))
                  If (ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
                      LINE(I:I) = Char(ILINE+ICAPA-ILOWA)
                  End If
                  ILINE = IChar(LINE(I+1:I+1))
                  If (ILINE.GE.ICAPA.AND.ILINE.LE.ICAPZ) THEN
                      LINE(I+1:I+1) = Char(ILINE-ICAPA+ILOWA)
                  End If

                  ELEM = LINE(I:I+1)
                  ! get coordinates
                  READ(LINE(I+2:),*,ERR=100) (VALUE(K),K=1,3)
                  Do K = 1, 18
                      IF(ELEM.EQ.EL(K)) THEN
                         AN(J) = K
                         IFOUND = 1
                         C(J,1) = VALUE(1)/0.529177D0
                         C(J,2) = VALUE(2)/0.529177D0
                         C(J,3) = VALUE(3)/0.529177D0
                         NATOMS = J
                         Goto 90
                      End If
                  End Do
                  Goto 90
              End If
          End Do
   90     Continue
          If (IFOUND.EQ.0) Goto 100
      End Do

  100 Continue

      If (NATOMS.EQ.0) STOP
      Write(6,60) NATOMS,CHARGE,MULTIP
      Do I=1,NATOMS
          Write(6,70) AN(I),C(I,1)*0.529177D0,C(I,2)*0.529177D0,
     + C(I,3)*0.529177D0
          Write(2,70) AN(I),C(I,1)*0.529177D0,C(I,2)*0.529177D0,
     + C(I,3)*0.529177D0
      End Do
60    Format(/5X,I4,16H ATOMS CHARGE = ,I4,17H  MULTIPLICITY = ,I4/)
70    Format(I4,3(3X,F12.7))
      END

      Integer Function AtoI(A)
      Character*80 A
      Integer I, J, K
      Integer Zero, Nine
      Logical IsNumber
      Zero = IChar('0')
      Nine = IChar('9')
      IsNumber = .FALSE.
      J = 0
      Do I = 1, 80
          K = IChar(A(I:I))
          If (K.GE.Zero.AND.K.LE.Nine) Then
              J = J * 10 + K - Zero
              IsNumber = .True.
          Else If (IsNumber) Then
              Goto 100
          End If
      End Do

 100  AtoI = J
      Return
      End

      Subroutine UpCase(KeyWrd)
      Character*80 KeyWrd
      ICapA = IChar('A')
      ILowA = IChar('a')
      ILowZ = IChar('z')
      Do I=1, 80
         ILine = IChar(KeyWrd(I:I))
         If (ILine.GE.ILowA.AND.ILine.LE.ILowZ) Then
            KeyWrd(I:I) = Char(ILine + ICapA - ILowA)
         EndIf
      End Do
      Return
      End
