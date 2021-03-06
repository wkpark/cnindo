C
C     Read MOPAC style input file.
C
      Subroutine ReadIn
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NUMATM=140, MAXORB=350, MAXELEM=29)
      COMMON /PERTBL/EL(MAXELEM)
      COMMON /INFO/ NATOMS,CHARGE,MULTIP,AN(NUMATM),N,C(NUMATM,3)
      COMMON /OPTION/ OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      COMMON /KEYWRD/ KEYWRD

      INTEGER OPTION,OPNCLO,HUCKEL,CNDO,INDO,CLOSED,OPEN
      INTEGER AN,CHARGE,CZ,U,ULIM,OCCA,OCCB
      CHARACTER KEYWRD*241, ELEM*4, EL*4
      CHARACTER LINE*80, TITLE*80, Comment*80, SPACE*1
      DIMENSION VALUE(40)
      Integer AtoI
      LOGICAL LEADSP
      Character*4 CTYPE(2)
      Character*6 COPNCLO(2)
      DATA CTYPE/'CNDO','INDO'/
      DATA COPNCLO/'OPEN  ','CLOSED'/
      DATA SPACE/' '/

      Read(*,'(A80)') KEYWRD
      Read(*,'(A80)') TITLE ! title
      Read(*,'(A80)') Comment ! comment

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
          READ(5,'(A)', End=100, Err=100) LINE
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
                  Do K = 1, MAXELEM
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
      Write(6,'(1X,A)') KEYWRD(1:80)
      Write(6,'(1X,A)') TITLE
      Write(6,'(1X,A)') Comment
      Write(6,50) CTYPE(OPTION+1), COPNCLO(OPNCLO+1)
      Write(6,60) NATOMS,CHARGE,MULTIP
      Do I=1,NATOMS
          Write(6,70) EL(AN(I)),C(I,1)*0.529177D0,C(I,2)*0.529177D0,
     + C(I,3)*0.529177D0
      End Do
      Write(6,*)
   50 Format(/5X,A4,1X,A6)
   60 Format(5X,I4,16H ATOMS CHARGE = ,I4,17H  MULTIPLICITY = ,I4/)
   70 Format(2X,A2,3(3X,F12.7))
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
