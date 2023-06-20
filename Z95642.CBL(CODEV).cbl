       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CODEV
       AUTHOR.        Burak Kozluca
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE
                             STATUS    PRT-ST.
           SELECT ACCT-REC   ASSIGN TO ACCTREC
                             STATUS    ACC-ST.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05 PRT-NO         PIC 9(04).
           05 PRT-NAME       PIC X(15).
           05 PRT-SURNAME    PIC X(15).
           05 PRT-BDATE      PIC 9(08).
           05 PRT-TODAY      PIC 9(08).
           05 PRT-DIFF       PIC 9(04).

       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05 ACCT-NO        PIC 9(04).
           05 ACCT-NAME      PIC X(15).
           05 ACCT-SURNAME   PIC X(15).
           05 ACCT-BDATE     PIC 9(08).
           05 ACCT-TODAY     PIC 9(08).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 PRT-ST   PIC 9(02).
              88 PRT-SUCCESS VALUE 00 97.
           05 ACC-ST   PIC 9(02).
              88 ACCT-EOF     VALUE 10.
              88 ACCT-SUCCESS VALUE 00 97.
           05 WS-INT-D PIC 9(07).
           05 WS-INT-T PIC 9(07).

      *------------------
       PROCEDURE DIVISION.
      *------------------
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS UNTIL ACCT-EOF.
           PERFORM H999-PROGRAM-EXIT.
      *
       H100-OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
           READ ACCT-REC.
       H100-END.EXIT.
      *
       H200-PROCESS.
           COMPUTE WS-INT-D = FUNCTION INTEGER-OF-DATE(ACCT-BDATE).
           COMPUTE WS-INT-T = FUNCTION INTEGER-OF-DATE(ACCT-TODAY).
           DISPLAY PRT-NAME.
           DISPLAY PRT-BDATE.
           INITIALIZE PRINT-REC.
           MOVE ACCT-NO TO PRT-NO.
           MOVE ACCT-NAME TO PRT-NAME.
           MOVE ACCT-SURNAME TO PRT-SURNAME.
           MOVE ACCT-BDATE TO PRT-BDATE.
           MOVE ACCT-TODAY TO PRT-TODAY.
           COMPUTE PRT-DIFF = WS-INT-T - WS-INT-D.
           WRITE PRINT-REC.
           READ ACCT-REC.
       H200-END.EXIT.
      *
       H300-CLOSE-FILES.
           CLOSE ACCT-REC
                 PRINT-LINE.
       H300-END.EXIT.
      *
       H999-PROGRAM-EXIT.
           PERFORM H300-CLOSE-FILES.
           STOP RUN.
       H999-END.EXIT.
      *
