      *-----------------------
      * Copyright Contributors to the COBOL Programming Course
      * SPDX-License-Identifier: CC-BY-4.0
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBLODEV1
       AUTHOR.        Burak Kozluca.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
      *PRINT-LINE file assign to PRTLINE
      *ACCT-REC file assign to ACCTREC
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
      *-------------
       DATA DIVISION.
      *-------------
      *In FILE SECTION, Variable names are defined.
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
      *In PRINT-REC block, output variables are defined.
       01  PRINT-REC.
           05  ACCT-NO-O      PIC X(8).
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
           05  LAST-NAME-O    PIC X(20).
           05  FIRST-NAME-O   PIC X(15).
           05  COMMENTS-O     PIC X(50).
      *
       FD  ACCT-REC RECORDING MODE F.
      *In ACCT-FIELDS block, input variables are defined.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
           05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
           05  LAST-NAME          PIC X(20).
           05  FIRST-NAME         PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR    PIC X(25).
               10  CITY-COUNTY    PIC X(20).
               10  USA-STATE      PIC X(15).
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
      *
      *In WORKING-STORAGE SECTION, checking EOF variable is defined.
       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.
      *------------------
       PROCEDURE DIVISION.
      *------------------
      *Input and output files are opened.
       OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
      *This loop reads records until LASTREC is set to 'Y' and writes each recor
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
           PERFORM UNTIL LASTREC = 'Y'
               PERFORM WRITE-RECORD
               PERFORM READ-RECORD
           END-PERFORM
           .
      *All files are closed, and the program ends with a GOBACK statement.
       CLOSE-STOP.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           GOBACK.
      *Read a record from the ACCT-REC file and check if it's the end of file (E
       READ-RECORD.
           READ ACCT-REC
               AT END MOVE 'Y' TO LASTREC
           END-READ.
      *Write the record data to the PRINT-REC file.
       WRITE-RECORD.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           MOVE FIRST-NAME   TO  FIRST-NAME-O.
           MOVE COMMENTS     TO  COMMENTS-O.
           WRITE PRINT-REC.
      *

