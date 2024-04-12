       IDENTIFICATION DIVISION.
       PROGRAM-ID. V2LIST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT MASTER-FILE-INDEXED ASSIGN TO 'V2MASTVS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS MASTER-SSN-INDEXED.

            SELECT SALESMAN-REPORT  ASSIGN TO 'SALESTP4.DOC'
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.

       FD  MASTER-FILE-INDEXED.

       01  MASTER-REC-INDEXED.
                  05  MASTER-SSN-INDEXED            PIC X(9).
                  05  MARITIAL-STATUS-INDEXED       PIC X.
                  05  EXEMPTIONS-INDEXED            PIC XX.
                  05  NAME-INDEXED.
                      10 F-INIT-INDEXED             PIC X.
                      10 M-INIT-INDEXED             PIC X.
                      10 LAST-NAME-INDEXED          PIC X(20).
                  05  GROSS-INDEXED                 PIC S9(7)V99.
                  05  FICA-INDEXED                  PIC S9(4)V99.
                  05  WH-INDEXED                    PIC S9(5)V99.
                  05  MOVE-INDEXED                  PIC S9(5)V99.
                  05  FILLER                        PIC X(17).

       FD  SALESMAN-REPORT RECORDING MODE IS F.
       01  REPORT-LINE-OUT                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORKING-FIELDS.
           05  EOF-MASTER-WS              PIC X(3)  VALUE 'NO '.
       01  ACCUMULATORS.
           05  AC-LINE-COUNT              PIC S999  VALUE 0.
           05  AC-PAGE-COUNT              PIC S999  VALUE 0.
           05  AC-RECORD-COUNT            PIC S999  VALUE 0.

       01  REPORT-HEADING.
           02  RH-LINE-1.
               03                              PIC X(27)  VALUE
                   "TAX   REPORT".
               03                              PIC X(18) VALUE SPACES.
               03                              PIC X(6)  VALUE 'PAGE:'.
               03  RH-PAGE-COUNT               PIC ZZ9.
               03                              PIC X(26) VALUE SPACES.

       01  DETAIL-LINE.
           05  FILLER                         PIC X VALUE SPACES.
           05  MASTER-SSN-OUT                 PIC X(9).
           05  FILLER                         PIC X VALUE SPACES.
           05  MARITIAL-STATUS-OUT            PIC X.
           05  FILLER                         PIC X VALUE SPACES.
           05  EXEMPTIONS-OUT                 PIC XX.
           05  FILLER                         PIC X VALUE SPACES.
           05  NAME-OUT.
               10 F-INIT-OUT                  PIC X.

               10 M-INIT-OUT                  PIC X.
               10 LAST-NAME-OUT               PIC X(20).
           05  FILLER                         PIC X VALUE SPACES.
           05  GROSS-OUT                      PIC S9(7)V99.
           05  FILLER                         PIC X VALUE SPACES.
           05  FICA-OUT                       PIC S9(4)V99.
           05  FILLER                         PIC X VALUE SPACES.
           05  WH-OUT                         PIC S9(5)V99.
           05  FILLER                         PIC X VALUE SPACES.
           05  MOVE-OUT                       PIC S9(5)V99.
           05  FILLER                         PIC X(17).

       01  SUMMARY-LINES.
           02  SL-LINE-1.
               03                              PIC X(26)  VALUE SPACES.
               03                              PIC X(25)  VALUE
                   "TOTAL RECORDS PRINTED =  ".
               03  SL-RECORD-COUNT             PIC ZZ9.
               03                              PIC X(26)  VALUE SPACES.

           02  SL-EOR-LINE.
               03                              PIC X(32)  VALUE SPACES.
               03                              PIC X(13)  VALUE
                   "END OF REPORT".
               03                              PIC X(35)  VALUE SPACES.


       PROCEDURE DIVISION.

       100-MAIN-MODULE.
           PERFORM 200-OPEN.
           PERFORM 300-PROCESS
               UNTIL EOF-MASTER-WS = 'YES'.
           PERFORM 600-WRAPUP.
           PERFORM 900-CLOSE.
           STOP RUN.

       200-OPEN.
           INITIALIZE ACCUMULATORS.

           OPEN INPUT MASTER-FILE-INDEXED
                OUTPUT  SALESMAN-REPORT.

           PERFORM 250-READ.

       250-READ.
           READ MASTER-FILE-INDEXED
               AT END MOVE 'YES' TO EOF-MASTER-WS
           END-READ.

       300-PROCESS.

           IF AC-LINE-COUNT = 0
               PERFORM 500-HEADER
           END-IF

               MOVE MASTER-SSN-INDEXED TO MASTER-SSN-OUT
               MOVE MARITIAL-STATUS-INDEXED TO MARITIAL-STATUS-OUT
               MOVE EXEMPTIONS-INDEXED TO EXEMPTIONS-OUT
               MOVE NAME-INDEXED TO NAME-OUT
               MOVE F-INIT-INDEXED TO F-INIT-OUT
               MOVE M-INIT-INDEXED TO M-INIT-OUT
               MOVE LAST-NAME-INDEXED TO LAST-NAME-OUT
               MOVE GROSS-INDEXED TO GROSS-OUT
               MOVE FICA-INDEXED TO FICA-OUT
               MOVE WH-INDEXED TO WH-OUT
               MOVE MOVE-INDEXED TO MOVE-OUT

               WRITE REPORT-LINE-OUT FROM DETAIL-LINE
                       AFTER ADVANCING 2 LINES

               ADD 2 TO AC-LINE-COUNT
               ADD 1 TO AC-RECORD-COUNT


           IF AC-LINE-COUNT > 30
                       MOVE ZERO TO AC-LINE-COUNT
           END-IF

           PERFORM 250-READ.

       500-HEADER.

           ADD 1 TO AC-PAGE-COUNT.
           MOVE AC-PAGE-COUNT TO RH-PAGE-COUNT.

           WRITE REPORT-LINE-OUT FROM RH-LINE-1
               AFTER PAGE.

           MOVE SPACES TO REPORT-LINE-OUT
           WRITE REPORT-LINE-OUT AFTER ADVANCING 2 LINES.

           MOVE 6 TO AC-LINE-COUNT.

       600-WRAPUP.

           MOVE AC-RECORD-COUNT TO SL-RECORD-COUNT.

           WRITE REPORT-LINE-OUT FROM SL-LINE-1
               AFTER ADVANCING 2 LINES.

           WRITE REPORT-LINE-OUT FROM SL-EOR-LINE
               AFTER ADVANCING 2 LINES.


       900-CLOSE.
           CLOSE MASTER-FILE-INDEXED
                 SALESMAN-REPORT.

