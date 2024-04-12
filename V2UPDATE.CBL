       IDENTIFICATION DIVISION.
       PROGRAM-ID.  V2UPDATE.

       ENVIRONMENT  DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT MASTER-FILE-INDEXED ASSIGN TO 'V2MASTVS.DAT'
                 ORGANIZATION IS INDEXED
                 ACCESS IS RANDOM
                 RECORD KEY IS MASTER-SSN-INDEXED.

             SELECT TRANS-FILE-IN  ASSIGN TO 'V2TRANIN.DAT'
                ORGANIZATION IS LINE SEQUENTIAL.

               SELECT TRANSACTION-LIST-REPORT
                 ASSIGN TO "TRANSOUT.DOC"
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  MASTER-FILE-INDEXED
           LABEL RECORDS ARE STANDARD.

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

       FD  TRANS-FILE-IN
           LABEL RECORDS ARE STANDARD.

       01  TRANS-REC-IN.
                  05  TRANS-SSN-IN              PIC X(9).
                  05  MARITIAL-STATUS-IN        PIC X.
                  05  EXEMPTIONS-IN             PIC XX.
                  05  NAME-IN.
                      10 F-INIT-IN              PIC X.
                      10 M-INIT-IN              PIC X.
                      10 LAST-NAME-IN           PIC X(20).
                  05  GROSS-IN                  PIC S9(7)V99.
                  05  FICA-IN                   PIC S9(4)V99.
                  05  WH-IN                     PIC S9(5)V99.
                  05  MOVE-IN                   PIC S9(5)V99.


                  05  TRANS-CODE-IN             PIC X.
                      88  EMPLOYEE-HIRED              VALUE '1'.
                      88  CONTINUING-EMPLOYEE         VALUE '2'.
                      88  EMPLOYEE-TERMINATED         VALUE '3'.
                05  FILLER              PIC X(41).

       FD  TRANSACTION-LIST-REPORT RECORDING MODE IS F.
       01  REPORT-LINE-OUT                     PIC X(132).


       WORKING-STORAGE SECTION.
       01  WORKING-FIELDS.
           05  EOF-TRANS-WS                    PIC X(3) VALUE 'NO '.


       01  TRANSACTION-OUTPUT-LINE.
           02                                  PIC X VALUE SPACE.
           02  TEL-RECORD                      PIC X(80).
           02                                  PIC X VALUE SPACE.
           02  TEL-MESSAGE                     PIC X(50).

       PROCEDURE DIVISION.

       100-MAIN-MODULE.
           PERFORM 200-OPEN.
           PERFORM 300-PROCESS
               UNTIL EOF-TRANS-WS = 'YES'.
           PERFORM 900-CLOSE.
           STOP RUN.

       200-OPEN.


           OPEN INPUT TRANS-FILE-IN
                I-O   MASTER-FILE-INDEXED
                OUTPUT TRANSACTION-LIST-REPORT.

           READ TRANS-FILE-IN
               AT END MOVE 'YES' TO EOF-TRANS-WS
           END-READ.

       300-PROCESS.


           EVALUATE TRUE
               WHEN EMPLOYEE-HIRED
                   PERFORM 400-NEW-RTN

               WHEN EMPLOYEE-TERMINATED
                   PERFORM 500-DELETE-RTN

               WHEN CONTINUING-EMPLOYEE
                   PERFORM 600-UPDATE-RTN

               WHEN OTHER
                   MOVE "BAD CODE ENTERED"
                   TO TEL-MESSAGE
                   PERFORM 700-PRINT-ERROR

            END-EVALUATE.



           READ TRANS-FILE-IN
               AT END MOVE 'YES' TO EOF-TRANS-WS
           END-READ.


       400-NEW-RTN.

            MOVE TRANS-SSN-IN TO MASTER-SSN-INDEXED
            MOVE MARITIAL-STATUS-IN TO MARITIAL-STATUS-INDEXED
            MOVE EXEMPTIONS-IN TO EXEMPTIONS-INDEXED
            MOVE NAME-IN TO NAME-INDEXED
            MOVE F-INIT-IN TO F-INIT-INDEXED
            MOVE M-INIT-IN TO M-INIT-INDEXED
            MOVE LAST-NAME-IN TO LAST-NAME-INDEXED
            MOVE GROSS-IN TO GROSS-INDEXED
            MOVE FICA-IN TO FICA-INDEXED
            MOVE WH-IN TO WH-INDEXED
            MOVE MOVE-IN TO MOVE-INDEXED

            WRITE MASTER-REC-INDEXED
               INVALID KEY
                   MOVE "ATTEMPTED ADDITION OF EXISTING RECORD"
                   TO  TEL-MESSAGE
                   PERFORM 700-PRINT-ERROR

               NOT INVALID KEY
                   MOVE "SUCESSFUL ADDITION"
                   TO TEL-MESSAGE
                   PERFORM 700-PRINT-ERROR

           END-WRITE.


       500-DELETE-RTN.
           MOVE TRANS-SSN-IN TO MASTER-SSN-INDEXED.
           READ MASTER-FILE-INDEXED
                INVALID KEY
                    MOVE "ATTEMPTED DELETION OF NON-EXISTING RECORD"
                    TO  TEL-MESSAGE
                    PERFORM 700-PRINT-ERROR

                 NOT INVALID KEY
                     DELETE MASTER-FILE-INDEXED

                         INVALID KEY
                             MOVE "BAD DELETION LOGIC"
                             TO TEL-MESSAGE
                             PERFORM 700-PRINT-ERROR

                         NOT INVALID KEY
                             MOVE "SUCCESSFUL DELETION"
                             TO TEL-MESSAGE
                             PERFORM 700-PRINT-ERROR

                     END-DELETE
           END-READ.

       600-UPDATE-RTN.

           MOVE TRANS-SSN-IN TO MASTER-SSN-INDEXED.

           READ MASTER-FILE-INDEXED

               INVALID KEY
                 MOVE "ATTEMPTED CHANGE/UPDATE OF NON-EXISTING RECORD"
                  TO TEL-MESSAGE
                   PERFORM 700-PRINT-ERROR

               NOT INVALID KEY PERFORM 800-REWRITE-RTN

           END-READ.



       800-REWRITE-RTN.

           REWRITE MASTER-REC-INDEXED

               INVALID KEY
                   MOVE "REWRITE LOGIC ERROR"
                   TO TEL-MESSAGE
                   PERFORM 700-PRINT-ERROR

                NOT INVALID KEY
                   MOVE "SUCESSFUL CHANGE"
                   TO TEL-MESSAGE
                   PERFORM 700-PRINT-ERROR

           END-REWRITE.

       700-PRINT-ERROR.

           MOVE TRANS-REC-IN TO TEL-RECORD.

           WRITE REPORT-LINE-OUT FROM TRANSACTION-OUTPUT-LINE
                AFTER ADVANCING 2 LINES.



       900-CLOSE.
           CLOSE TRANS-FILE-IN
                 MASTER-FILE-INDEXED
                 TRANSACTION-LIST-REPORT.

