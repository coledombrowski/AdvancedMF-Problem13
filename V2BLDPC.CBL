       IDENTIFICATION DIVISION.
       PROGRAM-ID. V2BLDPC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT MASTER-INPUT-FILE  ASSIGN TO 'V2MASTSQ.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MESSAGE-REPORT-FILE  ASSIGN TO 'SALECNT2.DOC'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MASTER-FILE-INDEXED ASSIGN TO 'V2MASTVS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS MASTER-SSN-INDEXED.

       DATA DIVISION.
       FILE SECTION.

       FD  MASTER-INPUT-FILE
           LABEL RECORDS ARE STANDARD.

       01  MASTER-REC-IN.
           05  MASTER-SSN-IN                 PIC X(9).
           05  MARITIAL-STATUS-IN            PIC X.
           05  EXEMPTIONS-IN                 PIC XX.
           05  NAME-IN.
               10 F-INIT-IN                  PIC X.
               10 M-INIT-IN                  PIC X.
               10 LAST-NAME-IN               PIC X(20).
           05  GROSS-IN                      PIC S9(7)V99.
           05  FICA-IN                       PIC S9(4)V99.
           05  WH-IN                         PIC S9(5)V99.
           05  MOVE-IN                       PIC S9(5)V99.
           05  FILLER                        PIC X(17).

       FD  MESSAGE-REPORT-FILE.
       01  MESSAGE-LINE-OUT                  PIC X(80).

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

       WORKING-STORAGE SECTION.
       01  WORKING-FIELDS.
           05  EOF-MASTSEQ-WS             PIC X(3)     VALUE 'NO '.
           05  RECORD-COUNT-WS            PIC S9(5)    VALUE ZERO.

       01  SUMMARY-LINE.
               05                              PIC X(14) VALUE SPACES.
               05                              PIC X(42) VALUE
                     "TOTAL NUMBER OF INDEXED RECORDS CREATED = ".
               05  RECORD-COUNT-OUT            PIC ZZ9.
               05                              PIC X(21) VALUE SPACES.

       PROCEDURE DIVISION.

       100-MAINLINE.
           PERFORM 200-OPEN.
           PERFORM 300-PROCESS UNTIL EOF-MASTSEQ-WS = 'YES'.
           PERFORM 900-CLOSE.
           STOP RUN.

       200-OPEN.
           OPEN INPUT MASTER-INPUT-FILE
                OUTPUT  MASTER-FILE-INDEXED
                        MESSAGE-REPORT-FILE
           PERFORM 250-READ.

       250-READ.
           READ MASTER-INPUT-FILE
                AT END MOVE 'YES' TO EOF-MASTSEQ-WS
           END-READ.

       300-PROCESS.
           WRITE MASTER-REC-INDEXED FROM MASTER-REC-IN
               INVALID KEY DISPLAY 'PROCESSING SAME RECORD TWICE'
                           DISPLAY 'NUM WAS = ' MASTER-SSN-INDEXED.


           ADD 1 TO RECORD-COUNT-WS.

           PERFORM 250-READ.


       900-CLOSE.

           MOVE RECORD-COUNT-WS TO RECORD-COUNT-OUT.

           WRITE MESSAGE-LINE-OUT FROM SUMMARY-LINE
               AFTER ADVANCING 3.

           CLOSE MASTER-INPUT-FILE
                 MASTER-FILE-INDEXED
                 MESSAGE-REPORT-FILE.

