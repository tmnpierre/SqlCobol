       IDENTIFICATION DIVISION.
       PROGRAM-ID. AgeStat.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MAX-AGE               PIC 99 VALUE ZEROS.
       01  MIN-AGE               PIC 99 VALUE 99.
       01  AGE-VALUE             PIC 99 VALUE ZEROS.
       01  AGE-COUNT             PIC 99 VALUE ZEROS.

       01  SQL-STATUS            PIC S9(4) COMP-5 VALUE ZEROS.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME               PIC X(30) VALUE 'country'.
       01  USERNAME             PIC X(30) VALUE 'cobol'.
       01  PASSWD               PIC X(10) VALUE SPACE.

       01  SQL-AGE-RESULT.
           05  MAX-AGE-VALUE    PIC 99.
           05  MIN-AGE-VALUE    PIC 99.

       01  SQL-AGE-COUNT-RESULT.
           05  SQL-AGE-VALUE   PIC 99.
           05  SQL-AGE-COUNT   PIC 99.

       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       1000-MAIN-START.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.

           IF SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.

           PERFORM 3001-GET-MAX-MIN-AGE
               THRU 3001-GET-MAX-MIN-AGE-END.

           PERFORM 4001-GET-AGE-COUNT
               THRU 4001-GET-AGE-COUNT-END.

       1000-MAIN-END.
           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           STOP RUN.
      ******************************************************************
       1001-ERROR-RTN-START.
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE SPACE.
           EVALUATE SQLCODE
              WHEN  +100
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection failed"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "Database error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
              WHEN OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       1001-ERROR-RTN-END.
           STOP RUN.
      ******************************************************************
       3001-GET-MAX-MIN-AGE.
           EXEC SQL
               SELECT MAX(age), MIN(age)
               INTO :MAX-AGE-VALUE, :MIN-AGE-VALUE
               FROM databank
           END-EXEC.

           MOVE MAX-AGE-VALUE TO MAX-AGE.
           MOVE MIN-AGE-VALUE TO MIN-AGE.

           DISPLAY 'Min Age: ' MIN-AGE.
           DISPLAY 'Max Age: ' MAX-AGE.
       3001-GET-MAX-MIN-AGE-END.
      ******************************************************************
       4001-GET-AGE-COUNT.
           DISPLAY 'Age Counts: '.
           EXEC SQL DECLARE AGE_CUR CURSOR FOR
               SELECT age, COUNT(*)
               FROM databank
               GROUP BY age
               ORDER BY age ASC
           END-EXEC.

           EXEC SQL OPEN AGE_CUR END-EXEC.

           PERFORM UNTIL SQLCODE = +100
               EXEC SQL
                   FETCH AGE_CUR
                   INTO :SQL-AGE-VALUE, :SQL-AGE-COUNT
               END-EXEC

               IF SQLCODE = 0 THEN
                   DISPLAY 'Age ' SQL-AGE-VALUE ': ' SQL-AGE-COUNT
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE AGE_CUR END-EXEC.
       4001-GET-AGE-COUNT-END.
      ******************************************************************
