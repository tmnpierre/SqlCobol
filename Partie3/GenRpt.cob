       IDENTIFICATION DIVISION.
       PROGRAM-ID. GenRpt.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MIN-AGE              PIC 99 VALUE ZEROS.
       01  MAX-AGE              PIC 99 VALUE ZEROS.
       01  MEDIAN-AGE           PIC 99 VALUE ZEROS.
       01  COUNTRY              PIC X(50) VALUE SPACES.
       01  CURRENT-COUNTRY      PIC X(50) VALUE SPACES.
       01  GENDER               PIC X(10) VALUE SPACES.
       01  MALE-COUNT           PIC 9999 VALUE ZEROS.
       01  FEMALE-COUNT         PIC 9999 VALUE ZEROS.
       01  OTHER-COUNT          PIC 9999 VALUE ZEROS.
       01  TOTAL-COUNT          PIC 9999 VALUE ZEROS.
       01  MALE-PROP            PIC 999 VALUE ZEROS.
       01  MALE-PROP-DISP       PIC Z99,99.
       01  FEMALE-PROP          PIC 999 VALUE ZEROS.
       01  FEMALE-PROP-DISP     PIC Z99,99.
       01  OTHER-PROP           PIC 999 VALUE ZEROS.
       01  OTHER-PROP-DISP      PIC Z99,99.
       01  REPORT-LINE          PIC X(80) VALUE SPACES.
       01  DASH-LINE            PIC X(80) VALUE ALL '-'.

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME               PIC X(30) VALUE 'country'.
       01  USERNAME             PIC X(30) VALUE 'cobol'.
       01  PASSWD               PIC X(10) VALUE SPACE.

OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(039) VALUE "SELECT MIN(age), MAX(age) FROM"
OCESQL  &  " databank".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(143) VALUE "SELECT age FROM ( SELECT age F"
OCESQL  &  "ROM databank ORDER BY age FETCH FIRST 50 PERCENT ROWS ONLY"
OCESQL  &  " ) AS subquery ORDER BY age DESC FETCH FIRST 1 ROW ONLY".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(053) VALUE "SELECT country, gender FROM da"
OCESQL  &  "tabank ORDER BY country".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
       1000-MAIN-START.
OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.

           IF SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.

           PERFORM 2000-GENERATE-REPORT
               THRU 2000-GENERATE-REPORT-END.

       1000-MAIN-END.
OCESQL*    EXEC SQL COMMIT WORK END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL DISCONNECT ALL END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
           STOP RUN.
      ******************************************************************
       1001-ERROR-RTN-START.
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE SPACE.
           EVALUATE SQLCODE
              WHEN +100
                 DISPLAY "Record not found"
              WHEN -01
                 DISPLAY "Connection failed"
              WHEN -20
                 DISPLAY "Internal error"
              WHEN -30
                 DISPLAY "Database error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
OCESQL*          EXEC SQL
OCESQL*              ROLLBACK
OCESQL*          END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ROLLBACK" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
              WHEN OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       1001-ERROR-RTN-END.
           STOP RUN.
      ******************************************************************
       2000-GENERATE-REPORT.
           DISPLAY 'Generating report...'.

           PERFORM 2100-GET-AGE-STATISTICS
               THRU 2100-GET-AGE-STATISTICS-END.

           PERFORM 2200-GET-GENDER-PROPORTIONS
               THRU 2200-GET-GENDER-PROPORTIONS-END.

       2000-GENERATE-REPORT-END.
      ******************************************************************
       2100-GET-AGE-STATISTICS.
OCESQL*    EXEC SQL
OCESQL*        SELECT MIN(age), MAX(age)
OCESQL*        INTO :MIN-AGE, :MAX-AGE
OCESQL*        FROM databank
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE MIN-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE MAX-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 0
OCESQL          BY VALUE 2
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

           DISPLAY 'Age Statistics: '.
           DISPLAY 'Minimum Age: ' MIN-AGE.
           DISPLAY 'Maximum Age: ' MAX-AGE.
           DISPLAY DASH-LINE.

OCESQL*    EXEC SQL
OCESQL*        SELECT age
OCESQL*        INTO :MEDIAN-AGE
OCESQL*        FROM (
OCESQL*            SELECT age
OCESQL*            FROM databank
OCESQL*            ORDER BY age
OCESQL*            FETCH FIRST 50 PERCENT ROWS ONLY
OCESQL*        ) AS subquery
OCESQL*        ORDER BY age DESC
OCESQL*        FETCH FIRST 1 ROW ONLY
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE MEDIAN-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 0
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

           DISPLAY 'Median Age: ' MEDIAN-AGE.
           DISPLAY DASH-LINE.
       2100-GET-AGE-STATISTICS-END.
      ******************************************************************
       2200-GET-GENDER-PROPORTIONS.
OCESQL*    EXEC SQL DECLARE COUNTRY_CUR CURSOR FOR
OCESQL*        SELECT country, gender
OCESQL*        FROM databank
OCESQL*        ORDER BY country
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GenRpt_COUNTRY_CUR" & x"00"
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.

OCESQL*    EXEC SQL OPEN COUNTRY_CUR END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GenRpt_COUNTRY_CUR" & x"00"
OCESQL     END-CALL.

           PERFORM WITH TEST AFTER UNTIL SQLCODE = +100
OCESQL*        EXEC SQL
OCESQL*            FETCH COUNTRY_CUR
OCESQL*            INTO :COUNTRY, :GENDER
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE COUNTRY
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GENDER
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GenRpt_COUNTRY_CUR" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               IF SQLCODE = 0 THEN
                   IF COUNTRY NOT = CURRENT-COUNTRY THEN
                       IF CURRENT-COUNTRY NOT = SPACES THEN
                           PERFORM 
                         2300-CALCULATE-AND-DISPLAY-GENDER-PROPORTIONS
                               THRU 
                      2300-CALCULATE-AND-DISPLAY-GENDER-PROPORTIONS-END
                       END-IF
                       MOVE COUNTRY TO CURRENT-COUNTRY
                       MOVE ZEROS TO MALE-COUNT, FEMALE-COUNT, 
                                     OTHER-COUNT, TOTAL-COUNT
                   END-IF

                   ADD 1 TO TOTAL-COUNT

                   EVALUATE GENDER
                       WHEN 'Male'
                           ADD 1 TO MALE-COUNT
                       WHEN 'Female'
                           ADD 1 TO FEMALE-COUNT
                       WHEN OTHER
                           ADD 1 TO OTHER-COUNT
                   END-EVALUATE
               END-IF
           END-PERFORM.

           PERFORM 2300-CALCULATE-AND-DISPLAY-GENDER-PROPORTIONS
               THRU 2300-CALCULATE-AND-DISPLAY-GENDER-PROPORTIONS-END

OCESQL*    EXEC SQL CLOSE COUNTRY_CUR END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GenRpt_COUNTRY_CUR" & x"00"
OCESQL     END-CALL
OCESQL    .
       2200-GET-GENDER-PROPORTIONS-END.
      ******************************************************************
       2300-CALCULATE-AND-DISPLAY-GENDER-PROPORTIONS.
           IF TOTAL-COUNT NOT = 0 THEN
               COMPUTE MALE-PROP = (MALE-COUNT * 100) / TOTAL-COUNT
               COMPUTE FEMALE-PROP = (FEMALE-COUNT * 100) / TOTAL-COUNT
               COMPUTE OTHER-PROP = (OTHER-COUNT * 100) / TOTAL-COUNT
           ELSE
               MOVE ZEROS TO MALE-PROP
               MOVE ZEROS TO FEMALE-PROP
               MOVE ZEROS TO OTHER-PROP
           END-IF

           MOVE MALE-PROP TO MALE-PROP-DISP
           MOVE FEMALE-PROP TO FEMALE-PROP-DISP
           MOVE OTHER-PROP TO OTHER-PROP-DISP

           DISPLAY 'Country: ' CURRENT-COUNTRY
           DISPLAY DASH-LINE

           PERFORM 2210-DISPLAY-GENDER-PROPORTIONS
               THRU 2210-DISPLAY-GENDER-PROPORTIONS-END

           DISPLAY DASH-LINE.
       2300-CALCULATE-AND-DISPLAY-GENDER-PROPORTIONS-END.
      ******************************************************************
       2210-DISPLAY-GENDER-PROPORTIONS.
           STRING 'Gender: Male, Proportion: ', MALE-PROP-DISP, ' %'
                  DELIMITED BY SIZE
                  INTO REPORT-LINE
           DISPLAY REPORT-LINE

           STRING 'Gender: Female, Proportion: ', FEMALE-PROP-DISP, ' %'
                  DELIMITED BY SIZE
                  INTO REPORT-LINE
           DISPLAY REPORT-LINE

           STRING 'Gender: Other, Proportion: ', OTHER-PROP-DISP, ' %'
                  DELIMITED BY SIZE
                  INTO REPORT-LINE
           DISPLAY REPORT-LINE.
       2210-DISPLAY-GENDER-PROPORTIONS-END.
      ******************************************************************
      ******************************************************************
      ******************************************************************
