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

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME               PIC X(30) VALUE 'country'.
       01  USERNAME             PIC X(30) VALUE 'cobol'.
       01  PASSWD               PIC X(10) VALUE SPACE.

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

           PERFORM 2000-GENERATE-REPORT
               THRU 2000-GENERATE-REPORT-END.

       1000-MAIN-END.
           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
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
       2000-GENERATE-REPORT.
           DISPLAY 'Generating report...'.

           PERFORM 2100-GET-AGE-STATISTICS
               THRU 2100-GET-AGE-STATISTICS-END.

           PERFORM 2200-GET-GENDER-PROPORTIONS
               THRU 2200-GET-GENDER-PROPORTIONS-END.

       2000-GENERATE-REPORT-END.
      ******************************************************************
       2100-GET-AGE-STATISTICS.
           EXEC SQL
               SELECT MIN(age), MAX(age)
               INTO :MIN-AGE, :MAX-AGE
               FROM databank
           END-EXEC.

           DISPLAY 'Age Statistics: '.
           DISPLAY 'Minimum Age: ' MIN-AGE.
           DISPLAY 'Maximum Age: ' MAX-AGE.
           DISPLAY DASH-LINE.

           EXEC SQL
               SELECT age
               INTO :MEDIAN-AGE
               FROM (
                   SELECT age
                   FROM databank
                   ORDER BY age
                   FETCH FIRST 50 PERCENT ROWS ONLY
               ) AS subquery
               ORDER BY age DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC.

           DISPLAY 'Median Age: ' MEDIAN-AGE.
           DISPLAY DASH-LINE.
       2100-GET-AGE-STATISTICS-END.
      ******************************************************************
       2200-GET-GENDER-PROPORTIONS.
           EXEC SQL DECLARE COUNTRY_CUR CURSOR FOR
               SELECT country, gender
               FROM databank
               ORDER BY country
           END-EXEC.

           EXEC SQL OPEN COUNTRY_CUR END-EXEC.

           PERFORM WITH TEST AFTER UNTIL SQLCODE = +100
               EXEC SQL
                   FETCH COUNTRY_CUR
                   INTO :COUNTRY, :GENDER
               END-EXEC

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

           EXEC SQL CLOSE COUNTRY_CUR END-EXEC.
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
