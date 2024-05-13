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
       01  MALE-PROP            PIC 9999 VALUE ZEROS.
       01  MALE-PROP-DISP       PIC Z99,99.
       01  FEMALE-PROP          PIC 9999 VALUE ZEROS.
       01  FEMALE-PROP-DISP     PIC Z99,99.
       01  OTHER-PROP           PIC 9999 VALUE ZEROS.
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
               SELECT MIN(age), MAX(age), PERCENTILE_CONT(0.5) 
                   WITHIN GROUP (ORDER BY age)
               INTO :MIN-AGE, :MAX-AGE, :MEDIAN-AGE
               FROM databank
           END-EXEC.

           DISPLAY 'Age Statistics: '.
           DISPLAY 'Minimum Age: ' MIN-AGE.
           DISPLAY 'Maximum Age: ' MAX-AGE.
           DISPLAY 'Median Age: ' MEDIAN-AGE.
           DISPLAY DASH-LINE.
       2100-GET-AGE-STATISTICS-END.
      ******************************************************************
       2200-GET-GENDER-PROPORTIONS.
           EXEC SQL DECLARE COUNTRY_CUR CURSOR FOR
               SELECT country, male_proportion, female_proportion,
                    other_proportion
               FROM gender_proportions
           END-EXEC.

           EXEC SQL OPEN COUNTRY_CUR END-EXEC.

           PERFORM WITH TEST AFTER UNTIL SQLCODE = +100
               EXEC SQL
                   FETCH COUNTRY_CUR
                   INTO :COUNTRY, :MALE-PROP, :FEMALE-PROP, :OTHER-PROP
               END-EXEC

               IF SQLCODE = 0 THEN
                   DISPLAY 'Country: ' COUNTRY
                   DISPLAY DASH-LINE
                   MOVE MALE-PROP TO MALE-PROP-DISP
                   MOVE FEMALE-PROP TO FEMALE-PROP-DISP
                   MOVE OTHER-PROP TO OTHER-PROP-DISP
                   PERFORM 2210-DISPLAY-GENDER-PROPORTIONS
                       THRU 2210-DISPLAY-GENDER-PROPORTIONS-END
                   DISPLAY DASH-LINE
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE COUNTRY_CUR END-EXEC.
       2200-GET-GENDER-PROPORTIONS-END.
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
