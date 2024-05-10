       IDENTIFICATION DIVISION.
       PROGRAM-ID. Corrige.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  IND-NAME           PIC X(50) VALUE SPACES.
       01  IND-FNAME          PIC X(50) VALUE SPACES.
       01  IND-EMAIL          PIC X(100) VALUE SPACES.
       01  IND-COUNTRY        PIC X(50) VALUE SPACES.
       01  IND-COUNTRY-CODE   PIC X(10) VALUE SPACES.
       01  CORRECT-COUNTRY-CODE PIC X(10) VALUE SPACES.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME             PIC X(30) VALUE 'country'.
       01  USERNAME           PIC X(30) VALUE 'cobol'.
       01  PASSWD             PIC X(10) VALUE SPACE.

       01  SQL-IND-RESULT.
           05  SQL-ID         PIC X(36).
           05  SQL-NAME       PIC X(50).
           05  SQL-FNAME      PIC X(50).
           05  SQL-EMAIL      PIC X(100).
           05  SQL-COUNTRY    PIC X(50).
           05  SQL-COUNTRY-CODE PIC X(10).

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

           PERFORM 3001-CORRECT-COUNTRY-CODES
               THRU 3001-CORRECT-COUNTRY-CODES-END.

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
       3001-CORRECT-COUNTRY-CODES.
           DISPLAY 'Correcting country codes...'.

           EXEC SQL DECLARE CURSOR1 CURSOR FOR
               SELECT id, last_name, first_name, email, country, 
                      country_code
               FROM databank
           END-EXEC.

           EXEC SQL OPEN CURSOR1 END-EXEC.

           PERFORM WITH TEST AFTER UNTIL SQLCODE = +100
               EXEC SQL
                   FETCH CURSOR1
                   INTO :SQL-ID, :SQL-NAME, :SQL-FNAME, :SQL-EMAIL, 
                        :SQL-COUNTRY, :SQL-COUNTRY-CODE
               END-EXEC

               IF SQLCODE = 0 THEN
                   MOVE SQL-COUNTRY TO IND-COUNTRY
                   MOVE SQL-COUNTRY-CODE TO IND-COUNTRY-CODE
                   
                   PERFORM 3100-DETERMINE-CORRECT-CODE

                   IF IND-COUNTRY-CODE NOT EQUAL TO CORRECT-COUNTRY-CODE
                    THEN
                       EXEC SQL
                           UPDATE databank
                           SET country_code = :CORRECT-COUNTRY-CODE
                           WHERE id = :SQL-ID
                       END-EXEC
                       
                       IF SQLCODE = 0 THEN
                           DISPLAY 'Updated country code for ID: ' 
                                    SQL-ID
                       ELSE
                           PERFORM 1001-ERROR-RTN-START
                               THRU 1001-ERROR-RTN-END
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE CURSOR1 END-EXEC.
       3001-CORRECT-COUNTRY-CODES-END.
      ******************************************************************
       3100-DETERMINE-CORRECT-CODE.
           IF IND-COUNTRY = 'France' THEN
               MOVE 'FR' TO CORRECT-COUNTRY-CODE
           ELSE IF IND-COUNTRY = 'Belgium' THEN
               MOVE 'BE' TO CORRECT-COUNTRY-CODE
           ELSE IF IND-COUNTRY = 'Luxembourg' THEN
               MOVE 'LU' TO CORRECT-COUNTRY-CODE
           ELSE IF IND-COUNTRY = 'Switzerland' THEN
               MOVE 'CH' TO CORRECT-COUNTRY-CODE
           ELSE
               MOVE '??' TO CORRECT-COUNTRY-CODE
           END-IF.
       3100-DETERMINE-CORRECT-CODE-END.
      ******************************************************************
