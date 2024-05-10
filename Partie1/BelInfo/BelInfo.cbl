       IDENTIFICATION DIVISION.
       PROGRAM-ID. BelInfo.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  IND-NAME           PIC X(50) VALUE SPACES.
       01  IND-FNAME          PIC X(50) VALUE SPACES.
       01  IND-EMAIL          PIC X(100) VALUE SPACES.
       01  IND-QUOTE          PIC X(255) VALUE SPACES.
       01  DASH-LINE          PIC X(48) VALUE ALL '-'.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME             PIC X(30) VALUE 'country'.
       01  USERNAME           PIC X(30) VALUE 'cobol'.
       01  PASSWD             PIC X(10) VALUE SPACE.

       01  SQL-IND-RESULT.
           05  SQL-NAME       PIC X(50).
           05  SQL-FNAME      PIC X(50).
           05  SQL-EMAIL      PIC X(100).
           05  SQL-QUOTE      PIC X(255).

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

           PERFORM 3001-GET-BELG-INFO
               THRU 3001-GET-BELG-INFO-END.

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
       3001-GET-BELG-INFO.
           DISPLAY 'Individuals from Belgium: '.
           DISPLAY DASH-LINE.

           EXEC SQL DECLARE BELG_CUR CURSOR FOR
               SELECT last_name, first_name, email, phrase
               FROM databank, phrase
               WHERE databank.country_code = 'BE'
                 AND databank.country_code = phrase.country_code
           END-EXEC.

           EXEC SQL OPEN BELG_CUR END-EXEC.

           PERFORM WITH TEST AFTER UNTIL SQLCODE = +100
               EXEC SQL
                   FETCH BELG_CUR
                   INTO :SQL-NAME, :SQL-FNAME, :SQL-EMAIL, :SQL-QUOTE
               END-EXEC

               IF SQLCODE = 0 THEN
                   MOVE SQL-NAME TO IND-NAME
                   MOVE SQL-FNAME TO IND-FNAME
                   MOVE SQL-EMAIL TO IND-EMAIL
                   MOVE SQL-QUOTE TO IND-QUOTE

                   DISPLAY 'Name: ' IND-NAME
                   DISPLAY 'Firstname: ' IND-FNAME
                   DISPLAY 'Email: ' IND-EMAIL
                   DISPLAY 'Quote: ' IND-QUOTE
                   DISPLAY DASH-LINE
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE BELG_CUR END-EXEC.
       3001-GET-BELG-INFO-END.
      ******************************************************************
