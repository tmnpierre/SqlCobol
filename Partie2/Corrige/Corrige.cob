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

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
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

OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(076) VALUE "SELECT id, last_name, first_na"
OCESQL  &  "me, email, country, country_code FROM databank".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(051) VALUE "UPDATE databank SET country_co"
OCESQL  &  "de = $1 WHERE id = $2".
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

           PERFORM 3001-CORRECT-COUNTRY-CODES
               THRU 3001-CORRECT-COUNTRY-CODES-END.

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
       3001-CORRECT-COUNTRY-CODES.
           DISPLAY 'Correcting country codes...'.

OCESQL*    EXEC SQL DECLARE CURSOR1 CURSOR FOR
OCESQL*        SELECT id, last_name, first_name, email, country, 
OCESQL*               country_code
OCESQL*        FROM databank
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "Corrige_CURSOR1" & x"00"
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.

OCESQL*    EXEC SQL OPEN CURSOR1 END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "Corrige_CURSOR1" & x"00"
OCESQL     END-CALL.

           PERFORM WITH TEST AFTER UNTIL SQLCODE = +100
OCESQL*        EXEC SQL
OCESQL*            FETCH CURSOR1
OCESQL*            INTO :SQL-ID, :SQL-NAME, :SQL-FNAME, :SQL-EMAIL, 
OCESQL*                 :SQL-COUNTRY, :SQL-COUNTRY-CODE
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 36
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-NAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-FNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 100
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-EMAIL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-COUNTRY
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-COUNTRY-CODE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "Corrige_CURSOR1" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               IF SQLCODE = 0 THEN
                   MOVE SQL-COUNTRY TO IND-COUNTRY
                   MOVE SQL-COUNTRY-CODE TO IND-COUNTRY-CODE
                   
                   PERFORM 3100-DETERMINE-CORRECT-CODE

                   IF IND-COUNTRY-CODE NOT EQUAL TO CORRECT-COUNTRY-CODE
                    THEN
OCESQL*                EXEC SQL
OCESQL*                    UPDATE databank
OCESQL*                    SET country_code = :CORRECT-COUNTRY-CODE
OCESQL*                    WHERE id = :SQL-ID
OCESQL*                END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE CORRECT-COUNTRY-CODE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 36
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 2
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
                       
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

OCESQL*    EXEC SQL CLOSE CURSOR1 END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "Corrige_CURSOR1" & x"00"
OCESQL     END-CALL
OCESQL    .
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
      ******************************************************************
      ******************************************************************
