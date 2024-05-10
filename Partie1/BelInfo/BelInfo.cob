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

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME             PIC X(30) VALUE 'country'.
       01  USERNAME           PIC X(30) VALUE 'cobol'.
       01  PASSWD             PIC X(10) VALUE SPACE.

       01  SQL-IND-RESULT.
           05  SQL-NAME       PIC X(50).
           05  SQL-FNAME      PIC X(50).
           05  SQL-EMAIL      PIC X(100).
           05  SQL-QUOTE      PIC X(255).

OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(148) VALUE "SELECT last_name, first_name, "
OCESQL  &  "email, phrase FROM databank, phrase WHERE databank.country"
OCESQL  &  "_code = 'BE' AND databank.country_code = phrase.country_co"
OCESQL  &  "de".
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

           PERFORM 3001-GET-BELG-INFO
               THRU 3001-GET-BELG-INFO-END.

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
       3001-GET-BELG-INFO.
           DISPLAY 'Individuals from Belgium: '.
           DISPLAY DASH-LINE.

OCESQL*    EXEC SQL DECLARE BELG_CUR CURSOR FOR
OCESQL*        SELECT last_name, first_name, email, phrase
OCESQL*        FROM databank, phrase
OCESQL*        WHERE databank.country_code = 'BE'
OCESQL*          AND databank.country_code = phrase.country_code
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "BelInfo_BELG_CUR" & x"00"
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.

OCESQL*    EXEC SQL OPEN BELG_CUR END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "BelInfo_BELG_CUR" & x"00"
OCESQL     END-CALL.

           PERFORM WITH TEST AFTER UNTIL SQLCODE = +100
OCESQL*        EXEC SQL
OCESQL*            FETCH BELG_CUR
OCESQL*            INTO :SQL-NAME, :SQL-FNAME, :SQL-EMAIL, :SQL-QUOTE
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
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
OCESQL          BY VALUE 255
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-QUOTE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "BelInfo_BELG_CUR" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

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

OCESQL*    EXEC SQL CLOSE BELG_CUR END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "BelInfo_BELG_CUR" & x"00"
OCESQL     END-CALL
OCESQL    .
       3001-GET-BELG-INFO-END.
      ******************************************************************
      ******************************************************************
      ******************************************************************
