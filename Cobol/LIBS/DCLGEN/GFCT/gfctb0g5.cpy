      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOG_PRFIL_CTRL)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0G5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0G5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOG_PRFIL_CTRL TABLE
           ( CPRFIL_ACSSO_TRANS             DECIMAL(3, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CNVEL_ACSSO_AGPTO              DECIMAL(3, 0) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOG_PRFIL_CTRL             *
      ******************************************************************
       01  GFCTB0G5.
      *    *************************************************************
           10 CPRFIL-ACSSO-TRANS   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CNVEL-ACSSO-AGPTO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
