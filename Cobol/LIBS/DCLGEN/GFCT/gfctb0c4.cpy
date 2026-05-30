      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PRFIL_ACSSO_USUAR)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0C4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0C4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PRFIL_ACSSO_USUAR TABLE
           ( CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             HINCL_ACSSO_PRFIL              TIMESTAMP NOT NULL,
             CPRFIL_ACSSO_TRANS             DECIMAL(3, 0) NOT NULL,
             CTERM_INCL_ACSSO               CHAR(8) NOT NULL,
             CTERM_EXCL_ACSSO               CHAR(8) NOT NULL,
             HEXCL_ACSSO_PRFIL              TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PRFIL_ACSSO_USUAR           *
      ******************************************************************
       01  GFCTB0C4.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-ACSSO-PRFIL    PIC X(26).
      *    *************************************************************
           10 CPRFIL-ACSSO-TRANS   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL-ACSSO     PIC X(8).
      *    *************************************************************
           10 CTERM-EXCL-ACSSO     PIC X(8).
      *    *************************************************************
           10 HEXCL-ACSSO-PRFIL    PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
