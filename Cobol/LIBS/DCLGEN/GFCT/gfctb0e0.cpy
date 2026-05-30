      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SUB_PRFIL_CTRL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0E0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0E0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SUB_PRFIL_CTRL TABLE
           ( CPRFIL_ACSSO_TRANS             DECIMAL(3, 0) NOT NULL,
             CSUB_PRFIL_ACSSO               DECIMAL(3, 0) NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             RSUB_PRFIL_ACSSO               CHAR(30) NOT NULL,
             CTERM_INCL_PRFIL               CHAR(8) NOT NULL,
             HINCL_PRFIL_ACSSO              TIMESTAMP NOT NULL,
             CTERM_EXCL_PRFIL               CHAR(8) NOT NULL,
             HEXCL_PRFIL_ACSSO              TIMESTAMP NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CNVEL_ACSSO_AGPTO              DECIMAL(3, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SUB_PRFIL_CTRL              *
      ******************************************************************
       01  GFCTB0E0.
      *    *************************************************************
           10 CPRFIL-ACSSO-TRANS   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSUB-PRFIL-ACSSO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 RSUB-PRFIL-ACSSO     PIC X(30).
      *    *************************************************************
           10 CTERM-INCL-PRFIL     PIC X(8).
      *    *************************************************************
           10 HINCL-PRFIL-ACSSO    PIC X(26).
      *    *************************************************************
           10 CTERM-EXCL-PRFIL     PIC X(8).
      *    *************************************************************
           10 HEXCL-PRFIL-ACSSO    PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CNVEL-ACSSO-AGPTO    PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
