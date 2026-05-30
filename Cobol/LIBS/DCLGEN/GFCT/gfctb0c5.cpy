      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PRFIL_CTRL_ACSSO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0C5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0C5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PRFIL_CTRL_ACSSO TABLE
           ( CPRFIL_ACSSO_TRANS             DECIMAL(3, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CIDTFD_DSTNO_PRFIL             DECIMAL(1, 0) NOT NULL,
             RPRFIL_ACSSO_TRANS             CHAR(30) NOT NULL,
             CTERM_INCL_PRFIL               CHAR(8) NOT NULL,
             HINCL_PRFIL_ACSSO              TIMESTAMP NOT NULL,
             CTERM_EXCL_PRFIL               CHAR(8) NOT NULL,
             HEXCL_PRFIL_ACSSO              TIMESTAMP NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CNVEL_ACSSO_AGPTO              DECIMAL(3, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PRFIL_CTRL_ACSSO            *
      ******************************************************************
       01  GFCTB0C5.
      *    *************************************************************
           10 CPRFIL-ACSSO-TRANS   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-DSTNO-PRFIL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RPRFIL-ACSSO-TRANS   PIC X(30).
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
