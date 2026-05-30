***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAGNDA_EXCL_CTA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0J4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0J4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAGNDA_EXCL_CTA TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             DAGNDA_EXCL_CTA                DATE NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC_INCL_AGNDA              DECIMAL(5, 0) NOT NULL,
             CSIT_PROCM_CANCT               DECIMAL(1, 0) NOT NULL,
             QCTA_EXCLD_AGPTO               DECIMAL(8, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0),
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0),
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAGNDA_EXCL_CTA             *
      ******************************************************************
       01  GFCTB0J4.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DAGNDA-EXCL-CTA      PIC X(10).
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-INCL-AGNDA    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PROCM-CANCT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QCTA-EXCLD-AGPTO     PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
