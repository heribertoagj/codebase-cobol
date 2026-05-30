      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_AGPTO_CTA)                             *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0F3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0F3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_AGPTO_CTA TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             RAGPTO_CTA                     CHAR(30) NOT NULL,
             CNVEL_ACSSO_AGPTO              DECIMAL(3, 0) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL,
             CINDCD_AGPTO_PERSO             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_AGPTO_CTA               *
      ******************************************************************
       01  GFCTB0F3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 RAGPTO-CTA           PIC X(30).
      *    *************************************************************
           10 CNVEL-ACSSO-AGPTO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AGPTO-PERSO   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
