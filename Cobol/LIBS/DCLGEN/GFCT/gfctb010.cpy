***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.AGNDA_DEB_AGPTO)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB010))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB010)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.AGNDA_DEB_AGPTO TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_FLEXZ_CTA                 DECIMAL(2, 0) NOT NULL,
             CINDCD_AGPTO_TOT               CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.AGNDA_DEB_AGPTO             *
      ******************************************************************
       01  GFCTB010.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-FLEXZ-CTA       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AGPTO-TOT     PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
