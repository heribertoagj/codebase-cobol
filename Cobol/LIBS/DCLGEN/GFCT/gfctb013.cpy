***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.AGNDA_GRP_PAB)                             *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB013))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB013)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.AGNDA_GRP_PAB TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_FLEXZ_CTA                 DECIMAL(2, 0) NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.AGNDA_GRP_PAB               *
      ******************************************************************
       01  GFCTB013.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-FLEXZ-CTA       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
