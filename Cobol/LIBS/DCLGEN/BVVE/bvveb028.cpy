      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THIST_MOTVO_RCUSA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB028))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB028)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THIST_MOTVO_RCUSA TABLE
           ( CMOTVO_RCUSA_AFLIA             DECIMAL(5, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             RMOTVO_RCUSA_AFLIA             CHAR(60),
             CUSUAR_MANUT                   CHAR(7),
             CINDCD_TPO_MANUT               CHAR(1)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THIST_MOTVO_RCUSA           *
      ******************************************************************
       01  BVVEB028.
      *    *************************************************************
           10 CMOTVO-RCUSA-AFLIA   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 RMOTVO-RCUSA-AFLIA   PIC X(60).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 CINDCD-TPO-MANUT     PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
