      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOTVO_RCUSA_AFLIA)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB027))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB027)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOTVO_RCUSA_AFLIA TABLE
           ( CMOTVO_RCUSA_AFLIA             DECIMAL(5, 0) NOT NULL,
             RMOTVO_RCUSA_AFLIA             CHAR(60),
             CUSUAR_MANUT                   CHAR(7)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOTVO_RCUSA_AFLIA          *
      ******************************************************************
       01  BVVEB027.
      *    *************************************************************
           10 CMOTVO-RCUSA-AFLIA   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RMOTVO-RCUSA-AFLIA   PIC X(60).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
