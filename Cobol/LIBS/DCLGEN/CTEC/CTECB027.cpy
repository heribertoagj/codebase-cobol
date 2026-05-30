      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOTVO_PEND_DIGIT_DOCTO)                   *
      *        LIBRARY(AD.DB2.DCLGEN(CTECB027))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CTECB027)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOTVO_PEND_DIGIT_DOCTO TABLE
           ( CMOTVO_PEND_DIGIT              DECIMAL(2, 0) NOT NULL,
             RMOTVO_PEND_DIGIT              CHAR(30) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOTVO_PEND_DIGIT_DOCTO     *
      ******************************************************************
       01  CTECB027.
      *    *************************************************************
           10 CMOTVO-PEND-DIGIT    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RMOTVO-PEND-DIGIT    PIC X(30).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
      