      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOTVO_DCLAS_OPER)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB097))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB097)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOTVO_DCLAS_OPER TABLE
           ( CMOTVO_DCLAS_OPER              DECIMAL(5, 0) NOT NULL,
             RMOTVO_DCLAS_OPER              CHAR(60) NOT NULL,
             CMOTVO_DCLAS_BACEN             CHAR(3) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL                          TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOTVO_DCLAS_OPER           *
      ******************************************************************
       01  RURCB097.
      *    *************************************************************
           10 CMOTVO-DCLAS-OPER    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RMOTVO-DCLAS-OPER    PIC X(60).
      *    *************************************************************
           10 CMOTVO-DCLAS-BACEN   PIC X(3).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
