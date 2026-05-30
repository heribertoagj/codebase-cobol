      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTPO_LOG_EVNTO)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB037))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB037)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTPO_LOG_EVNTO TABLE
           ( CLOG_EVNTO                     DECIMAL(10, 0) NOT NULL,
             RTPO_LOG_EVNTO                 CHAR(30) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTPO_LOG_EVNTO              *
      ******************************************************************
       01  BVVEB037.
      *    *************************************************************
           10 CLOG-EVNTO           PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-LOG-EVNTO       PIC X(30).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
