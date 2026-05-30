      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOG_EVNTO_CERTF)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB034))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB034)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOG_EVNTO_CERTF TABLE
           ( HLOG_EVNTO                     TIMESTAMP NOT NULL,
             CLOG_EVNTO                     DECIMAL(10, 0) NOT NULL,
             CTPO_EVNTO                     CHAR(3) NOT NULL,
             RCONTD_EVNTO                   VARCHAR(3500) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOG_EVNTO_CERTF            *
      ******************************************************************
       01  BVVEB034.
      *    *************************************************************
           10 HLOG-EVNTO           PIC X(26).
      *    *************************************************************
           10 CLOG-EVNTO           PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-EVNTO           PIC X(3).
      *    *************************************************************
           10 RCONTD-EVNTO.
              49 RCONTD-EVNTO-LEN
                 PIC S9(4) USAGE COMP.
              49 RCONTD-EVNTO-TEXT
                 PIC X(3500).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
