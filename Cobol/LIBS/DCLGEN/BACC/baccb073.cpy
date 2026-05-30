      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PARM_PEND_VOUCHER)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB073))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB073)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PARM_PEND_VOUCHER TABLE
           ( DULT_ATULZ                     DATE NOT NULL,
             QDIA_TOLRC_PEND                DECIMAL(3, 0) NOT NULL,
             CACAO_APOS_TOLRC               DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      *  DB2PRD.PARM_PEND_VOUCHER                                      *
      ******************************************************************
       01  BACCB073.
      *    *************************************************************
           10 DULT-ATULZ           PIC X(10).
      *    *************************************************************
           10 QDIA-TOLRC-PEND      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CACAO-APOS-TOLRC     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
