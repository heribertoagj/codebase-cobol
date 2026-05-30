      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01ORPNLIB_TRORPAG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV026))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV026)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01ORPNLIB_TRORPAG TABLE
           ( DJULI                          DECIMAL(7, 0) NOT NULL,
             CTPO_CAPTC_ORPAG               DECIMAL(2, 0) NOT NULL,
             CSEQ_ORPAG                     DECIMAL(7, 0) NOT NULL,
             CGRP_REJEI_BACC                DECIMAL(1, 0) NOT NULL,
             CTPO_REJEI_BACC                DECIMAL(2, 0) NOT NULL,
             CIND_SOLUC                     DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01ORPNLIB_TRORPAG          *
      ******************************************************************
       01  BACCV026.
      *    *************************************************************
           10 DJULI                PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CAPTC-ORPAG     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-ORPAG           PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-REJEI-BACC      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-REJEI-BACC      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIND-SOLUC           PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
