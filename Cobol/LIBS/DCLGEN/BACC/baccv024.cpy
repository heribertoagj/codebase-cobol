      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01RMORPAG_TRORPAG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV024))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV024)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01RMORPAG_TRORPAG TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CREMSS_ORPAG                   DECIMAL(7, 0) NOT NULL,
             CGRP_REJEI_BACC                DECIMAL(1, 0) NOT NULL,
             CTPO_REJEI_BACC                DECIMAL(2, 0) NOT NULL,
             CIND_SOLUC                     DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01RMORPAG_TRORPAG          *
      ******************************************************************
       01  BACCV024.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CREMSS-ORPAG         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-REJEI-BACC      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-REJEI-BACC      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIND-SOLUC           PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
