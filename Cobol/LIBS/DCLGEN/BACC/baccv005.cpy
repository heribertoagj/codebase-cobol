      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01TPO_CAPTC_ORPAG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV005))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV005)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01TPO_CAPTC_ORPAG TABLE
           ( CTPO_CAPTC_ORPAG               DECIMAL(2, 0) NOT NULL,
             RTPO_CAPTC_ORPAG               CHAR(15) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01TPO_CAPTC_ORPAG          *
      ******************************************************************
       01  BACCV005.
      *    *************************************************************
           10 CTPO-CAPTC-ORPAG     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-CAPTC-ORPAG     PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
