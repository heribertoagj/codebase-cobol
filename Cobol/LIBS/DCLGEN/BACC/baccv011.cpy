      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01RMORPAG_TPSTREM)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV011))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV011)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01RMORPAG_TPSTREM TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CREMSS_ORPAG                   DECIMAL(7, 0) NOT NULL,
             CSIT_REMSS_ORPAG               DECIMAL(2, 0) NOT NULL,
             HREG                           TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01RMORPAG_TPSTREM          *
      ******************************************************************
       01  BACCV011.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CREMSS-ORPAG         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-REMSS-ORPAG     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HREG                 PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
