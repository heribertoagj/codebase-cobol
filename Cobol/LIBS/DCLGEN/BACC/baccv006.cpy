      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01MAPA_REMSS_BACC)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV006))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV006)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01MAPA_REMSS_BACC TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CDIG_MAPA_ORPAG                DECIMAL(1, 0) NOT NULL,
             DCRIAC                         DATE NOT NULL,
             CTPO_CAPTC_ORPAG               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01MAPA_REMSS_BACC          *
      ******************************************************************
       01  BACCV006.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-MAPA-ORPAG      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DCRIAC               PIC X(10).
      *    *************************************************************
           10 CTPO-CAPTC-ORPAG     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
