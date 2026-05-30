      ******************************************************************
      * DCLGEN TABLE(DB2PRD.DEPDCIA_A_RMASTCA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB054))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB054)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.DEPDCIA_A_RMASTCA TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CREMSS_ORPAG                   DECIMAL(7, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             VTOT_LOTE_ORPAG                DECIMAL(15, 2) NOT NULL,
             QORPAG_LOTE                    DECIMAL(5, 0) NOT NULL,
             QTOT_PCELA_LOTE                DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.DEPDCIA_A_RMASTCA           *
      ******************************************************************
       01  BACCB054.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CREMSS-ORPAG         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VTOT-LOTE-ORPAG      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QORPAG-LOTE          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QTOT-PCELA-LOTE      PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
