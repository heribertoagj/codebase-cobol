      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MPORPAG_T_MPORPAG)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB053))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB053)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MPORPAG_T_MPORPAG TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             HRECEB                         TIMESTAMP NOT NULL,
             HLIBRC                         TIMESTAMP,
             QORPAG_LOTE                    DECIMAL(5, 0) NOT NULL,
             VTOT_LOTE_ORPAG                DECIMAL(15, 2) NOT NULL,
             QTOT_PCELA_LOTE                DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MPORPAG_T_MPORPAG           *
      ******************************************************************
       01  BACCB053.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HRECEB               PIC X(26).
      *    *************************************************************
           10 HLIBRC               PIC X(26).
      *    *************************************************************
           10 QORPAG-LOTE          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VTOT-LOTE-ORPAG      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTOT-PCELA-LOTE      PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
