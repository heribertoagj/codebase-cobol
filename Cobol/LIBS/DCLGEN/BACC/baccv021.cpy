      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01DIVRG_CTBIL_BAC)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV021))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV021)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01DIVRG_CTBIL_BAC TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL                    DECIMAL(3, 0) NOT NULL,
             CJUNC_DEPDC_DSTNO              DECIMAL(5, 0) NOT NULL,
             VDIVRG_CTBIL                   DECIMAL(15, 2) NOT NULL,
             DOCOR                          DATE NOT NULL,
             CDEB_CREDT                     DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01DIVRG_CTBIL_BAC          *
      ******************************************************************
       01  BACCV021.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC-DSTNO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VDIVRG-CTBIL         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DOCOR                PIC X(10).
      *    *************************************************************
           10 CDEB-CREDT           PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
