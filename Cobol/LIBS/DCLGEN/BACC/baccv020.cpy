      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01LCTO_CTBIL_BACC)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV020))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV020)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01LCTO_CTBIL_BACC TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL                    DECIMAL(3, 0) NOT NULL,
             CDEB_CREDT                     DECIMAL(1, 0) NOT NULL,
             VLCTO_CTBIL                    DECIMAL(15, 2) NOT NULL,
             DLCTO_CTBIL                    DATE NOT NULL,
             CJUNC_DEPDC_DSTNO              DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01LCTO_CTBIL_BACC          *
      ******************************************************************
       01  BACCV020.
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
           10 CDEB-CREDT           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VLCTO-CTBIL          PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DLCTO-CTBIL          PIC X(10).
      *    *************************************************************
           10 CJUNC-DEPDC-DSTNO    PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
