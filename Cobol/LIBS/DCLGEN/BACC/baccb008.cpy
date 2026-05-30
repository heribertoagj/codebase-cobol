      ******************************************************************
      * DCLGEN TABLE(DB2PRD.RSUMO_DIARIO_RAZAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB008))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB008)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.RSUMO_DIARIO_RAZAO TABLE
           ( CEMPR                          DECIMAL(5, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL                    DECIMAL(3, 0) NOT NULL,
             DMOVTO                         DATE NOT NULL,
             VSDO_INIC_DIA_CTA              DECIMAL(17, 2),
             QLCTO_DEB_CTBIL                DECIMAL(15, 0),
             VLCTO_DEB_CTBIL                DECIMAL(17, 2),
             QLCTO_CREDT_CTBIL              DECIMAL(15, 0),
             VLCTO_CREDT_CTBIL              DECIMAL(17, 2),
             VSDO_FNAL_DIA_CTA              DECIMAL(17, 2)
           ) END-EXEC.
      ******************************************************************
      *  DB2PRD.RSUMO_DIARIO_RAZAO                                     *
      ******************************************************************
       01  BACCB008.
      *    *************************************************************
           10 CEMPR                PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 VSDO-INIC-DIA-CTA    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QLCTO-DEB-CTBIL      PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 VLCTO-DEB-CTBIL      PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QLCTO-CREDT-CTBIL    PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 VLCTO-CREDT-CTBIL    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-FNAL-DIA-CTA    PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
