      ******************************************************************
      * DCLGEN TABLE(DB2PRD.RSUMO_MENSAL_RAZAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB010))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB010)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.RSUMO_MENSAL_RAZAO TABLE
           ( CEMPR                          DECIMAL(5, 0) NOT NULL,
             CTPO_SMARI                     CHAR(1) NOT NULL,
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL                    DECIMAL(3, 0) NOT NULL,
             DMES_MOVTO_CTBIL               DECIMAL(6, 0) NOT NULL,
             VSDO_INIC_CTBIL                DECIMAL(17, 2),
             QLCTO_DEB_CTBIL                DECIMAL(15, 0),
             VLCTO_DEB_CTBIL                DECIMAL(17, 2),
             QLCTO_CREDT_CTBIL              DECIMAL(15, 0),
             VLCTO_CREDT_CTBIL              DECIMAL(17, 2),
             VSDO_FNAL_CTBIL                DECIMAL(17, 2)
           ) END-EXEC.
      ******************************************************************
      *  DB2PRD.RSUMO_MENSAL_RAZAO                                     *
      ******************************************************************
       01  BACCB010.
      *    *************************************************************
           10 CEMPR                PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-SMARI           PIC X(1).
      *    *************************************************************
           10 CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-MOVTO-CTBIL     PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VSDO-INIC-CTBIL      PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QLCTO-DEB-CTBIL      PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 VLCTO-DEB-CTBIL      PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QLCTO-CREDT-CTBIL    PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 VLCTO-CREDT-CTBIL    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-FNAL-CTBIL      PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
