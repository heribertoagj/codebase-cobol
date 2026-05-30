      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOVTO_TARIF_CONSL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0G6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0G6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOVTO_TARIF_CONSL TABLE
           ( CEMIS_RELAT                    DECIMAL(5, 0) NOT NULL,
             DCOMPT_FCHTO_TARIF             DECIMAL(6, 0) NOT NULL,
             CTPO_CLI                       DECIMAL(2, 0) NOT NULL,
             CTPO_TOT_TARIF                 DECIMAL(2, 0) NOT NULL,
             CIDTFD_TOT_TARIF               DECIMAL(7, 0) NOT NULL,
             CIDTFD_COMPL_TARIF             DECIMAL(7, 0) NOT NULL,
             QTARIF_MOVTO_BRUTO             DECIMAL(12, 0) NOT NULL,
             VTARIF_MOVTO_BRUTO             DECIMAL(12, 2) NOT NULL,
             QTARIF_MOVTO_LIQ               DECIMAL(12, 0) NOT NULL,
             VTARIF_MOVTO_LIQ               DECIMAL(12, 2) NOT NULL,
             QTARIF_BENEF_PCIAL             DECIMAL(12, 0) NOT NULL,
             VTARIF_BENEF_PCIAL             DECIMAL(12, 2) NOT NULL,
             QTARIF_BENEF_TOT               DECIMAL(12, 0) NOT NULL,
             VTARIF_BENEF_TOT               DECIMAL(12, 2) NOT NULL,
             QTARIF_DEB_CONS                DECIMAL(12, 0) NOT NULL,
             VTARIF_DEB_CONS                DECIMAL(12, 2) NOT NULL,
             QTARIF_ESTRN_ANT               DECIMAL(12, 2) NOT NULL,
             VTARIF_ESTRN_ANT               DECIMAL(12, 2) NOT NULL,
             QTARIF_ESTRN_ATUAL             DECIMAL(12, 0) NOT NULL,
             VTARIF_ESTRN_ATUAL             DECIMAL(12, 2) NOT NULL,
             QTARIF_MOVTO_DEB               DECIMAL(12, 0) NOT NULL,
             VTARIF_MOVTO_DEB               DECIMAL(12, 2) NOT NULL,
             QTARIF_DEB_AGNDA               DECIMAL(12, 0) NOT NULL,
             VTARIF_DEB_AGNDA               DECIMAL(12, 2) NOT NULL,
             QTARIF_DEB_RTCAO               DECIMAL(12, 0) NOT NULL,
             VTARIF_DEB_RTCAO               DECIMAL(12, 2) NOT NULL,
             QTARIF_CADTR_SIST              DECIMAL(12, 0) NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL,
             QTARIF_RECBR_CLI               DECIMAL(12, 0) NOT NULL,
             VTARIF_RECBR_CLI               DECIMAL(12, 2) NOT NULL,
             QTARIF_PERDA_CLI               DECIMAL(12, 0) NOT NULL,
             VTARIF_PERDA_CLI               DECIMAL(10, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOVTO_TARIF_CONSL          *
      ******************************************************************
       01  GFCTB0G6.
      *    *************************************************************
           10 CEMIS-RELAT          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DCOMPT-FCHTO-TARIF   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CLI             PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-TOT-TARIF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-TOT-TARIF     PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-COMPL-TARIF   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-MOVTO-BRUTO   PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-MOVTO-BRUTO   PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-MOVTO-LIQ     PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-MOVTO-LIQ     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-BENEF-PCIAL   PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-BENEF-PCIAL   PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-BENEF-TOT     PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-BENEF-TOT     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-DEB-CONS      PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-CONS      PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-ESTRN-ANT     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-ESTRN-ANT     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-ESTRN-ATUAL   PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-ESTRN-ATUAL   PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-MOVTO-DEB     PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-MOVTO-DEB     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-DEB-AGNDA     PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-AGNDA     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-DEB-RTCAO     PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-RTCAO     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-CADTR-SIST    PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-RECBR-CLI     PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-RECBR-CLI     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-PERDA-CLI     PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-PERDA-CLI     PIC S9(8)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************