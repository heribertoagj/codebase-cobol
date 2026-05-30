***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOVTO_PCELD_CRRTT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0I3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0I3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOVTO_PCELD_CRRTT TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CPCELA_TARIF                   DECIMAL(3, 0) NOT NULL,
             CTOT_PCELA_TARIF               DECIMAL(5, 0) NOT NULL,
             VTARIF_BRUTO_TOT               DECIMAL(11, 2) NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL,
             VTARIF_LIQ_MOVTO               DECIMAL(11, 2) NOT NULL,
             VTARIF_DEB_MOVTO               DECIMAL(11, 2) NOT NULL,
             DEFETV_DEB_MOVTO               DATE NOT NULL,
             CSIT_EVNTO_RECBD               DECIMAL(2, 0) NOT NULL,
             DAGNDA_DEB_MOVTO               DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOVTO_PCELD_CRRTT          *
      ******************************************************************
       01  GFCTB0I3.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CPCELA-TARIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTOT-PCELA-TARIF     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-BRUTO-TOT     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-LIQ-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-DEB-MOVTO     PIC X(10).
      *    *************************************************************
           10 CSIT-EVNTO-RECBD     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DAGNDA-DEB-MOVTO     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
