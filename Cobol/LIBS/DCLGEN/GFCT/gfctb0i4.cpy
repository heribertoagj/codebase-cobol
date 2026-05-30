***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOVTO_PCELD_NAO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0I4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0I4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOVTO_PCELD_NAO TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CPCELA_TARIF                   DECIMAL(3, 0) NOT NULL,
             CTOT_PCELA_TARIF               DECIMAL(5, 0) NOT NULL,
             VTARIF_BRUTO_TOT               DECIMAL(11, 2) NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOVTO_PCELD_NAO            *
      ******************************************************************
       01  GFCTB0I4.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
