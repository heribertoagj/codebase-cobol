***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MOVTO_ESTRN_CRRTT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB090))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB090)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MOVTO_ESTRN_CRRTT TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             DCREDT_ESTRN_TARIF             DATE NOT NULL,
             CTPO_CTA_MOVTO                 DECIMAL(1, 0) NOT NULL,
             CBCO_DSTNO_MOVTO               DECIMAL(3, 0) NOT NULL,
             CAG_DSTNO_MOVTO                DECIMAL(5, 0) NOT NULL,
             CCTA_DSTNO_MOVTO               DECIMAL(13, 0) NOT NULL,
             CPAB_DSTNO_MOVTO               DECIMAL(5, 0) NOT NULL,
             VESTRN_TARIF                   DECIMAL(11, 2) NOT NULL,
             DOCOR_EVNTO                    DATE NOT NULL,
             QEVNTO_AGRUP                   DECIMAL(6, 0) NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL,
             DEFETV_DEB_MOVTO               DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MOVTO_ESTRN_CRRTT           *
      ******************************************************************
       01  GFCTB090.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 DCREDT-ESTRN-TARIF   PIC X(10).
      *    *************************************************************
           10 CTPO-CTA-MOVTO       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-DSTNO-MOVTO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-DSTNO-MOVTO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-DSTNO-MOVTO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPAB-DSTNO-MOVTO     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VESTRN-TARIF         PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DOCOR-EVNTO          PIC X(10).
      *    *************************************************************
           10 QEVNTO-AGRUP         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-DEB-MOVTO     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
