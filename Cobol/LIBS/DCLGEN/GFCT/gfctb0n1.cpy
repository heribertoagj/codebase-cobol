***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOVTO_COBRD_EXTRT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOVTO_COBRD_EXTRT TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             QEVNTO_AGRUP                   DECIMAL(6, 0) NOT NULL,
             CTPO_CTA_MOVTO                 DECIMAL(1, 0) NOT NULL,
             CBCO_DSTNO_MOVTO               DECIMAL(3, 0) NOT NULL,
             CAG_DSTNO_MOVTO                DECIMAL(5, 0) NOT NULL,
             CCTA_DSTNO_MOVTO               DECIMAL(13, 0) NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL,
             VTARIF_DEB_MOVTO               DECIMAL(11, 2) NOT NULL,
             DEFETV_DEB_MOVTO               DATE NOT NULL,
             CSIT_EVNTO_RECBD               DECIMAL(2, 0) NOT NULL,
             CDOCTO_EXTRT                   DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOVTO_COBRD_EXTRT          *
      ******************************************************************
       01  GFCTB0N1.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QEVNTO-AGRUP         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CTA-MOVTO       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-DSTNO-MOVTO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-DSTNO-MOVTO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-DSTNO-MOVTO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-DEB-MOVTO     PIC X(10).
      *    *************************************************************
           10 CSIT-EVNTO-RECBD     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-EXTRT         PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
