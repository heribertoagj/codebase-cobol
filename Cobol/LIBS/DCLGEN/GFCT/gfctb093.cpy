***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MOVTO_EVNTO_NAO)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB093))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB093)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MOVTO_EVNTO_NAO TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CMIDIA_SERVC_MOVTO             DECIMAL(5, 0) NOT NULL,
             CPERIF_SERVC_MOVTO             DECIMAL(5, 0) NOT NULL,
             QEVNTO_AGRUP                   DECIMAL(6, 0) NOT NULL,
             CIDTFD_NAO_CRRTT               CHAR(30) NOT NULL,
             VEVNTO_CORR_MOVTO              DECIMAL(11, 2) NOT NULL,
             VBRUTO_TARIF_MOVTO             DECIMAL(11, 2) NOT NULL,
             VTARIF_LIQ_MOVTO               DECIMAL(11, 2) NOT NULL,
             VTARIF_DEB_MOVTO               DECIMAL(11, 2) NOT NULL,
             DEFETV_DEB_MOVTO               DATE NOT NULL,
             CSIT_EVNTO_RECBD               DECIMAL(2, 0) NOT NULL,
             DOCOR_EVNTO                    DATE NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL,
             DAGNDA_DEB_MOVTO               DATE NOT NULL,
             CINDCD_TARIF_PCELD             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MOVTO_EVNTO_NAO             *
      ******************************************************************
       01  GFCTB093.
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
           10 CMIDIA-SERVC-MOVTO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPERIF-SERVC-MOVTO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QEVNTO-AGRUP         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-NAO-CRRTT     PIC X(30).
      *    *************************************************************
           10 VEVNTO-CORR-MOVTO    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VBRUTO-TARIF-MOVTO   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-LIQ-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-DEB-MOVTO     PIC X(10).
      *    *************************************************************
           10 CSIT-EVNTO-RECBD     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DOCOR-EVNTO          PIC X(10).
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DAGNDA-DEB-MOVTO     PIC X(10).
      *    *************************************************************
           10 CINDCD-TARIF-PCELD   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
