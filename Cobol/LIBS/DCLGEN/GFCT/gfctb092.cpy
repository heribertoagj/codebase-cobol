***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MOVTO_EVNTO_CRRTT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB092))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB092)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MOVTO_EVNTO_CRRTT TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CMIDIA_SERVC_MOVTO             DECIMAL(5, 0) NOT NULL,
             CPERIF_SERVC_MOVTO             DECIMAL(5, 0) NOT NULL,
             QEVNTO_AGRUP                   DECIMAL(6, 0) NOT NULL,
             HPREST_SERVC_MOVTO             TIME NOT NULL,
             CTPO_CTA_MOVTO                 DECIMAL(1, 0) NOT NULL,
             CBCO_DSTNO_MOVTO               DECIMAL(3, 0) NOT NULL,
             CAG_DSTNO_MOVTO                DECIMAL(5, 0) NOT NULL,
             CCTA_DSTNO_MOVTO               DECIMAL(13, 0) NOT NULL,
             CPAB_DSTNO_MOVTO               DECIMAL(5, 0) NOT NULL,
             CAG_CTLZA_DEB                  DECIMAL(5, 0) NOT NULL,
             CCTA_CTLZA_DEB                 DECIMAL(13, 0) NOT NULL,
             VTARIF_BRUTO_MOVTO             DECIMAL(11, 2) NOT NULL,
             VTARIF_LIQ_MOVTO               DECIMAL(11, 2) NOT NULL,
             VTARIF_DEB_MOVTO               DECIMAL(11, 2) NOT NULL,
             DAGNDA_DEB_MOVTO               DATE NOT NULL,
             DEFETV_DEB_MOVTO               DATE NOT NULL,
             QMAX_TENTV_DEB                 DECIMAL(3, 0) NOT NULL,
             QTENTV_DEB_MOVTO               DECIMAL(3, 0) NOT NULL,
             CSIT_EVNTO_RECBD               DECIMAL(2, 0) NOT NULL,
             CIDTFD_PGMC_COBR               DECIMAL(2, 0) NOT NULL,
             DOCOR_EVNTO                    DATE NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL,
             CINDCD_TARIF_PCELD             CHAR(1) NOT NULL,
             QREG_INFMD_BONIF               DECIMAL(5, 0) NOT NULL,
             CINDCD_TARIF_EXCED             DECIMAL(1, 0) NOT NULL,
             CDOCTO_EXTRT                   DECIMAL(9, 0) NOT NULL,
             DANO_REFT_EXTRT                DECIMAL(4, 0) NOT NULL,
             CFUNC_SOLCT_EXTRT              DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             RJUSTF_SOLTC_EXTRT             CHAR(30) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MOVTO_EVNTO_CRRTT           *
      ******************************************************************
       01  GFCTB092.
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
           10 HPREST-SERVC-MOVTO   PIC X(8).
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
           10 CAG-CTLZA-DEB        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CTLZA-DEB       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-BRUTO-MOVTO   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-LIQ-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DAGNDA-DEB-MOVTO     PIC X(10).
      *    *************************************************************
           10 DEFETV-DEB-MOVTO     PIC X(10).
      *    *************************************************************
           10 QMAX-TENTV-DEB       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QTENTV-DEB-MOVTO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-EVNTO-RECBD     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PGMC-COBR     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DOCOR-EVNTO          PIC X(10).
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TARIF-PCELD   PIC X(1).
      *    *************************************************************
           10 QREG-INFMD-BONIF     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TARIF-EXCED   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-EXTRT         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-REFT-EXTRT      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-SOLCT-EXTRT    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 RJUSTF-SOLTC-EXTRT   PIC X(30).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 35      *
      ******************************************************************
