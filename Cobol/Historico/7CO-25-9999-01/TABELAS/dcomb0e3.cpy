      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSMULA_FORNC_DESC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0E3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0E3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSMULA_FORNC_DESC TABLE
           ( NSMULA_OPER_DESC               DECIMAL(9, 0) NOT NULL,
             NFORNC_DESC_COML               DECIMAL(4, 0) NOT NULL,
             CDOCTO_FSCAL_DESC              CHAR(20) NOT NULL,
             CSERIE_DOCTO_FSCAL             CHAR(5) NOT NULL,
             DEMIS_DOCTO_FSCAL              DATE NOT NULL,
             DVCTO_DOCTO_FSCAL              DATE NOT NULL,
             VDOCTO_FSCAL                   DECIMAL(17, 2) NOT NULL,
             CCONDC_PGTO_FORNC              DECIMAL(5, 0) NOT NULL,
             CIDTFD_SEQ_PGTO                CHAR(16) NOT NULL,
             CORIGE_PGTO_FORNC              DECIMAL(1, 0) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CBCO_PRODT_COBR                DECIMAL(3, 0) NOT NULL,
             CIDTFD_PRODT_COBR              DECIMAL(2, 0) NOT NULL,
             CNEGOC_COBR                    DECIMAL(18, 0) NOT NULL,
             CTITLO_COBR_BCO                DECIMAL(18, 0) NOT NULL,
             CNRO_SEQ_TITLO                 DECIMAL(3, 0) NOT NULL,
             CINDCD_SELEC_DESC              CHAR(1) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             DINCL_REG                      DATE NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSMULA_FORNC_DESC           *
      ******************************************************************
       01  DCOMB0E3.
      *    *************************************************************
           10 NSMULA-OPER-DESC     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NFORNC-DESC-COML     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-FSCAL-DESC    PIC X(20).
      *    *************************************************************
           10 CSERIE-DOCTO-FSCAL   PIC X(5).
      *    *************************************************************
           10 DEMIS-DOCTO-FSCAL    PIC X(10).
      *    *************************************************************
           10 DVCTO-DOCTO-FSCAL    PIC X(10).
      *    *************************************************************
           10 VDOCTO-FSCAL         PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCONDC-PGTO-FORNC    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SEQ-PGTO      PIC X(16).
      *    *************************************************************
           10 CORIGE-PGTO-FORNC    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-PRODT-COBR      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CNEGOC-COBR          PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CTITLO-COBR-BCO      PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-SEQ-TITLO       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-SELEC-DESC    PIC X(1).
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 DINCL-REG            PIC X(10).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
