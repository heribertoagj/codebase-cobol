      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPGTO_INCOT_BASIL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0K2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0K2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPGTO_INCOT_BASIL TABLE
           ( NSEQ_PGTO_INCOT                DECIMAL(12, 0) NOT NULL,
             DPROCM_PGTO_INCOT              DATE NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CCART                          CHAR(5) NOT NULL,
             CCONTR_CORP_PCELA              DECIMAL(7, 0) NOT NULL,
             CSIT_PGTO_INCOT                DECIMAL(1, 0) NOT NULL,
             DVCTO_PCELA_DESC               DATE NOT NULL,
             DEFETV_BAIXA_PCELA             DATE NOT NULL,
             VEFETV_BAIXA_PCELA             DECIMAL(17, 2) NOT NULL,
             CTPO_PGTO_CONTR                DECIMAL(2, 0) NOT NULL,
             CTPO_PGTO_PCELA                DECIMAL(2, 0) NOT NULL,
             DANO_MES_BASE                  DECIMAL(6, 0) NOT NULL,
             CPRODT                         DECIMAL(3, 0) NOT NULL,
             NPCELA_DESC_COML               DECIMAL(5, 0) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             NSEQ_REG_DETLH                 DECIMAL(12, 0) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0),
             CTERM                          CHAR(8) NOT NULL,
             DINCL_REG                      DATE NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPGTO_INCOT_BASIL           *
      ******************************************************************
       01  DCOMB0K2.
      *    *************************************************************
           10 NSEQ-PGTO-INCOT      PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 DPROCM-PGTO-INCOT    PIC X(10).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CCART                PIC X(5).
      *    *************************************************************
           10 CCONTR-CORP-PCELA    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PGTO-INCOT      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-PCELA-DESC     PIC X(10).
      *    *************************************************************
           10 DEFETV-BAIXA-PCELA   PIC X(10).
      *    *************************************************************
           10 VEFETV-BAIXA-PCELA   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PGTO-CONTR      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PGTO-PCELA      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-MES-BASE        PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-REG-DETLH       PIC S9(12)V USAGE COMP-3.
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 27      *
      ******************************************************************
