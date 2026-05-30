      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPOSIC_TITLO_BXADO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0J4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0J4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPOSIC_TITLO_BXADO TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CPRODT                         DECIMAL(3, 0) NOT NULL,
             CSPROD_DESC_COML               DECIMAL(3, 0) NOT NULL,
             DANO_OPER_DESC                 DECIMAL(4, 0) NOT NULL,
             NSEQ_OPER_DESC                 DECIMAL(9, 0) NOT NULL,
             NPCELA_DESC_COML               DECIMAL(5, 0) NOT NULL,
             CSIT_DESC_COML                 DECIMAL(3, 0) NOT NULL,
             DVCTO_PCELA_DESC               DATE,
             VPRINC_PCELA_DESC              DECIMAL(17, 2),
             VJURO_PCELA_DESC               DECIMAL(17, 2),
             VIOF_PCELA_DESC                DECIMAL(17, 2),
             VEFETV_BAIXA_PCELA             DECIMAL(17, 2),
             DEFETV_BAIXA_PCELA             DATE,
             VJURO_BAIXA_PCELA              DECIMAL(17, 2),
             VIOF_BAIXA_PCELA               DECIMAL(17, 2),
             CCNPJ_CPF                      DECIMAL(9, 0),
             CFLIAL_CNPJ                    DECIMAL(5, 0),
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0),
             IPSSOA_SACDO                   CHAR(60),
             CTITLO_COBR_BCO                DECIMAL(18, 0),
             CSEU_NRO_TITLO                 CHAR(15),
             QTITLO_CHEQ_PCELA              DECIMAL(9, 0),
             CCNPJ_CPF_ST                   CHAR(9),
             CFLIAL_CNPJ_ST                 CHAR(4),
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPOSIC_TITLO_BXADO          *
      ******************************************************************
       01  DCOMB0J4.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-PCELA-DESC     PIC X(10).
      *    *************************************************************
           10 VPRINC-PCELA-DESC    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO-PCELA-DESC     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VIOF-PCELA-DESC      PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VEFETV-BAIXA-PCELA   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-BAIXA-PCELA   PIC X(10).
      *    *************************************************************
           10 VJURO-BAIXA-PCELA    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VIOF-BAIXA-PCELA     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IPSSOA-SACDO         PIC X(60).
      *    *************************************************************
           10 CTITLO-COBR-BCO      PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CSEU-NRO-TITLO       PIC X(15).
      *    *************************************************************
           10 QTITLO-CHEQ-PCELA    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 27      *
      ******************************************************************
