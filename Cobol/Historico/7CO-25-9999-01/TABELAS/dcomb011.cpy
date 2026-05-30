      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONTR_LIM_DESC)                           *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB011))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB011)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONTR_LIM_DESC TABLE
           ( CCONTR_LIM_DESC                DECIMAL(9, 0) NOT NULL,
             CVRSAO_CONTR_LIM               DECIMAL(3, 0) NOT NULL,
             CINDCD_MODLD_DESC              CHAR(1) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(5, 0) NOT NULL,
             CBCO_OPER_CONTR                DECIMAL(3, 0) NOT NULL,
             CAG_OPER_CONTR                 DECIMAL(5, 0) NOT NULL,
             DINCL_CONTR_LIM                DATE NOT NULL,
             DINIC_VGCIA_CONTR              DATE NOT NULL,
             DVCTO_CONTR_LIM                DATE NOT NULL,
             TCONTR_LIM_DESC                DECIMAL(5, 0) NOT NULL,
             CINDCD_PRZ_CONTR               CHAR(2) NOT NULL,
             VCONTR_LIM_DESC                DECIMAL(17, 2) NOT NULL,
             VUTLZD_CONTR_LIM               DECIMAL(17, 2) NOT NULL,
             VDISPN_CONTR_LIM               DECIMAL(17, 2) NOT NULL,
             HATULZ_SDO_CONTR               TIMESTAMP NOT NULL,
             CINDCD_CONTR_ROTVO             CHAR(1) NOT NULL,
             CINDCD_RENOV_AUTOM             CHAR(1) NOT NULL,
             CINDCD_RTEIO_BNEFC             CHAR(1) NOT NULL,
             CINDCD_LIBRC_CONTR             CHAR(1) NOT NULL,
             CINDCD_TARIF_ADTTO             CHAR(1) NOT NULL,
             CPRODT                         DECIMAL(3, 0) NOT NULL,
             DINIC_PARMZ_CONTR              DATE NOT NULL,
             CTPO_COBR_TAC                  DECIMAL(3, 0) NOT NULL,
             CINDCD_TAC_SPROD               CHAR(1) NOT NULL,
             VTAC_CONTR_LIM                 DECIMAL(17, 2) NOT NULL,
             DTAC_CONTR_LIM                 DATE NOT NULL,
             CINDCD_TPO_RENOV               CHAR(1) NOT NULL,
             DRENOV_CONTR_LIM               DATE NOT NULL,
             CSIT_DESC_COML                 DECIMAL(3, 0) NOT NULL,
             HSIT_DESC_COML                 TIMESTAMP NOT NULL,
             DULT_UTILZ_CONTR               DATE NOT NULL,
             DULT_ADTTO_CONTR               DATE NOT NULL,
             CINDCD_MOTVO_ADTTO             CHAR(1) NOT NULL,
             VTARIF_ADTTO_CONTR             DECIMAL(17, 2) NOT NULL,
             DCOBR_TARIF_ADTTO              DATE NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CCLUB                          DECIMAL(10, 0) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CINDCD_OPER_INADP              CHAR(1) NOT NULL,
             DATULZ_OPER_INADP              DATE NOT NULL,
             CCHAVE_ELMTO_DESC              CHAR(45) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONTR_LIM_DESC             *
      ******************************************************************
       01  DCOMB011.
      *    *************************************************************
           10 CCONTR-LIM-DESC      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CVRSAO-CONTR-LIM     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-MODLD-DESC    PIC X(1).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-OPER-CONTR      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-OPER-CONTR       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINCL-CONTR-LIM      PIC X(10).
      *    *************************************************************
           10 DINIC-VGCIA-CONTR    PIC X(10).
      *    *************************************************************
           10 DVCTO-CONTR-LIM      PIC X(10).
      *    *************************************************************
           10 TCONTR-LIM-DESC      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRZ-CONTR     PIC X(2).
      *    *************************************************************
           10 VCONTR-LIM-DESC      PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VUTLZD-CONTR-LIM     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VDISPN-CONTR-LIM     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 HATULZ-SDO-CONTR     PIC X(26).
      *    *************************************************************
           10 CINDCD-CONTR-ROTVO   PIC X(1).
      *    *************************************************************
           10 CINDCD-RENOV-AUTOM   PIC X(1).
      *    *************************************************************
           10 CINDCD-RTEIO-BNEFC   PIC X(1).
      *    *************************************************************
           10 CINDCD-LIBRC-CONTR   PIC X(1).
      *    *************************************************************
           10 CINDCD-TARIF-ADTTO   PIC X(1).
      *    *************************************************************
           10 CPRODT               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-PARMZ-CONTR    PIC X(10).
      *    *************************************************************
           10 CTPO-COBR-TAC        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TAC-SPROD     PIC X(1).
      *    *************************************************************
           10 VTAC-CONTR-LIM       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DTAC-CONTR-LIM       PIC X(10).
      *    *************************************************************
           10 CINDCD-TPO-RENOV     PIC X(1).
      *    *************************************************************
           10 DRENOV-CONTR-LIM     PIC X(10).
      *    *************************************************************
           10 CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HSIT-DESC-COML       PIC X(26).
      *    *************************************************************
           10 DULT-UTILZ-CONTR     PIC X(10).
      *    *************************************************************
           10 DULT-ADTTO-CONTR     PIC X(10).
      *    *************************************************************
           10 CINDCD-MOTVO-ADTTO   PIC X(1).
      *    *************************************************************
           10 VTARIF-ADTTO-CONTR   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DCOBR-TARIF-ADTTO    PIC X(10).
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCLUB                PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CINDCD-OPER-INADP    PIC X(1).
      *    *************************************************************
           10 DATULZ-OPER-INADP    PIC X(10).
      *    *************************************************************
           10 CCHAVE-ELMTO-DESC    PIC X(45).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 51      *
      ******************************************************************
