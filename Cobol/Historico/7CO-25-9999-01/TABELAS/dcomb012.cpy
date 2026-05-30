      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONVE_CLI_DESC)                           *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB012))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB012)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONVE_CLI_DESC TABLE
           ( CCONVE_CLI_DESC                DECIMAL(9, 0) NOT NULL,
             CVRSAO_CONVE_CLI               DECIMAL(3, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(5, 0) NOT NULL,
             CBCO_OPER_CONVE                DECIMAL(3, 0) NOT NULL,
             CAG_OPER_CONVE                 DECIMAL(5, 0) NOT NULL,
             CINDCD_RENTB_NEGOC             CHAR(1) NOT NULL,
             CPRODT                         DECIMAL(3, 0) NOT NULL,
             CSPROD_DESC_COML               DECIMAL(3, 0) NOT NULL,
             DINCL_CONVE_CLI                DATE NOT NULL,
             DINIC_VGCIA_CONVE              DATE NOT NULL,
             DVCTO_CONVE_DESC               DATE NOT NULL,
             TCONVE_DESC_COML               DECIMAL(5, 0) NOT NULL,
             CINDCD_PRZ_CONVE               CHAR(2) NOT NULL,
             VLIM_CONVE_CLI                 DECIMAL(17, 2) NOT NULL,
             VUTLZD_CONVE_CLI               DECIMAL(17, 2) NOT NULL,
             VDISPN_CONVE_CLI               DECIMAL(17, 2) NOT NULL,
             HATULZ_SDO_CONVE               TIMESTAMP NOT NULL,
             CINDCD_CONVE_ROTVO             CHAR(1) NOT NULL,
             CTPO_COBR_TAC                  DECIMAL(3, 0) NOT NULL,
             CIDTFD_FIXO_TAC                DECIMAL(5, 0) NOT NULL,
             CIDTFD_PERC_TAC                DECIMAL(5, 0) NOT NULL,
             CIDTFD_MIN_TAC                 DECIMAL(5, 0) NOT NULL,
             CIDTFD_MAX_TAC                 DECIMAL(5, 0) NOT NULL,
             CINDCD_TARIF_REG               CHAR(1) NOT NULL,
             CINDCD_PGDOR_JURO              CHAR(1) NOT NULL,
             CINDCD_RTEIO_BNEFC             CHAR(1) NOT NULL,
             CINDCD_LIBRC_ANTCP             CHAR(1) NOT NULL,
             PMAX_LIBRC_ANTCP               DECIMAL(6, 3) NOT NULL,
             PMAX_ANTVL_VCTO                DECIMAL(6, 3) NOT NULL,
             TMIN_VCTO_TITLO                DECIMAL(5, 0) NOT NULL,
             TMAX_VCTO_TITLO                DECIMAL(5, 0) NOT NULL,
             CINDCD_PRZ_VCTO                CHAR(2) NOT NULL,
             CREGRA_GERC_MORA               DECIMAL(3, 0) NOT NULL,
             CREGRA_GERC_CRATS              DECIMAL(3, 0) NOT NULL,
             CINDCD_TX_SPROD                CHAR(1) NOT NULL,
             CTPO_TX_JURO_DESC              DECIMAL(3, 0) NOT NULL,
             CIDTFD_TX_JURO                 DECIMAL(5, 0) NOT NULL,
             PINDIC_ECONM_JURO              DECIMAL(6, 3),
             PTX_INDIC_ECONM                DECIMAL(6, 3) NOT NULL,
             CREGRA_COMIS_CONVE             DECIMAL(3, 0) NOT NULL,
             VCALC_COMIS_CONVE              DECIMAL(17, 2) NOT NULL,
             PCALC_COMIS_CONVE              DECIMAL(5, 2) NOT NULL,
             DDIA_PGTO_COMIS                DECIMAL(2, 0) NOT NULL,
             CINDCD_CONVE_ESPCL             CHAR(1) NOT NULL,
             TRECPC_ARQ_CONVE               DECIMAL(3, 0) NOT NULL,
             CINDCD_PRZ_RECPC               CHAR(2) NOT NULL,
             TARQ_DEB_CONVE                 DECIMAL(3, 0) NOT NULL,
             CINDCD_PRZ_ARQ_DEB             CHAR(2) NOT NULL,
             CINDCD_CONSL_DEB               CHAR(1) NOT NULL,
             CINDCD_CONSL_EXTRT             CHAR(1) NOT NULL,
             CINDCD_MEIO_COMUN              CHAR(1) NOT NULL,
             CDDD_FAX_CONVN                 CHAR(4) NOT NULL,
             CFONE_FAX_CONVN                DECIMAL(8, 0) NOT NULL,
             CRMAL_FAX_CONVN                CHAR(6) NOT NULL,
             EEMAIL_CLI_CONVN               CHAR(70) NOT NULL,
             CSIT_DESC_COML                 DECIMAL(3, 0) NOT NULL,
             HSIT_DESC_COML                 TIMESTAMP NOT NULL,
             DULT_UTILZ_CONVE               DATE NOT NULL,
             DULT_ADTTO_CONVE               DATE NOT NULL,
             CINDCD_MOTVO_ADTTO             CHAR(1) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CCLUB                          DECIMAL(10, 0) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CINDCD_PROTE_TITLO             CHAR(1) NOT NULL,
             QDIA_PROTE_TITLO               DECIMAL(3, 0) NOT NULL,
             CCHAVE_ELMTO_DESC              CHAR(45) NOT NULL,
             PTAC_FIXA_CONVE                DECIMAL(5, 2) NOT NULL,
             CINDCD_PRZ_MAX                 CHAR(2) NOT NULL,
             CINDCD_DIA_PROTE               CHAR(2) NOT NULL,
             CREGRA_GERC_CONS               DECIMAL(3, 0) NOT NULL,
             QDIA_OPER_ATIVO                DECIMAL(4, 0) NOT NULL,
             CINDCD_FREQ_ESTOQ              CHAR(2) NOT NULL,
             CINDCD_OPER_ATRSO              CHAR(1) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONVE_CLI_DESC             *
      ******************************************************************
       01  DCOMB012.
      *    *************************************************************
           10 CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-OPER-CONVE      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-OPER-CONVE       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-RENTB-NEGOC   PIC X(1).
      *    *************************************************************
           10 CPRODT               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINCL-CONVE-CLI      PIC X(10).
      *    *************************************************************
           10 DINIC-VGCIA-CONVE    PIC X(10).
      *    *************************************************************
           10 DVCTO-CONVE-DESC     PIC X(10).
      *    *************************************************************
           10 TCONVE-DESC-COML     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRZ-CONVE     PIC X(2).
      *    *************************************************************
           10 VLIM-CONVE-CLI       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VUTLZD-CONVE-CLI     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VDISPN-CONVE-CLI     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 HATULZ-SDO-CONVE     PIC X(26).
      *    *************************************************************
           10 CINDCD-CONVE-ROTVO   PIC X(1).
      *    *************************************************************
           10 CTPO-COBR-TAC        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-FIXO-TAC      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PERC-TAC      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-MIN-TAC       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-MAX-TAC       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TARIF-REG     PIC X(1).
      *    *************************************************************
           10 CINDCD-PGDOR-JURO    PIC X(1).
      *    *************************************************************
           10 CINDCD-RTEIO-BNEFC   PIC X(1).
      *    *************************************************************
           10 CINDCD-LIBRC-ANTCP   PIC X(1).
      *    *************************************************************
           10 PMAX-LIBRC-ANTCP     PIC S9(3)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 PMAX-ANTVL-VCTO      PIC S9(3)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 TMIN-VCTO-TITLO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 TMAX-VCTO-TITLO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRZ-VCTO      PIC X(2).
      *    *************************************************************
           10 CREGRA-GERC-MORA     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CREGRA-GERC-CRATS    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TX-SPROD      PIC X(1).
      *    *************************************************************
           10 CTPO-TX-JURO-DESC    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-TX-JURO       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 PINDIC-ECONM-JURO    PIC S9(3)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 PTX-INDIC-ECONM      PIC S9(3)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 CREGRA-COMIS-CONVE   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VCALC-COMIS-CONVE    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PCALC-COMIS-CONVE    PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DDIA-PGTO-COMIS      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CONVE-ESPCL   PIC X(1).
      *    *************************************************************
           10 TRECPC-ARQ-CONVE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRZ-RECPC     PIC X(2).
      *    *************************************************************
           10 TARQ-DEB-CONVE       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRZ-ARQ-DEB   PIC X(2).
      *    *************************************************************
           10 CINDCD-CONSL-DEB     PIC X(1).
      *    *************************************************************
           10 CINDCD-CONSL-EXTRT   PIC X(1).
      *    *************************************************************
           10 CINDCD-MEIO-COMUN    PIC X(1).
      *    *************************************************************
           10 CDDD-FAX-CONVN       PIC X(4).
      *    *************************************************************
           10 CFONE-FAX-CONVN      PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CRMAL-FAX-CONVN      PIC X(6).
      *    *************************************************************
           10 EEMAIL-CLI-CONVN     PIC X(70).
      *    *************************************************************
           10 CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HSIT-DESC-COML       PIC X(26).
      *    *************************************************************
           10 DULT-UTILZ-CONVE     PIC X(10).
      *    *************************************************************
           10 DULT-ADTTO-CONVE     PIC X(10).
      *    *************************************************************
           10 CINDCD-MOTVO-ADTTO   PIC X(1).
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
           10 CINDCD-PROTE-TITLO   PIC X(1).
      *    *************************************************************
           10 QDIA-PROTE-TITLO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCHAVE-ELMTO-DESC    PIC X(45).
      *    *************************************************************
           10 PTAC-FIXA-CONVE      PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRZ-MAX       PIC X(2).
      *    *************************************************************
           10 CINDCD-DIA-PROTE     PIC X(2).
      *    *************************************************************
           10 CREGRA-GERC-CONS     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QDIA-OPER-ATIVO      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-FREQ-ESTOQ    PIC X(2).
      *    *************************************************************
           10 CINDCD-OPER-ATRSO    PIC X(1).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 83      *
      ******************************************************************
