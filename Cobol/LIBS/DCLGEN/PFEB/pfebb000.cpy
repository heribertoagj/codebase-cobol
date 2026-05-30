***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CLIENTE_PAGADOR)                           *
      *        LIBRARY(AD.DB2.DCLGEN(PFEBB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(PFEBB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CLIENTE_PAGADOR TABLE
           ( CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CFLIAL_CGC                     DECIMAL(5, 0) NOT NULL,
             CCTRL_CGC                      DECIMAL(2, 0) NOT NULL,
             ICLI                           CHAR(40),
             ECLI                           CHAR(40),
             CCEP_NOVO                      DECIMAL(5, 0),
             CCEP_COMPL                     DECIMAL(3, 0),
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CRZ_CTBIL                      DECIMAL(5, 0) NOT NULL,
             CIDTFD_ACSSO                   DECIMAL(9, 0) NOT NULL,
             CCONTR                         DECIMAL(9, 0) NOT NULL,
             CGRP_EMPR                      DECIMAL(6, 0) NOT NULL,
             DCADTO_CLI                     DATE NOT NULL,
             DCANCT_CONVE                   DATE,
             DCONF_DEB                      DATE,
             CCTRL_NRO_PGTO                 CHAR(16),
             CINDCD_PROD_TESTE              CHAR(1) NOT NULL,
             EINTRN_CORSP_EMPR              CHAR(30) NOT NULL,
             CJUNC_DEPDC_TARIF              DECIMAL(5, 0),
             CCTA_CORR_TARIF                DECIMAL(7, 0),
             CRZ_CTBIL_TARIF                DECIMAL(5, 0),
             DINIC_LIM                      DATE NOT NULL,
             DFNAL_LIM                      DATE NOT NULL,
             VLIM_CREDT                     DECIMAL(15, 2) NOT NULL,
             STTUS_ALT_VLR_LIM              DECIMAL(2, 0) NOT NULL,
             CFUNC_ATULZ_LIM                DECIMAL(9, 0) NOT NULL,
             HULT_ATULZ_LIM                 TIMESTAMP NOT NULL,
             CSIT_LIM_CLI                   DECIMAL(1, 0) NOT NULL,
             CINDCD_CONS_SDO                CHAR(1) NOT NULL,
             CINDCD_CHEQ_VINC               CHAR(1) NOT NULL,
             CIND_CONS_SDO_TED              CHAR(1),
             CIND_COMPS_SDO_TED             CHAR(1),
             VLIM_PGTO_TED                  DECIMAL(15, 2),
             CTPO_CLI_PGDOR                 DECIMAL(3, 0),
             CIDTFD_LISTA_DEB               CHAR(1),
             CIDTFD_AVISO_INDVD             CHAR(1),
             CDSTNO_AVISO                   DECIMAL(1, 0),
             CINDCD_HABLT_PGMD              CHAR(1) NOT NULL,
             CCONVE_CTA_SALRL               DECIMAL(13, 0) NOT NULL,
             DINIC_VGCIA_CONVE              DATE,
             CIDTFD_TPO_CONVE               CHAR(1),
             CIDTFD_SERVC_ESPCL             CHAR(1),
             CINDCD_TRATO_ESPCL             CHAR(1) NOT NULL,
             CCTCIA_LISTA_DEB               CHAR(1),
             CCONS_TITLO_SUPE               CHAR(1) NOT NULL,
             CCOMPS_TITLO_SUPE              CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CLIENTE_PAGADOR             *
      ******************************************************************
       01  PFEBB000.
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CGC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CGC            PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 ICLI                 PIC X(40).
      *    *************************************************************
           10 ECLI                 PIC X(40).
      *    *************************************************************
           10 CCEP-NOVO            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CRZ-CTBIL            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-ACSSO         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-EMPR            PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DCADTO-CLI           PIC X(10).
      *    *************************************************************
           10 DCANCT-CONVE         PIC X(10).
      *    *************************************************************
           10 DCONF-DEB            PIC X(10).
      *    *************************************************************
           10 CCTRL-NRO-PGTO       PIC X(16).
      *    *************************************************************
           10 CINDCD-PROD-TESTE    PIC X(1).
      *    *************************************************************
           10 EINTRN-CORSP-EMPR    PIC X(30).
      *    *************************************************************
           10 CJUNC-DEPDC-TARIF    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR-TARIF      PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CRZ-CTBIL-TARIF      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-LIM            PIC X(10).
      *    *************************************************************
           10 DFNAL-LIM            PIC X(10).
      *    *************************************************************
           10 VLIM-CREDT           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 STTUS-ALT-VLR-LIM    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-ATULZ-LIM      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ-LIM       PIC X(26).
      *    *************************************************************
           10 CSIT-LIM-CLI         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CONS-SDO      PIC X(1).
      *    *************************************************************
           10 CINDCD-CHEQ-VINC     PIC X(1).
      *    *************************************************************
           10 CIND-CONS-SDO-TED    PIC X(1).
      *    *************************************************************
           10 CIND-COMPS-SDO-TED   PIC X(1).
      *    *************************************************************
           10 VLIM-PGTO-TED        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CLI-PGDOR       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LISTA-DEB     PIC X(1).
      *    *************************************************************
           10 CIDTFD-AVISO-INDVD   PIC X(1).
      *    *************************************************************
           10 CDSTNO-AVISO         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-HABLT-PGMD    PIC X(1).
      *    *************************************************************
           10 CCONVE-CTA-SALRL     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-CONVE    PIC X(10).
      *    *************************************************************
           10 CIDTFD-TPO-CONVE     PIC X(1).
      *    *************************************************************
           10 CIDTFD-SERVC-ESPCL   PIC X(1).
      *    *************************************************************
           10 CINDCD-TRATO-ESPCL   PIC X(1).
      *    *************************************************************
           10 CCTCIA-LISTA-DEB     PIC X(1).
      *    *************************************************************
           10 CCONS-TITLO-SUPE     PIC X(1).
      *    *************************************************************
           10 CCOMPS-TITLO-SUPE    PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      ******************************************************************
