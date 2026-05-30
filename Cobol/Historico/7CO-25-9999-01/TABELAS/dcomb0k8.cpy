      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDESPR_TITLO_OPER)                         *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0K8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0K8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDESPR_TITLO_OPER TABLE
           ( DANO_OPER_DESC                 DECIMAL(4, 0) NOT NULL,
             NSEQ_OPER_DESC                 DECIMAL(9, 0) NOT NULL,
             NTITLO_DESC_COML               DECIMAL(5, 0) NOT NULL,
             CBCO_PRODT_COBR                DECIMAL(3, 0) NOT NULL,
             CIDTFD_PRODT_COBR              DECIMAL(2, 0),
             CNEGOC_COBR                    DECIMAL(18, 0) NOT NULL,
             CTITLO_COBR_BCO                DECIMAL(18, 0) NOT NULL,
             CNRO_SEQ_TITLO                 DECIMAL(3, 0) NOT NULL,
             NNOTA_PROMS_RURAL              DECIMAL(11, 0) NOT NULL,
             CINDCD_NOTA_PROMS              CHAR(3) NOT NULL,
             CTPO_PROTR_RURAL               DECIMAL(2, 0) NOT NULL,
             CPRODT_RURAL                   DECIMAL(3, 0) NOT NULL,
             QPRODT_RURAL                   DECIMAL(9, 0) NOT NULL,
             CUND_MEDD                      DECIMAL(3, 0) NOT NULL,
             CINDCD_PGDOR_JURO              CHAR(1) NOT NULL,
             DEMIS_TITLO                    DATE NOT NULL,
             DVCTO_TITLO_DESC               DATE NOT NULL,
             VTITLO_DESC_COML               DECIMAL(17, 2) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             IPSSOA_SACDO                   CHAR(60) NOT NULL,
             CSEU_NRO_TITLO                 CHAR(15) NOT NULL,
             CESPCE_TITLO_COBR              DECIMAL(3, 0) NOT NULL,
             CINDCD_INSTR_DESC              DECIMAL(1, 0) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             ELOGDR                         CHAR(30) NOT NULL,
             IBAIRO                         CHAR(40) NOT NULL,
             IMUN                           CHAR(30) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL,
             CSIT_DESC_COML                 DECIMAL(3, 0) NOT NULL,
             HSIT_DESC_COML                 TIMESTAMP NOT NULL,
             VABTMT_TITLO_DESC              DECIMAL(17, 2) NOT NULL,
             VORPAG_RCBVL                   DECIMAL(17, 2) NOT NULL,
             NPCELA_DESC_COML               DECIMAL(5, 0) NOT NULL,
             CINDCD_SELEC_DESC              CHAR(1) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             DINCL_REG                      DATE NOT NULL,
             CORIGE_REC                     DECIMAL(3, 0) NOT NULL,
             CBCO_DEPOS                     DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA_DEPOS                DECIMAL(5, 0) NOT NULL,
             RANLSE_CREDT_CLI               CHAR(20) NOT NULL,
             DSOLTC_ANLSE_CREDT             DATE NOT NULL,
             CSIT_ANLSE_CREDT               DECIMAL(3, 0) NOT NULL,
             CINDCD_ISENC_ANLSE             CHAR(1) NOT NULL,
             CINDCD_PROTE_TITLO             CHAR(1) NOT NULL,
             QDIA_PROTE_TITLO               DECIMAL(3, 0) NOT NULL,
             CBCO_DEB_AUTOM                 DECIMAL(3, 0) NOT NULL,
             CAG_DEB_AUTOM                  DECIMAL(5, 0) NOT NULL,
             CCTA_DEB_AUTOM                 DECIMAL(13, 0) NOT NULL,
             CIDTFD_PRODT_ORIGN             DECIMAL(2, 0) NOT NULL,
             CADM_CATAO_CREDT               DECIMAL(9, 0) NOT NULL,
             CINDCD_VDA_RCBVL               CHAR(1) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDESPR_TITLO_OPER           *
      ******************************************************************
       01  DCOMB0K8.
      *    *************************************************************
           10 DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NTITLO-DESC-COML     PIC S9(5)V USAGE COMP-3.
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
           10 NNOTA-PROMS-RURAL    PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-NOTA-PROMS    PIC X(3).
      *    *************************************************************
           10 CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QPRODT-RURAL         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PGDOR-JURO    PIC X(1).
      *    *************************************************************
           10 DEMIS-TITLO          PIC X(10).
      *    *************************************************************
           10 DVCTO-TITLO-DESC     PIC X(10).
      *    *************************************************************
           10 VTITLO-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IPSSOA-SACDO         PIC X(60).
      *    *************************************************************
           10 CSEU-NRO-TITLO       PIC X(15).
      *    *************************************************************
           10 CESPCE-TITLO-COBR    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-INSTR-DESC    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 ELOGDR               PIC X(30).
      *    *************************************************************
           10 IBAIRO               PIC X(40).
      *    *************************************************************
           10 IMUN                 PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HSIT-DESC-COML       PIC X(26).
      *    *************************************************************
           10 VABTMT-TITLO-DESC    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VORPAG-RCBVL         PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
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
           10 CORIGE-REC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-DEPOS           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA-DEPOS      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RANLSE-CREDT-CLI     PIC X(20).
      *    *************************************************************
           10 DSOLTC-ANLSE-CREDT   PIC X(10).
      *    *************************************************************
           10 CSIT-ANLSE-CREDT     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ISENC-ANLSE   PIC X(1).
      *    *************************************************************
           10 CINDCD-PROTE-TITLO   PIC X(1).
      *    *************************************************************
           10 QDIA-PROTE-TITLO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-DEB-AUTOM       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-DEB-AUTOM        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-DEB-AUTOM       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-ORIGN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CADM-CATAO-CREDT     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-VDA-RCBVL     PIC X(1).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 59      *
      ******************************************************************
