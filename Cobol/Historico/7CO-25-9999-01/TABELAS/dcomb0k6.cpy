      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDESPR_CHEQC_DESC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0K6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0K6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDESPR_CHEQC_DESC TABLE
           ( DANO_OPER_DESC                 DECIMAL(4, 0) NOT NULL,
             NSEQ_OPER_DESC                 DECIMAL(9, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_CORR_BCRIA                DECIMAL(11, 0) NOT NULL,
             CCHEQ                          DECIMAL(7, 0) NOT NULL,
             CBCO_CLI_CHEQC                 DECIMAL(3, 0),
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CFLIAL_EMPR_CHEQC              DECIMAL(5, 0) NOT NULL,
             CPROT_LOTE_CHEQC               DECIMAL(7, 0) NOT NULL,
             DMOVTO                         DATE NOT NULL,
             CLOTE_CHEQC                    DECIMAL(7, 0) NOT NULL,
             DVCTO_CHEQ_CTDIA               DATE NOT NULL,
             VCHEQC                         DECIMAL(15, 2) NOT NULL,
             CINDCD_CHEQ_SELEC              CHAR(1) NOT NULL,
             CTPO_LOTE_OPER                 CHAR(1) NOT NULL,
             NPCELA_DESC_COML               DECIMAL(5, 0) NOT NULL,
             RANLSE_CREDT_CLI               CHAR(20) NOT NULL,
             DSOLTC_ANLSE_CREDT             DATE NOT NULL,
             CSIT_ANLSE_CREDT               DECIMAL(3, 0) NOT NULL,
             CINDCD_ISENC_ANLSE             CHAR(1) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             DINCL_REG                      DATE NOT NULL,
             CCNPJ_CPF_EMITT                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_EMITT              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_EMITT               DECIMAL(2, 0) NOT NULL,
             CCNPJ_CPF_EMITT_ST             CHAR(9) NOT NULL,
             CFLIAL_EMITT_ST                CHAR(4) NOT NULL,
             CCTRL_EMITT_ST                 DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDESPR_CHEQC_DESC           *
      ******************************************************************
       01  DCOMB0K6.
      *    *************************************************************
           10 DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR-BCRIA      PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CCHEQ                PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-CLI-CHEQC       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-EMPR-CHEQC    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPROT-LOTE-CHEQC     PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 CLOTE-CHEQC          PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-CHEQ-CTDIA     PIC X(10).
      *    *************************************************************
           10 VCHEQC               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CHEQ-SELEC    PIC X(1).
      *    *************************************************************
           10 CTPO-LOTE-OPER       PIC X(1).
      *    *************************************************************
           10 NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RANLSE-CREDT-CLI     PIC X(20).
      *    *************************************************************
           10 DSOLTC-ANLSE-CREDT   PIC X(10).
      *    *************************************************************
           10 CSIT-ANLSE-CREDT     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ISENC-ANLSE   PIC X(1).
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 DINCL-REG            PIC X(10).
      *    *************************************************************
           10 CCNPJ-CPF-EMITT      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-EMITT    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-EMITT     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-EMITT-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-EMITT-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-EMITT-ST       PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************
