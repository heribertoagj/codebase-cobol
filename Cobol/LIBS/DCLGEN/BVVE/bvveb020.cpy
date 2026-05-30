      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAFLIA_ESTBL_COML)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB020))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB020)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAFLIA_ESTBL_COML TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             IESTBL_AFLIA                   CHAR(32) NOT NULL,
             IPLAQT_EQPMT_ESTBL             CHAR(22) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CSIT_AFLIA_PCERO               CHAR(1),
             CSIT_PPSTA_AFLIA               DECIMAL(2, 0),
             CCNPJ_CPF_CONTT                DECIMAL(9, 0),
             CFLIAL_CNPJ_CONTT              DECIMAL(5, 0),
             CCTRL_CNPJ_CONTT               DECIMAL(2, 0),
             CRAMO_ATVDD_ESTBL              DECIMAL(5, 0) NOT NULL,
             CCRM_RESP_ESTBL                DECIMAL(6, 0),
             CCRO_RESP_ESTBL                DECIMAL(6, 0),
             ICNTAT_ESTBL_AFLIA             CHAR(32) NOT NULL,
             CENDER_COML_IGUAL              CHAR(1) NOT NULL,
             CDDD_FONE_ESTBL                CHAR(4) NOT NULL,
             CFONE_ESTBL_COML               DECIMAL(8, 0) NOT NULL,
             CDDD_FAX_ESTBL                 CHAR(4),
             CFONE_FAX_ESTBL                DECIMAL(8, 0),
             EEMAIL_CNTAT_ESTBL             CHAR(50),
             CTPO_FUNCN_ESTBL               CHAR(1) NOT NULL,
             CTPO_ESTBL_COML                CHAR(1) NOT NULL,
             CPTO_VDA_MTRIZ                 DECIMAL(9, 0),
             CPTO_VDA_CTLZA                 DECIMAL(9, 0),
             CLOC_PGTO_ESTBL                CHAR(1) NOT NULL,
             CLOC_ESTBL_COML                CHAR(1) NOT NULL,
             DINCL_ESTBL_COML               DATE NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CPTO_VDA_ESTBL                 DECIMAL(9, 0),
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             DAFLIA_ESTBL_COML              DATE,
             CFUNC_ALT_ESTBL                DECIMAL(9, 0),
             DALT_ESTBL_COML                DATE,
             CMOTVO_RCUSA_AFLIA             DECIMAL(5, 0),
             CEMPR_TERC_CONTT               DECIMAL(4, 0),
             NLIN_TFONI_COML                DECIMAL(11, 0),
             NLIN_FAX_COML                  DECIMAL(11, 0),
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL,
             CCNPJ_CPF_CONTT_ST             CHAR(9),
             CFLIAL_CONTT_ST                CHAR(4),
             CCTRL_CONTT_ST                 DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAFLIA_ESTBL_COML           *
      ******************************************************************
       01  BVVEB020.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IESTBL-AFLIA         PIC X(32).
      *    *************************************************************
           10 IPLAQT-EQPMT-ESTBL   PIC X(22).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-AFLIA-PCERO     PIC X(1).
      *    *************************************************************
           10 CSIT-PPSTA-AFLIA     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-CONTT      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-CONTT    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CONTT     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CRAMO-ATVDD-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCRM-RESP-ESTBL      PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCRO-RESP-ESTBL      PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 ICNTAT-ESTBL-AFLIA   PIC X(32).
      *    *************************************************************
           10 CENDER-COML-IGUAL    PIC X(1).
      *    *************************************************************
           10 CDDD-FONE-ESTBL      PIC X(4).
      *    *************************************************************
           10 CFONE-ESTBL-COML     PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CDDD-FAX-ESTBL       PIC X(4).
      *    *************************************************************
           10 CFONE-FAX-ESTBL      PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 EEMAIL-CNTAT-ESTBL   PIC X(50).
      *    *************************************************************
           10 CTPO-FUNCN-ESTBL     PIC X(1).
      *    *************************************************************
           10 CTPO-ESTBL-COML      PIC X(1).
      *    *************************************************************
           10 CPTO-VDA-MTRIZ       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CPTO-VDA-CTLZA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CLOC-PGTO-ESTBL      PIC X(1).
      *    *************************************************************
           10 CLOC-ESTBL-COML      PIC X(1).
      *    *************************************************************
           10 DINCL-ESTBL-COML     PIC X(10).
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPTO-VDA-ESTBL       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DAFLIA-ESTBL-COML    PIC X(10).
      *    *************************************************************
           10 CFUNC-ALT-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DALT-ESTBL-COML      PIC X(10).
      *    *************************************************************
           10 CMOTVO-RCUSA-AFLIA   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPR-TERC-CONTT     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NLIN-TFONI-COML      PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 NLIN-FAX-COML        PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-CONTT-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-CONTT-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CONTT-ST       PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      ******************************************************************
