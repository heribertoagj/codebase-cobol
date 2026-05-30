      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSOLTC_TRNSF_DOMCL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB026))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB026)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSOLTC_TRNSF_DOMCL TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CPTO_VDA_ESTBL                 DECIMAL(9, 0) NOT NULL,
             CTPO_OPER_FINCR                CHAR(1) NOT NULL,
             CIDTFD_BANDE_ESTBL             CHAR(2) NOT NULL,
             DINCL_ESTBL_COML               DATE NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CBCO_ORIGE_ESTBL               DECIMAL(3, 0),
             CAG_ORIGE_ESTBL                DECIMAL(5, 0),
             CCTA_ORIGE_ESTBL               DECIMAL(13, 0),
             CDIG_CTA_ORIGE                 CHAR(1),
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CSIT_SOLTC_PCERO               CHAR(1) NOT NULL,
             DRETOR_SOLTC_PCERO             DATE,
             CTRAVA_DOMCL_ESTBL             CHAR(1),
             CPTO_VDA_CTLZA                 DECIMAL(9, 0),
             HENVIO_INFO_PCERO              TIMESTAMP,
             CEMPR_TERC_CONTT               DECIMAL(4, 0),
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSOLTC_TRNSF_DOMCL          *
      ******************************************************************
       01  BVVEB026.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPTO-VDA-ESTBL       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OPER-FINCR      PIC X(1).
      *    *************************************************************
           10 CIDTFD-BANDE-ESTBL   PIC X(2).
      *    *************************************************************
           10 DINCL-ESTBL-COML     PIC X(10).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-ORIGE-ESTBL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-ORIGE-ESTBL      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-ORIGE-ESTBL     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-CTA-ORIGE       PIC X(1).
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-SOLTC-PCERO     PIC X(1).
      *    *************************************************************
           10 DRETOR-SOLTC-PCERO   PIC X(10).
      *    *************************************************************
           10 CTRAVA-DOMCL-ESTBL   PIC X(1).
      *    *************************************************************
           10 CPTO-VDA-CTLZA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HENVIO-INFO-PCERO    PIC X(26).
      *    *************************************************************
           10 CEMPR-TERC-CONTT     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
