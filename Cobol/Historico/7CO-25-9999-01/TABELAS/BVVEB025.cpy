      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSOLTC_TRAVA_DOMCL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB025))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB025)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSOLTC_TRAVA_DOMCL TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CPTO_VDA_ESTBL                 DECIMAL(9, 0) NOT NULL,
             CTRAVA_DOMCL_COML              CHAR(1) NOT NULL,
             CTPO_OPER_FINCR                CHAR(1) NOT NULL,
             CIDTFD_BANDE_ESTBL             CHAR(2) NOT NULL,
             DINIC_TRAVA_DOMCL              DATE NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             DFNAL_TRAVA_DOMCL              DATE NOT NULL,
             DINCL_ESTBL_COML               DATE NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CSIT_SOLTC_PCERO               CHAR(1) NOT NULL,
             CAVISO_RETOR_TRAVA             DECIMAL(5, 0),
             RMSGEM_RETOR_TRAVA             CHAR(60),
             DFNAL_TRAVA_PCERO              DATE,
             HENVIO_INFO_PCERO              TIMESTAMP,
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSOLTC_TRAVA_DOMCL          *
      ******************************************************************
       01  BVVEB025.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPTO-VDA-ESTBL       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTRAVA-DOMCL-COML    PIC X(1).
      *    *************************************************************
           10 CTPO-OPER-FINCR      PIC X(1).
      *    *************************************************************
           10 CIDTFD-BANDE-ESTBL   PIC X(2).
      *    *************************************************************
           10 DINIC-TRAVA-DOMCL    PIC X(10).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DFNAL-TRAVA-DOMCL    PIC X(10).
      *    *************************************************************
           10 DINCL-ESTBL-COML     PIC X(10).
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-SOLTC-PCERO     PIC X(1).
      *    *************************************************************
           10 CAVISO-RETOR-TRAVA   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RMSGEM-RETOR-TRAVA   PIC X(60).
      *    *************************************************************
           10 DFNAL-TRAVA-PCERO    PIC X(10).
      *    *************************************************************
           10 HENVIO-INFO-PCERO    PIC X(26).
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
