      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSOLTC_ENCRR_CTA_DESC)                     *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0P3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0P3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSOLTC_ENCRR_CTA_DESC TABLE
           ( NSOLTC_ENCRR_CTA               DECIMAL(17, 0) NOT NULL,
             DANO_OPER_DESC                 DECIMAL(4, 0) NOT NULL,
             NSEQ_OPER_DESC                 DECIMAL(9, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CSIT_SOLTC_ENCRR               CHAR(1) NOT NULL,
             DSOLTC_ENCRR_CTA               DATE NOT NULL,
             DPREVT_EFTVC_DSVNC             DATE NOT NULL,
             CPSSOA_JURID_CTA               DECIMAL(10, 0) NOT NULL,
             CTPO_NEGOC_CTA                 DECIMAL(3, 0) NOT NULL,
             NSEQ_NEGOC_CTA                 DECIMAL(10, 0) NOT NULL,
             CTPO_SOLTC_ENCRR               DECIMAL(1, 0) NOT NULL,
             CMOTVO_ENCRR_CTA               DECIMAL(3, 0) NOT NULL,
             CFORMA_DSVNC_CTA               DECIMAL(3, 0) NOT NULL,
             CBCO_SOLCT_ENCRR               DECIMAL(3, 0) NOT NULL,
             CAG_SOLCT_ENCRR                DECIMAL(5, 0) NOT NULL,
             CUSUAR_RESP                    CHAR(9) NOT NULL,
             HATULZ                         TIMESTAMP NOT NULL,
             CTRANS_PROG_ATULZ              CHAR(8) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSOLTC_ENCRR_CTA_DESC       *
      ******************************************************************
       01  DCOMB0P3.
      *    *************************************************************
           10 NSOLTC-ENCRR-CTA     PIC S9(17)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-SOLTC-ENCRR     PIC X(1).
      *    *************************************************************
           10 DSOLTC-ENCRR-CTA     PIC X(10).
      *    *************************************************************
           10 DPREVT-EFTVC-DSVNC   PIC X(10).
      *    *************************************************************
           10 CPSSOA-JURID-CTA     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-NEGOC-CTA       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-NEGOC-CTA       PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-SOLTC-ENCRR     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-ENCRR-CTA     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-DSVNC-CTA     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-SOLCT-ENCRR     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-SOLCT-ENCRR      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      *    *************************************************************
           10 HATULZ               PIC X(26).
      *    *************************************************************
           10 CTRANS-PROG-ATULZ    PIC X(8).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
