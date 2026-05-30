      ******************************************************************
      * DCLGEN TABLE(DB2PRD.COMPR_CATAO_BNDES)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB089))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB089)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.COMPR_CATAO_BNDES TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             CDADOS_IDTFD_TRANS             DECIMAL(11, 0) NOT NULL,
             CSEQ_IDTFD_TRANS               DECIMAL(12, 0) NOT NULL,
             DCOMPR_BNDES                   DATE NOT NULL,
             CAUTRZ_COMPR                   CHAR(6) NOT NULL,
             CCONTR_CATAO_BNDES             DECIMAL(11, 0),
             DLIBRC_CONTR                   DATE,
             VCOMPR_BNDES                   DECIMAL(14, 2) NOT NULL,
             VFINAN_BNDES                   DECIMAL(14, 2) NOT NULL,
             VLIQDD_CONTR                   DECIMAL(14, 2),
             CFORNC_CGC_NRO                 DECIMAL(9, 0) NOT NULL,
             CFORNC_CGC_FLIAL               DECIMAL(5, 0) NOT NULL,
             CFORNC_CGC_CTRL                CHAR(2) NOT NULL,
             CCLI_CGC_NRO                   DECIMAL(9, 0) NOT NULL,
             CCLI_CGC_FLIAL                 DECIMAL(5, 0) NOT NULL,
             CCLI_CGC_CTRL                  CHAR(2) NOT NULL,
             CIDTFD_TRANS                   DECIMAL(15, 0) NOT NULL,
             DULT_PGTO                      DATE,
             CMOTVO_CANCT                   CHAR(3),
             VPCELA_CONTR                   DECIMAL(14, 2) NOT NULL,
             CSIT_CONTR_BNDES               DECIMAL(3, 0) NOT NULL,
             VTX_JURO_CONTR                 DECIMAL(5, 2) NOT NULL,
             QPCELA_CONTR                   DECIMAL(3, 0) NOT NULL,
             CULT_ALT_CATAO                 CHAR(8),
             DULT_ALT                       DATE,
             CESTBL_CONVN_BNDES             CHAR(15) NOT NULL,
             CFORNC_CGC_NRO_ST              CHAR(9) NOT NULL,
             CFORNC_CGC_FLIAL_ST            CHAR(4) NOT NULL,
             CFORNC_CGC_CTRL_ST             CHAR(2) NOT NULL,
             CCLI_CGC_NRO_ST                CHAR(9) NOT NULL,
             CCLI_CGC_FLIAL_ST              CHAR(4) NOT NULL,
             CCLI_CGC_CTRL_ST               CHAR(2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.COMPR_CATAO_BNDES           *
      ******************************************************************
       01  BACCB089.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CDADOS-IDTFD-TRANS   PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-IDTFD-TRANS     PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 DCOMPR-BNDES         PIC X(10).
      *    *************************************************************
           10 CAUTRZ-COMPR         PIC X(6).
      *    *************************************************************
           10 CCONTR-CATAO-BNDES   PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 DLIBRC-CONTR         PIC X(10).
      *    *************************************************************
           10 VCOMPR-BNDES         PIC S9(12)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VFINAN-BNDES         PIC S9(12)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VLIQDD-CONTR         PIC S9(12)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CFORNC-CGC-NRO       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFORNC-CGC-FLIAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFORNC-CGC-CTRL      PIC X(2).
      *    *************************************************************
           10 CCLI-CGC-NRO         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCLI-CGC-FLIAL       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCLI-CGC-CTRL        PIC X(2).
      *    *************************************************************
           10 CIDTFD-TRANS         PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 DULT-PGTO            PIC X(10).
      *    *************************************************************
           10 CMOTVO-CANCT         PIC X(3).
      *    *************************************************************
           10 VPCELA-CONTR         PIC S9(12)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSIT-CONTR-BNDES     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VTX-JURO-CONTR       PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QPCELA-CONTR         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CULT-ALT-CATAO       PIC X(8).
      *    *************************************************************
           10 DULT-ALT             PIC X(10).
      *    *************************************************************
           10 CESTBL-CONVN-BNDES   PIC X(15).
      *    *************************************************************
           10 CFORNC-CGC-NRO-ST    PIC X(9).
      *    *************************************************************
           10 CFORNC-CGC-FLIAL-ST  PIC X(4).
      *    *************************************************************
           10 CFORNC-CGC-CTRL-ST   PIC X(2).
      *    *************************************************************
           10 CCLI-CGC-NRO-ST      PIC X(9).
      *    *************************************************************
           10 CCLI-CGC-FLIAL-ST    PIC X(4).
      *    *************************************************************
           10 CCLI-CGC-CTRL-ST     PIC X(2).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 33      *
      ******************************************************************
