      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MASTCARD_ORPAG)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB051))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB051)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MASTCARD_ORPAG TABLE
           ( DJULI                          DECIMAL(7, 0) NOT NULL,
             CTPO_CAPTC_ORPAG               DECIMAL(2, 0) NOT NULL,
             CSEQ_ORPAG                     DECIMAL(7, 0) NOT NULL,
             CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CREMSS_ORPAG                   DECIMAL(7, 0) NOT NULL,
             CORPAG_BACC                    DECIMAL(7, 0) NOT NULL,
             CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             CSUFXO_CATAO_CREDT             DECIMAL(3, 0) NOT NULL,
             CPREFX_ESTBL_BACC              DECIMAL(4, 0) NOT NULL,
             CBCO_VISA                      DECIMAL(3, 0) NOT NULL,
             CESTBL_CONVN_BACC              DECIMAL(6, 0) NOT NULL,
             DCOMPR                         DATE NOT NULL,
             VPRINC_ORPAG                   DECIMAL(13, 2) NOT NULL,
             VOUTRO_SERVC_ORPAG             DECIMAL(13, 2) NOT NULL,
             QPCELA_ORPAG                   DECIMAL(2, 0) NOT NULL,
             CMEIO_ENTRD_TRANS              DECIMAL(1, 0) NOT NULL,
             CAUTRZ_VDA_CATAO               CHAR(6) NOT NULL,
             DOCOR                          DATE NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CGRP_DSBTO_ORPAG               DECIMAL(4, 0) NOT NULL,
             CPRIOR_TRATO_REJEI             DECIMAL(2, 0) NOT NULL,
             CSIT_ORPAG                     DECIMAL(1, 0) NOT NULL,
             CVARVL_ORPAG_REMSS             CHAR(3) NOT NULL,
             WVARVL_ORPAG_REMSS             CHAR(47) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MASTCARD_ORPAG              *
      ******************************************************************
       01  BACCB051.
      *    *************************************************************
           10 DJULI                PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CAPTC-ORPAG     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-ORPAG           PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CREMSS-ORPAG         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CORPAG-BACC          PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSUFXO-CATAO-CREDT   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPREFX-ESTBL-BACC    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-VISA            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CESTBL-CONVN-BACC    PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DCOMPR               PIC X(10).
      *    *************************************************************
           10 VPRINC-ORPAG         PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VOUTRO-SERVC-ORPAG   PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QPCELA-ORPAG         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CMEIO-ENTRD-TRANS    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CAUTRZ-VDA-CATAO     PIC X(6).
      *    *************************************************************
           10 DOCOR                PIC X(10).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-DSBTO-ORPAG     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CPRIOR-TRATO-REJEI   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-ORPAG           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CVARVL-ORPAG-REMSS   PIC X(3).
      *    *************************************************************
           10 WVARVL-ORPAG-REMSS   PIC X(47).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
