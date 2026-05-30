      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THIST_MEIO_LIBRC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0B4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0B4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THIST_MEIO_LIBRC TABLE
           ( DANO_OPER_DESC                 DECIMAL(4, 0) NOT NULL,
             NSEQ_OPER_DESC                 DECIMAL(9, 0) NOT NULL,
             CMEIO_LIBRC_DESC               DECIMAL(3, 0) NOT NULL,
             NOPER_MEIO_LIBRC               DECIMAL(3, 0) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CSIT_DESC_COML                 DECIMAL(3, 0) NOT NULL,
             HSIT_DESC_COML                 TIMESTAMP NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(3, 0) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             IPSSOA_DESC_COML               CHAR(60) NOT NULL,
             CCLUB                          DECIMAL(10, 0) NOT NULL,
             VLIBRC_DESC_COML               DECIMAL(17, 2) NOT NULL,
             PLIBRC_DESC_COML               DECIMAL(6, 3) NOT NULL,
             DLIBRC_DESC_COML               DATE NOT NULL,
             CINDCD_LIBRC_ANTCP             CHAR(1) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CINDCD_LIBRC_CANAL             CHAR(1) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THIST_MEIO_LIBRC            *
      ******************************************************************
       01  DCOMB0B4.
      *    *************************************************************
           10 DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CMEIO-LIBRC-DESC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NOPER-MEIO-LIBRC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HSIT-DESC-COML       PIC X(26).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IPSSOA-DESC-COML     PIC X(60).
      *    *************************************************************
           10 CCLUB                PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 VLIBRC-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PLIBRC-DESC-COML     PIC S9(3)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 DLIBRC-DESC-COML     PIC X(10).
      *    *************************************************************
           10 CINDCD-LIBRC-ANTCP   PIC X(1).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CINDCD-LIBRC-CANAL   PIC X(1).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
