      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PPRIE_ESTBL_AFLIA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB010))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB010)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PPRIE_ESTBL_AFLIA TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CCPF_PPRIE                     DECIMAL(9, 0) NOT NULL,
             CCTRL_CPF_PPRIE                DECIMAL(2, 0) NOT NULL,
             IPPRIE_ESTBL_AFLIA             CHAR(32) NOT NULL,
             DNASC_PPRIE                    DATE NOT NULL,
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PPRIE_ESTBL_AFLIA           *
      ******************************************************************
       01  BVVEB010.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCPF-PPRIE           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-PPRIE      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IPPRIE-ESTBL-AFLIA   PIC X(32).
      *    *************************************************************
           10 DNASC-PPRIE          PIC X(10).
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
