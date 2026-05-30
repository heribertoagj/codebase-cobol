      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPPRIE_ESTBL_COML)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB021))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB021)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPPRIE_ESTBL_COML TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CCPF_PPRIE                     DECIMAL(9, 0) NOT NULL,
             CCTRL_CPF_PPRIE                DECIMAL(2, 0) NOT NULL,
             IPPRIE_ESTBL_AFLIA             CHAR(32) NOT NULL,
             DNASC_PPRIE                    DATE NOT NULL,
             PPRTCP_ACAO_PPRIE              DECIMAL(3, 2) NOT NULL,
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPPRIE_ESTBL_COML           *
      ******************************************************************
       01  BVVEB021.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCPF-PPRIE           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-PPRIE      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IPPRIE-ESTBL-AFLIA   PIC X(32).
      *    *************************************************************
           10 DNASC-PPRIE          PIC X(10).
      *    *************************************************************
           10 PPRTCP-ACAO-PPRIE    PIC S9(1)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
