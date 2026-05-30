      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ERRO_PPSTA_AFLIA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB007))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB007)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ERRO_PPSTA_AFLIA TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CSEQ_ERRO                      DECIMAL(3, 0) NOT NULL,
             ICLUNA_ERRO                    CHAR(18) NOT NULL,
             CERRO_PREEN_AFLIA              DECIMAL(2, 0) NOT NULL,
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ERRO_PPSTA_AFLIA            *
      ******************************************************************
       01  BVVEB007.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-ERRO            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 ICLUNA-ERRO          PIC X(18).
      *    *************************************************************
           10 CERRO-PREEN-AFLIA    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
