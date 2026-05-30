      ******************************************************************
      * DCLGEN TABLE(DB2PRD.FONE_ESTBL_AFLIA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB009))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB009)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.FONE_ESTBL_AFLIA TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CTPO_ENDER_AFLIA               DECIMAL(1, 0) NOT NULL,
             CTPO_FONE_ESTBL                DECIMAL(1, 0) NOT NULL,
             CSEQ_FONE_PPRIE                DECIMAL(1, 0) NOT NULL,
             CDDD_FONE_PPRIE                CHAR(4),
             CFONE_PPRIE                    DECIMAL(8, 0),
             NLIN_TFONI_COML                DECIMAL(11, 0),
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.FONE_ESTBL_AFLIA            *
      ******************************************************************
       01  BVVEB009.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ENDER-AFLIA     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-FONE-ESTBL      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-FONE-PPRIE      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CDDD-FONE-PPRIE      PIC X(4).
      *    *************************************************************
           10 CFONE-PPRIE          PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 NLIN-TFONI-COML      PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
