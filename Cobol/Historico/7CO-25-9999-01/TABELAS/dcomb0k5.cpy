      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TESTOQ_OPER_ATIVO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0K5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0K5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TESTOQ_OPER_ATIVO TABLE
           ( CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CPRODT                         DECIMAL(3, 0) NOT NULL,
             CSPROD_DESC_COML               DECIMAL(3, 0) NOT NULL,
             CCONTR_CONVE_DESC              DECIMAL(9, 0) NOT NULL,
             CELMTO_DESC_COML               DECIMAL(3, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             QOPER_ATIVO_ESTOQ              DECIMAL(9, 0) NOT NULL,
             VOPER_ATIVO_ESTOQ              DECIMAL(15, 2) NOT NULL,
             DINCL_REG                      DATE,
             CFUNC_BDSCO                    DECIMAL(9, 0),
             CTERM                          CHAR(8),
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TESTOQ_OPER_ATIVO           *
      ******************************************************************
       01  DCOMB0K5.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCONTR-CONVE-DESC    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CELMTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QOPER-ATIVO-ESTOQ    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 VOPER-ATIVO-ESTOQ    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DINCL-REG            PIC X(10).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
