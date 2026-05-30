      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOTE_RPROC_PGTO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0K1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0K1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOTE_RPROC_PGTO TABLE
           ( NSEQ_REG_MOVTO                 DECIMAL(12, 0) NOT NULL,
             DPROCM_MOVTO_BASIL             DATE NOT NULL,
             CRPROC_MOVTO_BASIL             CHAR(1) NOT NULL,
             CTPO_LYOUT_CABEC               CHAR(1),
             CCTRO_CUSTO                    CHAR(4),
             NSEQ_REG_CABEC                 DECIMAL(12, 0) NOT NULL,
             CERRO_REG_CABEC                CHAR(32) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CTPO_LYOUT_RODAP               CHAR(1) NOT NULL,
             QREG_DETLH_MOVTO               DECIMAL(9, 0),
             QREG_TOT_MOVTO                 DECIMAL(9, 0) NOT NULL,
             NSEQ_REG_RODAP                 DECIMAL(12, 0) NOT NULL,
             CERRO_REG_RODAP                CHAR(32) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0),
             CTERM                          CHAR(8) NOT NULL,
             DINCL_REG                      DATE NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOTE_RPROC_PGTO            *
      ******************************************************************
       01  DCOMB0K1.
      *    *************************************************************
           10 NSEQ-REG-MOVTO       PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 DPROCM-MOVTO-BASIL   PIC X(10).
      *    *************************************************************
           10 CRPROC-MOVTO-BASIL   PIC X(1).
      *    *************************************************************
           10 CTPO-LYOUT-CABEC     PIC X(1).
      *    *************************************************************
           10 CCTRO-CUSTO          PIC X(4).
      *    *************************************************************
           10 NSEQ-REG-CABEC       PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 CERRO-REG-CABEC      PIC X(32).
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-LYOUT-RODAP     PIC X(1).
      *    *************************************************************
           10 QREG-DETLH-MOVTO     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QREG-TOT-MOVTO       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-REG-RODAP       PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 CERRO-REG-RODAP      PIC X(32).
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 DINCL-REG            PIC X(10).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 22      *
      ******************************************************************
