      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CARTAO_CREDITO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV016))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV016)                                     *
      *        APOST                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CARTAO_CREDITO TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             CSUFXO_CATAO_CREDT             DECIMAL(3, 0) NOT NULL,
             DDIA_DEB                       DECIMAL(2, 0) NOT NULL,
             DCANCT                         DATE,
             VTOT_LIM_COMPR                 DECIMAL(9, 2) NOT NULL,
             IPTDOR_CATAO                   CHAR(30) NOT NULL,
             DVALDD_CATAO                   DECIMAL(6, 0) NOT NULL,
             DDIA_FCHTO_CATAO               DECIMAL(2, 0) NOT NULL,
             CTPO_CATAO_CREDT               DECIMAL(2, 0) NOT NULL,
             VLIM_COMPR_DOLAR               DECIMAL(9, 2) NOT NULL,
             CCANCT_CATAO_CREDT             CHAR(2) NOT NULL,
             CCGC_CPF_CMAST                 DECIMAL(16, 0) NOT NULL,
             CTTLAR_DEPDT_CATAO             DECIMAL(1, 0) NOT NULL,
             CFONE_CMAST                    CHAR(18),
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT_INSTC             CHAR(19) NOT NULL,
             VLIM_SAQUE_CATAO               DECIMAL(9, 2) NOT NULL,
             VLIM_SAQUE_DOLAR               DECIMAL(9, 2) NOT NULL,
             DEMIS                          DATE NOT NULL,
             CTPO_ANUDD_CATAO               CHAR(1) NOT NULL,
             DLAST_CYCLE_CHANGE             DATE,
             DULT_ALT_ENDER                 DATE NOT NULL,
             PTX_ROTVO_ANTER                DECIMAL(6, 2) NOT NULL,
             PTX_ROTVO_ATUAL                DECIMAL(6, 2) NOT NULL,
             PTX_ROTVO_FUTUR                DECIMAL(6, 2) NOT NULL,
             CIDTFD_MOD_CATAO               CHAR(2) NOT NULL,
             CCGC_CPF_CMAST_ST              CHAR(15) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CARTAO_CREDITO           *
      ******************************************************************
       01  BACCV016.
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
           10 CSUFXO-CATAO-CREDT   PIC S9(3)V USAGE COMP-3.
           10 DDIA-DEB             PIC S9(2)V USAGE COMP-3.
           10 DCANCT               PIC X(10).
           10 VTOT-LIM-COMPR       PIC S9(7)V9(2) USAGE COMP-3.
           10 IPTDOR-CATAO         PIC X(30).
           10 DVALDD-CATAO         PIC S9(6)V USAGE COMP-3.
           10 DDIA-FCHTO-CATAO     PIC S9(2)V USAGE COMP-3.
           10 CTPO-CATAO-CREDT     PIC S9(2)V USAGE COMP-3.
           10 VLIM-COMPR-DOLAR     PIC S9(7)V9(2) USAGE COMP-3.
           10 CCANCT-CATAO-CREDT   PIC X(2).
           10 CCGC-CPF-CMAST       PIC S9(16)V USAGE COMP-3.
           10 CTTLAR-DEPDT-CATAO   PIC S9(1)V USAGE COMP-3.
           10 CFONE-CMAST          PIC X(18).
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
           10 CCATAO-CREDT-INSTC   PIC X(19).
           10 VLIM-SAQUE-CATAO     PIC S9(7)V9(2) USAGE COMP-3.
           10 VLIM-SAQUE-DOLAR     PIC S9(7)V9(2) USAGE COMP-3.
           10 DEMIS                PIC X(10).
           10 CTPO-ANUDD-CATAO     PIC X(1).
           10 DLAST-CYCLE-CHANGE   PIC X(10).
           10 DULT-ALT-ENDER       PIC X(10).
           10 PTX-ROTVO-ANTER      PIC S9(4)V9(2) USAGE COMP-3.
           10 PTX-ROTVO-ATUAL      PIC S9(4)V9(2) USAGE COMP-3.
           10 PTX-ROTVO-FUTUR      PIC S9(4)V9(2) USAGE COMP-3.
           10 CIDTFD-MOD-CATAO     PIC X(2).
           10 CCGC-CPF-CMAST-ST    PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 28      *
      ******************************************************************
