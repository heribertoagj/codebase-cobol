      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMANUT_PPRIE_IMOV_RECOR)                   *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB018))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB018)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMANUT_PPRIE_IMOV_RECOR TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             NPPRIE_IMOV                    DECIMAL(3, 0) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CTPO_PSSOA                     DECIMAL(1, 0) NOT NULL,
             CNIRF_IMOV_RURAL               CHAR(9),
             CCCIR                          CHAR(13),
             CREG_AMBTL_RURAL               CHAR(41),
             PPRESV_AMBTL_PPRIE             DECIMAL(5, 2),
             COUTGA_AGUA_IMOV               CHAR(30),
             DVALDD_OUTGA_AGUA              DATE,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(5) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMANUT_PPRIE_IMOV_RECOR     *
      ******************************************************************
       01  RUCAB018.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 NPPRIE-IMOV          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PSSOA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CNIRF-IMOV-RURAL     PIC X(9).
      *    *************************************************************
           10 CCCIR                PIC X(13).
      *    *************************************************************
           10 CREG-AMBTL-RURAL     PIC X(41).
      *    *************************************************************
           10 PPRESV-AMBTL-PPRIE   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 COUTGA-AGUA-IMOV     PIC X(30).
      *    *************************************************************
           10 DVALDD-OUTGA-AGUA    PIC X(10).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(5).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
