      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONVE_CTRL_FORNC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB097))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB097)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONVE_CTRL_FORNC TABLE
           ( CCONVE_CLI_DESC                DECIMAL(9, 0) NOT NULL,
             CVRSAO_CONVE_CLI               DECIMAL(3, 0) NOT NULL,
             CCNPJ_CPF_FORNC                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_FORNC              DECIMAL(5, 0) NOT NULL,
             DVCTO_PGTO_FORNC               DATE NOT NULL,
             CCTRL_CNPJ_FORNC               DECIMAL(2, 0) NOT NULL,
             VUTLZD_VCTO_FORNC              DECIMAL(17, 2) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CCNPJ_CPF_FORNC_ST             CHAR(9) NOT NULL,
             CFLIAL_FORNC_ST                CHAR(4) NOT NULL,
             CCTRL_FORNC_ST                 DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONVE_CTRL_FORNC           *
      ******************************************************************
       01  DCOMB097.
      *    *************************************************************
           10 CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-FORNC      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-FORNC    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-PGTO-FORNC     PIC X(10).
      *    *************************************************************
           10 CCTRL-CNPJ-FORNC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VUTLZD-VCTO-FORNC    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CCNPJ-CPF-FORNC-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-FORNC-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-FORNC-ST       PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
