      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_CHESP)                                *
      *        LIBRARY(AD.DB2.DCLGEN(SCMPB003))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SCMPB003)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_CHESP TABLE
           ( CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             DINIC_BENEF                    DATE NOT NULL,
             CCTRL_CGC_CPF                  DECIMAL(2, 0) NOT NULL,
             DFNAL_BENEF                    DATE NOT NULL,
             VLIM_CREDT_CLI                 DECIMAL(15, 2) NOT NULL,
             PREDC_PER                      DECIMAL(5, 2) NOT NULL,
             VSDOMD_TMES_POUP               DECIMAL(15, 2) NOT NULL,
             VSDOMD_TMES_FUNDO              DECIMAL(15, 2) NOT NULL,
             VSDOMD_TMES_ACAO               DECIMAL(15, 0) NOT NULL,
             VTX_DIA                        DECIMAL(13, 8) NOT NULL,
             CCGC_CPF_ST                    CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_CHESP                  *
      ******************************************************************
       01  SCMPB003.
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-BENEF          PIC X(10).
      *    *************************************************************
           10 CCTRL-CGC-CPF        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DFNAL-BENEF          PIC X(10).
      *    *************************************************************
           10 VLIM-CREDT-CLI       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PREDC-PER            PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDOMD-TMES-POUP     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDOMD-TMES-FUNDO    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDOMD-TMES-ACAO     PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 VTX-DIA              PIC S9(5)V9(8) USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-ST          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
