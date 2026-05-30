      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOPER_ISENC_PSSOA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB036))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB036)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOPER_ISENC_PSSOA TABLE
           ( CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             NSEQ_PSSOA_DESC                DECIMAL(3, 0) NOT NULL,
             CTPO_ISENC_DESC                DECIMAL(3, 0) NOT NULL,
             DINIC_VGCIA_ISENC              DATE NOT NULL,
             DANO_OPER_DESC                 DECIMAL(4, 0) NOT NULL,
             NSEQ_OPER_DESC                 DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CCLUB                          DECIMAL(10, 0) NOT NULL,
             CINDCD_PSSOA_DESC              CHAR(2) NOT NULL,
             DFIM_VGCIA_ISENC               DATE NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOPER_ISENC_PSSOA           *
      ******************************************************************
       01  DCOMB036.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-PSSOA-DESC      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ISENC-DESC      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-ISENC    PIC X(10).
      *    *************************************************************
           10 DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCLUB                PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PSSOA-DESC    PIC X(2).
      *    *************************************************************
           10 DFIM-VGCIA-ISENC     PIC X(10).
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
