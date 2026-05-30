      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSPROD_ISENC_PSSOA)                        *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB063))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB063)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSPROD_ISENC_PSSOA TABLE
           ( CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             NSEQ_PSSOA_DESC                DECIMAL(3, 0) NOT NULL,
             CTPO_ISENC_DESC                DECIMAL(3, 0) NOT NULL,
             DINIC_VGCIA_ISENC              DATE NOT NULL,
             CPRODT                         DECIMAL(3, 0) NOT NULL,
             CSPROD_DESC_COML               DECIMAL(3, 0) NOT NULL,
             DFIM_VGCIA_ISENC               DATE NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CCLUB                          DECIMAL(10, 0) NOT NULL,
             CINDCD_PSSOA_DESC              CHAR(2) NOT NULL,
             CINDCD_REST_VLR                CHAR(1) NOT NULL,
             VREST_MAX_OPER                 DECIMAL(17, 2) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSPROD_ISENC_PSSOA          *
      ******************************************************************
       01  DCOMB063.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-PSSOA-DESC      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ISENC-DESC      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-ISENC    PIC X(10).
      *    *************************************************************
           10 CPRODT               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-VGCIA-ISENC     PIC X(10).
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCLUB                PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PSSOA-DESC    PIC X(2).
      *    *************************************************************
           10 CINDCD-REST-VLR      PIC X(1).
      *    *************************************************************
           10 VREST-MAX-OPER       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
