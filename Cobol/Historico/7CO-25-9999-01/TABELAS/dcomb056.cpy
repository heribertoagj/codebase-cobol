      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSACDO_ISENC_CDENT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB056))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB056)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSACDO_ISENC_CDENT TABLE
           ( CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             NSEQ_PSSOA_DESC                DECIMAL(3, 0) NOT NULL,
             CTPO_ISENC_DESC                DECIMAL(3, 0) NOT NULL,
             DINIC_VGCIA_ISENC              DATE NOT NULL,
             CCNPJ_CPF_PROPN                DECIMAL(9, 0) NOT NULL,
             DFIM_VGCIA_ISENC               DATE NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CFLIAL_CNPJ_PROPN              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_PROPN               DECIMAL(2, 0) NOT NULL,
             CCLUB_CDENT_PROPN              DECIMAL(10, 0) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL,
             CCNPJ_CPF_PROPN_ST             CHAR(9) NOT NULL,
             CFLIAL_PROPN_ST                CHAR(4) NOT NULL,
             CCTRL_PROPN_ST                 DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSACDO_ISENC_CDENT          *
      ******************************************************************
       01  DCOMB056.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-PSSOA-DESC      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ISENC-DESC      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-ISENC    PIC X(10).
      *    *************************************************************
           10 CCNPJ-CPF-PROPN      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-VGCIA-ISENC     PIC X(10).
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-PROPN    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-PROPN     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCLUB-CDENT-PROPN    PIC S9(10)V USAGE COMP-3.
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
      *    *************************************************************
           10 CCNPJ-CPF-PROPN-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-PROPN-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-PROPN-ST       PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
