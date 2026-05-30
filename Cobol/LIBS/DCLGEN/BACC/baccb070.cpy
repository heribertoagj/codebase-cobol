      ******************************************************************
      * DCLGEN TABLE(DB2PRD.BACC_TRANS)                                *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB070))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB070)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.BACC_TRANS TABLE
           ( TRANS_CUST_KEY                 CHAR(19) NOT NULL,
             TRANS_DTA_VCTO                 DECIMAL(7, 0) NOT NULL,
             TRANS_INST_ASS                 DECIMAL(7, 0) NOT NULL,
             TRANS_MOEDA                    DECIMAL(1, 0) NOT NULL,
             TRANS_TIT_DEP                  DECIMAL(1, 0) NOT NULL,
             TRANS_NUMCART                  CHAR(19) NOT NULL,
             TRANS_SIST                     DECIMAL(1, 0) NOT NULL,
             TRANS_TIPO_PAG                 DECIMAL(1, 0) NOT NULL,
             TRANS_AB_FECH                  CHAR(1) NOT NULL,
             TRANS_AGENCIA                  DECIMAL(5, 0) NOT NULL,
             TRANS_CONTA                    DECIMAL(7, 0) NOT NULL,
             TRANS_DTA_COMP                 DECIMAL(7, 0) NOT NULL,
             TRANS_DTA_MOV                  DECIMAL(7, 0) NOT NULL,
             TRANS_ORGPAG                   DECIMAL(7, 0) NOT NULL,
             TRANS_DOC                      CHAR(23) NOT NULL,
             TRANS_TIPO_OPE                 CHAR(4) NOT NULL,
             TRANS_PARCELAS                 DECIMAL(5, 0),
             TRANS_VLR_COMP                 DECIMAL(13, 2) NOT NULL,
             TRANS_VLR_DEBT                 DECIMAL(13, 2) NOT NULL,
             TRANS_DTA_DEBT                 DECIMAL(7, 0) NOT NULL,
             TRANS_VEND                     DECIMAL(13, 0),
             TRANS_DESC                     CHAR(35) NOT NULL,
             TRANS_MOE_ORIG                 CHAR(3),
             TRANS_CID_ORIG                 CHAR(13),
             TRANS_EST_ORIG                 CHAR(2),
             TRANS_VLR_ORIG                 DECIMAL(15, 3),
             TRANS_CCUSTO                   CHAR(10),
             TRANS_CGC_CPF                  CHAR(16),
             TRANS_RATIV                    DECIMAL(7, 0),
             TRANS_PERC_JUR                 DECIMAL(5, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      *  DB2PRD.BACC_TRANS                                             *
      ******************************************************************
       01  BACCB070.
      *    *************************************************************
           10 TRANS-CUST-KEY       PIC X(19).
      *    *************************************************************
           10 TRANS-DTA-VCTO       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-INST-ASS       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-MOEDA          PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-TIT-DEP        PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-NUMCART        PIC X(19).
      *    *************************************************************
           10 TRANS-SIST           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-TIPO-PAG       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-AB-FECH        PIC X(1).
      *    *************************************************************
           10 TRANS-AGENCIA        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-CONTA          PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-DTA-COMP       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-DTA-MOV        PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-ORGPAG         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-DOC            PIC X(23).
      *    *************************************************************
           10 TRANS-TIPO-OPE       PIC X(4).
      *    *************************************************************
           10 TRANS-PARCELAS       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-VLR-COMP       PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 TRANS-VLR-DEBT       PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 TRANS-DTA-DEBT       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-VEND           PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-DESC           PIC X(35).
      *    *************************************************************
           10 TRANS-MOE-ORIG       PIC X(3).
      *    *************************************************************
           10 TRANS-CID-ORIG       PIC X(13).
      *    *************************************************************
           10 TRANS-EST-ORIG       PIC X(2).
      *    *************************************************************
           10 TRANS-VLR-ORIG       PIC S9(12)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 TRANS-CCUSTO         PIC X(10).
      *    *************************************************************
           10 TRANS-CGC-CPF        PIC X(16).
      *    *************************************************************
           10 TRANS-RATIV          PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TRANS-PERC-JUR       PIC S9(3)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
