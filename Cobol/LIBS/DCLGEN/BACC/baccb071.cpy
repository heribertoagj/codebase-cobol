      ******************************************************************
      * DCLGEN TABLE(DB2PRD.BACC_SALDOS)                               *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB071))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB071)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.BACC_SALDOS TABLE
           ( SALD_CUST_KEY                  CHAR(19) NOT NULL,
             SALD_DTA_VCTO                  DECIMAL(7, 0) NOT NULL,
             SALD_INST_ASS                  DECIMAL(7, 0) NOT NULL,
             SALD_MOEDA                     DECIMAL(1, 0) NOT NULL,
             SALD_TIT_DEP                   DECIMAL(1, 0) NOT NULL,
             SALD_NUMCART                   CHAR(19) NOT NULL,
             SALD_SIST                      DECIMAL(1, 0) NOT NULL,
             SALD_TIPO_PAG                  DECIMAL(1, 0) NOT NULL,
             SALD_AGENCIA                   DECIMAL(5, 0) NOT NULL,
             SALD_CONTA                     DECIMAL(7, 0) NOT NULL,
             SALD_AB_FECH                   CHAR(1) NOT NULL,
             SALD_DTA_DEBT                  DECIMAL(7, 0) NOT NULL,
             SALD_SLD_ANT                   DECIMAL(15, 2) NOT NULL,
             SALD_DEBITO                    DECIMAL(15, 2) NOT NULL,
             SALD_CREDITO                   DECIMAL(15, 2) NOT NULL,
             SALD_PAGTO                     DECIMAL(15, 2) NOT NULL,
             SALD_SLD_REM                   DECIMAL(15, 2) NOT NULL,
             SALD_COMPRAS                   DECIMAL(15, 2) NOT NULL,
             SALD_COMP_PARC                 DECIMAL(15, 2) NOT NULL,
             SALD_TAXAS                     DECIMAL(15, 2) NOT NULL,
             SALD_SAQUES                    DECIMAL(15, 2) NOT NULL,
             SALD_ENC_SAQ                   DECIMAL(15, 2) NOT NULL,
             SALD_ENC_FIN                   DECIMAL(15, 2) NOT NULL,
             SALD_ENC_PARC                  DECIMAL(15, 2) NOT NULL,
             SALD_OUT_DEBT                  DECIMAL(15, 2) NOT NULL,
             SALD_OUT_CRED                  DECIMAL(15, 2) NOT NULL,
             SALD_SLD_ATU                   DECIMAL(15, 2) NOT NULL,
             SALD_PAGTO_MIN                 DECIMAL(15, 2) NOT NULL,
             SALD_VLR_OPCAO                 DECIMAL(15, 2) NOT NULL,
             SALD_FILL                      DECIMAL(15, 2) NOT NULL,
             SALD_LIMITE                    DECIMAL(12, 0) NOT NULL,
             SALD_CCUSTO                    CHAR(10),
             SALD_CGC_CPF                   CHAR(16),
             SALD_NSS_NUM                   DECIMAL(10, 0) NOT NULL,
             SALD_AGE_CED                   DECIMAL(5, 0) NOT NULL,
             SALD_CTA_CED                   DECIMAL(10, 0) NOT NULL,
             SALD_VLR_COT_DOLAR             DECIMAL(15, 3) NOT NULL,
             SALD_DTA_COT_DOLAR             DATE NOT NULL,
             SALD_CT_REAL_DOLAR             DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      *  DB2PRD.BACC_SALDOS                                            *
      ******************************************************************
       01  BACCB071.
      *    *************************************************************
           10 SALD-CUST-KEY        PIC X(19).
      *    *************************************************************
           10 SALD-DTA-VCTO        PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-INST-ASS        PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-MOEDA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-TIT-DEP         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-NUMCART         PIC X(19).
      *    *************************************************************
           10 SALD-SIST            PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-TIPO-PAG        PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-AGENCIA         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-CONTA           PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-AB-FECH         PIC X(1).
      *    *************************************************************
           10 SALD-DTA-DEBT        PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-SLD-ANT         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-DEBITO          PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-CREDITO         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-PAGTO           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-SLD-REM         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-COMPRAS         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-COMP-PARC       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-TAXAS           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-SAQUES          PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-ENC-SAQ         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-ENC-FIN         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-ENC-PARC        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-OUT-DEBT        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-OUT-CRED        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-SLD-ATU         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-PAGTO-MIN       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-VLR-OPCAO       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-FILL            PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 SALD-LIMITE          PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-CCUSTO          PIC X(10).
      *    *************************************************************
           10 SALD-CGC-CPF         PIC X(16).
      *    *************************************************************
           10 SALD-NSS-NUM         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-AGE-CED         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-CTA-CED         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 SALD-VLR-COT-DOLAR   PIC S9(12)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 SALD-DTA-COT-DOLAR   PIC X(10).
      *    *************************************************************
           10 SALD-CT-REAL-DOLAR   PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 39      *
      ******************************************************************
