      ******************************************************************
      * DCLGEN TABLE(DB2PRD.OBJE_CAD_POUPADOR)                         *
      *        LIBRARY(AD.DB2.DCLGEN(OBJEB010))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(OBJEB010)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.OBJE_CAD_POUPADOR TABLE
           ( POUPADOR_AGENCIA               DECIMAL(5, 0) NOT NULL,
             POUPADOR_CONTA                 DECIMAL(7, 0) NOT NULL,
             POUPADOR_NOME_CLIE             CHAR(39) NOT NULL,
             POUPADOR_ALERTA                CHAR(1) NOT NULL,
             POUPADOR_END_CLIE              CHAR(40) NOT NULL,
             POUPADOR_CEP                   DECIMAL(5, 0) NOT NULL,
             POUPADOR_TELEFONE              DECIMAL(7, 0) NOT NULL,
             POUPADOR_SEXO                  CHAR(1) NOT NULL,
             POUPADOR_DT_NASC               DECIMAL(7, 0) NOT NULL,
             POUPADOR_CGC_CPF               DECIMAL(9, 0) NOT NULL,
             POUPADOR_FILIAL                DECIMAL(5, 0) NOT NULL,
             POUPADOR_CONTROLE              CHAR(2) NOT NULL,
             POUPADOR_RAZAO                 DECIMAL(5, 0) NOT NULL,
             POUPADOR_RAMO_AT               DECIMAL(5, 0) NOT NULL,
             POUPADOR_TIPO_CTA              CHAR(2) NOT NULL,
             POUPADOR_PESSOA                CHAR(1) NOT NULL,
             POUPADOR_COD_GRUP              DECIMAL(5, 0) NOT NULL,
             POUPADOR_TP_TRI                CHAR(1) NOT NULL,
             POUPADOR_DT_QBMES              DECIMAL(7, 0) NOT NULL,
             POUPADOR_PSTO_SER              DECIMAL(3, 0) NOT NULL,
             POUPADOR_DT_ABERT              DECIMAL(7, 0) NOT NULL,
             CDDD                           CHAR(4) NOT NULL,
             CFONE                          DECIMAL(8, 0) NOT NULL,
             CDDD_FAX                       CHAR(4) NOT NULL,
             CFAX                           DECIMAL(8, 0) NOT NULL,
             DNASC                          DATE NOT NULL,
             DABERT_CTA                     DATE NOT NULL,
             DULT_ACERT                     DATE NOT NULL,
             DRCADT                         DATE NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CCEP_NOVO_COMPL                DECIMAL(3, 0) NOT NULL,
             CID_CLI                        CHAR(26) NOT NULL,
             CRAMO_ATVDD_CNPJ               DECIMAL(7, 0) NOT NULL,
             ELOGDR                         CHAR(40) NOT NULL,
             ENRO_IMOV                      CHAR(7) NOT NULL,
             ECOMPL_NRO_LOGDR               CHAR(20) NOT NULL,
             EBAIRO_LOGDR                   CHAR(20) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             NLIN_TFONI                     DECIMAL(11, 0),
             NLIN_TFONI_FAX                 DECIMAL(11, 0),
             POUPADOR_CGC_ST                CHAR(9) NOT NULL,
             POUPADOR_FILIAL_ST             CHAR(4) NOT NULL,
             POUPADOR_CONTR_ST              CHAR(2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.OBJE_CAD_POUPADOR           *
      ******************************************************************
       01  OBJEB010.
      *    *************************************************************
           10 POUPADOR-AGENCIA     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-CONTA       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-NOME-CLIE   PIC X(39).
      *    *************************************************************
           10 POUPADOR-ALERTA      PIC X(1).
      *    *************************************************************
           10 POUPADOR-END-CLIE    PIC X(40).
      *    *************************************************************
           10 POUPADOR-CEP         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-TELEFONE    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-SEXO        PIC X(1).
      *    *************************************************************
           10 POUPADOR-DT-NASC     PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-CGC-CPF     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-FILIAL      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-CONTROLE    PIC X(2).
      *    *************************************************************
           10 POUPADOR-RAZAO       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-RAMO-AT     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-TIPO-CTA    PIC X(2).
      *    *************************************************************
           10 POUPADOR-PESSOA      PIC X(1).
      *    *************************************************************
           10 POUPADOR-COD-GRUP    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-TP-TRI      PIC X(1).
      *    *************************************************************
           10 POUPADOR-DT-QBMES    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-PSTO-SER    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-DT-ABERT    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CDDD                 PIC X(4).
      *    *************************************************************
           10 CFONE                PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CDDD-FAX             PIC X(4).
      *    *************************************************************
           10 CFAX                 PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 DNASC                PIC X(10).
      *    *************************************************************
           10 DABERT-CTA           PIC X(10).
      *    *************************************************************
           10 DULT-ACERT           PIC X(10).
      *    *************************************************************
           10 DRCADT               PIC X(10).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-NOVO-COMPL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CID-CLI              PIC X(26).
      *    *************************************************************
           10 CRAMO-ATVDD-CNPJ     PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 ELOGDR               PIC X(40).
      *    *************************************************************
           10 ENRO-IMOV            PIC X(7).
      *    *************************************************************
           10 ECOMPL-NRO-LOGDR     PIC X(20).
      *    *************************************************************
           10 EBAIRO-LOGDR         PIC X(20).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NLIN-TFONI           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 NLIN-TFONI-FAX       PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 POUPADOR-CGC-ST      PIC X(9).
      *    *************************************************************
           10 POUPADOR-FILIAL-ST   PIC X(4).
      *    *************************************************************
           10 POUPADOR-CONTR-ST    PIC X(2).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 44      *
      ******************************************************************
