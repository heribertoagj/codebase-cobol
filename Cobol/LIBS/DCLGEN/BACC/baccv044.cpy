      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01FAT_CATAO_CREDT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV044))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV044)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01FAT_CATAO_CREDT TABLE
           ( CTPO_FAT_CATAO                 DECIMAL(1, 0) NOT NULL,
             CANO_NOSSO_NRO                 DECIMAL(3, 0) NOT NULL,
             CNOSSO_NRO                     DECIMAL(9, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CDIG_CTA_CORR                  CHAR(1) NOT NULL,
             CCART_COBR                     DECIMAL(3, 0) NOT NULL,
             CIND_MOEDA                     DECIMAL(1, 0) NOT NULL,
             CJUNC_DEPDC_SCTRO              DECIMAL(5, 0) NOT NULL,
             CDIG_DEPDC_SCTRO               CHAR(1) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CDIG_DEPDC                     CHAR(1) NOT NULL,
             IPTDOR_CATAO                   CHAR(30) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             CCATAO_CREDT_INSTC             CHAR(19) NOT NULL,
             VFAT_CATAO_CREDT               DECIMAL(15, 2) NOT NULL,
             VPGTO_MIN_FAT                  DECIMAL(15, 2) NOT NULL,
             DVCTO_FAT_CATAO                DATE NOT NULL,
             DLIM_PGTO_FAT                  DATE NOT NULL,
             DDIA_FCHTO_CATAO               DECIMAL(2, 0) NOT NULL,
             PROTVO_CATAO_CREDT             DECIMAL(5, 2) NOT NULL,
             PMORA_CATAO_CREDT              DECIMAL(5, 2) NOT NULL,
             VCOTACAO_DOLAR                 DECIMAL(15, 3) NOT NULL,
             DCOTACAO_DOLAR                 DATE NOT NULL,
             SCATAO_R_DLAR_CONV             DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01FAT_CATAO_CREDT          *
      ******************************************************************
       01  BACCV044.
      *    *************************************************************
           10 CTPO-FAT-CATAO       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CANO-NOSSO-NRO       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CNOSSO-NRO           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-CTA-CORR        PIC X(1).
      *    *************************************************************
           10 CCART-COBR           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIND-MOEDA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC-SCTRO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-DEPDC-SCTRO     PIC X(1).
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-DEPDC           PIC X(1).
      *    *************************************************************
           10 IPTDOR-CATAO         PIC X(30).
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT-INSTC   PIC X(19).
      *    *************************************************************
           10 VFAT-CATAO-CREDT     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPGTO-MIN-FAT        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-FAT-CATAO      PIC X(10).
      *    *************************************************************
           10 DLIM-PGTO-FAT        PIC X(10).
      *    *************************************************************
           10 DDIA-FCHTO-CATAO     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 PROTVO-CATAO-CREDT   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PMORA-CATAO-CREDT    PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCOTACAO-DOLAR       PIC S9(12)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 DCOTACAO-DOLAR       PIC X(10).
      *    *************************************************************
           10 SCATAO-R-DLAR-CONV   PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
