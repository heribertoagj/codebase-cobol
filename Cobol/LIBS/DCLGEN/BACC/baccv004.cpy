      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CATAO_CREDT)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV004))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV004)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CATAO_CREDT TABLE
           ( CCATAO_CREDT_INSTC             CHAR(19) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             DALT_STTUS_CATAO               DATE NOT NULL,
             DVALDD_CATAO                   DATE NOT NULL,
             QRENOV_CATAO                   DECIMAL(1, 0) NOT NULL,
             ECORSP_CATAO                   CHAR(40) NOT NULL,
             ICIDDE_CATAO                   CHAR(30) NOT NULL,
             CUF                            CHAR(3) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CVDDOR_CATAO                   DECIMAL(9, 0) NOT NULL,
             CCATAO_ANTER                   CHAR(19) NOT NULL,
             VBONUS_MOEDA                   DECIMAL(15, 2) NOT NULL,
             CCPF_PTDOR                     DECIMAL(11, 0) NOT NULL,
             CSTTUS_CATAO                   CHAR(2) NOT NULL,
             CTTLAR_DEPDT_CATAO             DECIMAL(1, 0) NOT NULL,
             CFAIXA_COBR_ANUDD              DECIMAL(1, 0) NOT NULL,
             CPTDOR_PG_ANUDD                CHAR(1) NOT NULL,
             DINCL_PTDOR_CREMT              DATE NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             DANO_VEIC                      DECIMAL(4, 0) NOT NULL,
             CCHASI_VEIC                    CHAR(17) NOT NULL,
             DCOMPR_VEIC                    DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CATAO_CREDT              *
      ******************************************************************
       01  BACCV004.
      *    *************************************************************
           10 CCATAO-CREDT-INSTC   PIC X(19).
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DALT-STTUS-CATAO     PIC X(10).
      *    *************************************************************
           10 DVALDD-CATAO         PIC X(10).
      *    *************************************************************
           10 QRENOV-CATAO         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 ECORSP-CATAO         PIC X(40).
      *    *************************************************************
           10 ICIDDE-CATAO         PIC X(30).
      *    *************************************************************
           10 CUF                  PIC X(3).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CVDDOR-CATAO         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-ANTER         PIC X(19).
      *    *************************************************************
           10 VBONUS-MOEDA         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCPF-PTDOR           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CSTTUS-CATAO         PIC X(2).
      *    *************************************************************
           10 CTTLAR-DEPDT-CATAO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFAIXA-COBR-ANUDD    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPTDOR-PG-ANUDD      PIC X(1).
      *    *************************************************************
           10 DINCL-PTDOR-CREMT    PIC X(10).
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-VEIC            PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCHASI-VEIC          PIC X(17).
      *    *************************************************************
           10 DCOMPR-VEIC          PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 22      *
      ******************************************************************
