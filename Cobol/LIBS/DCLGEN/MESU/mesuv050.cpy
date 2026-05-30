      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V50DEPDC_BDSCO)                            *
      *        LIBRARY(AD.DB2.DCLGEN(MESUV050))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(MESUV050)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V50DEPDC_BDSCO TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             IDEPDC_BDSCO                   CHAR(20) NOT NULL,
             ELOGDR_DEPDC_BDSCO             CHAR(30) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CFLIAL_CGC                     DECIMAL(5, 0) NOT NULL,
             CCTRL_CGC_DEPDC                CHAR(2) NOT NULL,
             IMUN_DEPDC                     CHAR(25) NOT NULL,
             DOCUPC_ESTBL                   DATE,
             DINAUG_DEPDC_BDSCO             DATE,
             DPOSSE_GER                     DECIMAL(6, 0) NOT NULL,
             IFUNC_GER                      CHAR(30) NOT NULL,
             CCONDC_IMOV                    CHAR(1) NOT NULL,
             CCATEG_AG_PNERA                CHAR(1) NOT NULL,
             CIDTFD_CONDC_RSUMO             CHAR(1) NOT NULL,
             CPROCS_TPO_TRNSM               CHAR(1) NOT NULL,
             CTPO_PROCS_DEPDC               CHAR(1) NOT NULL,
             CCONDC_EMIS_RELAT              CHAR(1) NOT NULL,
             CRGNAL_DIR                     DECIMAL(3, 0) NOT NULL,
             CRGNAL_PSSOA                   DECIMAL(3, 0) NOT NULL,
             CAREA_INSP_DEPDC               DECIMAL(3, 0) NOT NULL,
             CDIR_RGNAL                     DECIMAL(3, 0) NOT NULL,
             CDISTR_CORSP                   DECIMAL(5, 0) NOT NULL,
             CLOC_BALAN                     DECIMAL(5, 0) NOT NULL,
             CTESOU                         DECIMAL(5, 0) NOT NULL,
             CCMARA_COMPE                   DECIMAL(3, 0) NOT NULL,
             CINTGC_CMARA_COMPE             CHAR(1) NOT NULL,
             CJUNC_DEPDC_CRS                DECIMAL(5, 0) NOT NULL,
             CJUNC_DEPDC_SCTRO              DECIMAL(5, 0) NOT NULL,
             CLIN_TRNSM                     DECIMAL(5, 0) NOT NULL,
             CJUNC_DEPDC_ELETR              DECIMAL(5, 0) NOT NULL,
             CJUNC_DEPDC_INC                DECIMAL(5, 0) NOT NULL,
             DENCRR_DEPDC_BDSCO             DATE,
             CSEDE_INSP_ESPEC               DECIMAL(5, 0) NOT NULL,
             CJUNC_DEPDC_BDSCO              DECIMAL(5, 0) NOT NULL,
             CFONE                          DECIMAL(7, 0) NOT NULL,
             CRMAL_DEPDC_BDSCO              DECIMAL(5, 0) NOT NULL,
             CDDD_DEPDC_BDSCO               DECIMAL(3, 0) NOT NULL,
             CRGIAO_GEOGR                   DECIMAL(3, 0) NOT NULL,
             CDIFDR_AG_PNERA                CHAR(1) NOT NULL,
             IMUN_REDZD                     CHAR(14) NOT NULL,
             CSEDE_INSP_GRAL                DECIMAL(5, 0) NOT NULL,
             CONLINE_VIA_SATLT              CHAR(1) NOT NULL,
             DONLINE_OFIC                   DATE,
             DAPROV_TOT_LOG                 DATE,
             IDEPDC_BDSCO_CATAO             CHAR(20) NOT NULL,
             CID_MUN                        DECIMAL(7, 0) NOT NULL,
             CDIG_DEPDC                     CHAR(1) NOT NULL,
             CTELEX_DEPDC_BDSCO             DECIMAL(7, 0) NOT NULL,
             CTELEX_SUFXO_BDSCO             CHAR(4) NOT NULL,
             CDIVIS_MERC                    CHAR(2) NOT NULL,
             CJUNC_DIR_RGNAL                DECIMAL(5, 0) NOT NULL,
             CAG_VIA_SATLT                  CHAR(1) NOT NULL,
             DAG_PDRAO                      DATE,
             CINDCD_TERM_GERC               DECIMAL(3, 0) NOT NULL,
             CCEP_COPLT                     DECIMAL(9, 0) NOT NULL,
             CEXIST_RETAG                   CHAR(1) NOT NULL,
             QHORA_FUSO                     DECIMAL(3, 0) NOT NULL,
             CFONE_NOVO                     DECIMAL(8, 0) NOT NULL,
             PONTO_ATENDIMENTO              DECIMAL(2, 0) NOT NULL,
             SIND_JURISDICAO                DECIMAL(3, 0) NOT NULL,
             CD_GERENTE_AGENCIA             DECIMAL(8, 0) NOT NULL,
             N_TELESALDO                    DECIMAL(8, 0) NOT NULL,
             N_FAXEXTRATO                   DECIMAL(8, 0) NOT NULL,
             CCGC_CPF_ST                    CHAR(9) NOT NULL,
             CFLIAL_CGC_ST                  CHAR(5) NOT NULL,
             CCTRL_CGC_DEPDC_ST             CHAR(2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V50DEPDC_BDSCO              *
      ******************************************************************
       01  MESUV050.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 IDEPDC-BDSCO         PIC X(20).
      *    *************************************************************
           10 ELOGDR-DEPDC-BDSCO   PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CGC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CGC-DEPDC      PIC X(2).
      *    *************************************************************
           10 IMUN-DEPDC           PIC X(25).
      *    *************************************************************
           10 DOCUPC-ESTBL         PIC X(10).
      *    *************************************************************
           10 DINAUG-DEPDC-BDSCO   PIC X(10).
      *    *************************************************************
           10 DPOSSE-GER           PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 IFUNC-GER            PIC X(30).
      *    *************************************************************
           10 CCONDC-IMOV          PIC X(1).
      *    *************************************************************
           10 CCATEG-AG-PNERA      PIC X(1).
      *    *************************************************************
           10 CIDTFD-CONDC-RSUMO   PIC X(1).
      *    *************************************************************
           10 CPROCS-TPO-TRNSM     PIC X(1).
      *    *************************************************************
           10 CTPO-PROCS-DEPDC     PIC X(1).
      *    *************************************************************
           10 CCONDC-EMIS-RELAT    PIC X(1).
      *    *************************************************************
           10 CRGNAL-DIR           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CRGNAL-PSSOA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAREA-INSP-DEPDC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDIR-RGNAL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDISTR-CORSP         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CLOC-BALAN           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTESOU               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCMARA-COMPE         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINTGC-CMARA-COMPE   PIC X(1).
      *    *************************************************************
           10 CJUNC-DEPDC-CRS      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC-SCTRO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CLIN-TRNSM           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC-ELETR    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC-INC      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DENCRR-DEPDC-BDSCO   PIC X(10).
      *    *************************************************************
           10 CSEDE-INSP-ESPEC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC-BDSCO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFONE                PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CRMAL-DEPDC-BDSCO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDDD-DEPDC-BDSCO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-GEOGR         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDIFDR-AG-PNERA      PIC X(1).
      *    *************************************************************
           10 IMUN-REDZD           PIC X(14).
      *    *************************************************************
           10 CSEDE-INSP-GRAL      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CONLINE-VIA-SATLT    PIC X(1).
      *    *************************************************************
           10 DONLINE-OFIC         PIC X(10).
      *    *************************************************************
           10 DAPROV-TOT-LOG       PIC X(10).
      *    *************************************************************
           10 IDEPDC-BDSCO-CATAO   PIC X(20).
      *    *************************************************************
           10 CID-MUN              PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-DEPDC           PIC X(1).
      *    *************************************************************
           10 CTELEX-DEPDC-BDSCO   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CTELEX-SUFXO-BDSCO   PIC X(4).
      *    *************************************************************
           10 CDIVIS-MERC          PIC X(2).
      *    *************************************************************
           10 CJUNC-DIR-RGNAL      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-VIA-SATLT        PIC X(1).
      *    *************************************************************
           10 DAG-PDRAO            PIC X(10).
      *    *************************************************************
           10 CINDCD-TERM-GERC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COPLT           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CEXIST-RETAG         PIC X(1).
      *    *************************************************************
           10 QHORA-FUSO           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFONE-NOVO           PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 PONTO-ATENDIMENTO    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 SIND-JURISDICAO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CD-GERENTE-AGENCIA   PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 N-TELESALDO          PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 N-FAXEXTRATO         PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-ST          PIC X(9).
      *    *************************************************************
           10 CFLIAL-CGC-ST        PIC X(5).
      *    *************************************************************
           10 CCTRL-CGC-DEPDC-ST   PIC X(2).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 68      *
      ******************************************************************
