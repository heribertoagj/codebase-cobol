      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMANUT_CONTR_RECOR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB015))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB015)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMANUT_CONTR_RECOR TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CSIT_ALT_REG                   DECIMAL(1, 0) NOT NULL,
             VFINAN                         DECIMAL(15, 2) NOT NULL,
             CESPCE_TITLO_GARNT             DECIMAL(3, 0),
             CTPO_PROTR_RURAL               DECIMAL(2, 0),
             CORIGE_REC                     DECIMAL(3, 0),
             VRECTA_BRUTA_EMPTO             DECIMAL(15, 2),
             CMUN_RURAL                     DECIMAL(9, 0),
             MAREA_TOT                      DECIMAL(9, 2),
             QUND_FINCD_RURAL               DECIMAL(11, 2),
             CPRODT_RURAL                   DECIMAL(3, 0),
             QUND_PROD_PROVV                DECIMAL(11, 2),
             DINIC_PLATI                    DATE,
             DFIM_PLATI                     DATE,
             DINIC_COLHE                    DATE,
             DFIM_COLHE                     DATE,
             CTPO_AGROP_RURAL               DECIMAL(5, 0),
             CTPO_INTGC_RURAL               DECIMAL(5, 0),
             CINDCD_UND_PRDTV               DECIMAL(1, 0),
             CTPO_IGCAO_RURAL               DECIMAL(5, 0),
             CTPO_CTIVO_RURAL               DECIMAL(5, 0),
             CCICLO_PROD_RURAL              DECIMAL(5, 0),
             VPCELA_LIBRC                   DECIMAL(15, 2),
             VGARNT_RENDA_MIN               DECIMAL(15, 2),
             PJURO_ENCAR_FINCR              DECIMAL(8, 5),
             DEVNTO                         DATE,
             CTPO_EVNTO_RURAL               DECIMAL(4, 0),
             CFUNC_BDSCO_AUTRZ              DECIMAL(9, 0),
             PRISCO_FUNDO_CNSTT             DECIMAL(5, 2),
             PRISCO_TSORO_NACIO             DECIMAL(5, 2),
             PJURO_POS_ENCAR                DECIMAL(5, 2),
             CTPO_ENCAR_COMPL               DECIMAL(2, 0),
             NBASE_LEGAL_RENEG              DECIMAL(5, 0),
             DANO_BASE                      DECIMAL(4, 0),
             CJUNC_DEPDC                    DECIMAL(5, 0),
             CSEQ_STUDO_NEGOC               DECIMAL(9, 0),
             DVCTO_CONTR                    DATE NOT NULL,
             DMOVTO_RECOR                   DATE NOT NULL,
             CFUNC_ATULZ                    DECIMAL(9, 0) NOT NULL,
             CCETFC_ORGNC_EMPTO             CHAR(80),
             EURL_CETFC_ORGNC               VARCHAR(256),
             CRSTRB_EMPTO_RURAL             CHAR(80),
             EURL_RSTRB_EMPTO               VARCHAR(256),
             PENERG_RENOV_EMPTO             DECIMAL(5, 2),
             CTPO_SOLO_AGCLT                DECIMAL(2, 0),
             CCICLO_CTVDA                   DECIMAL(2, 0),
             CMANUT_SOLO                    DECIMAL(2, 0),
             CTPO_SEGUR_CONTT               DECIMAL(1, 0),
             CSCRET_TSORO_NACIO             CHAR(13),
             CCLIMT_RURAL_BACEN             DECIMAL(2, 0),
             COUTRO_TRATO_SOLO              DECIMAL(5, 0),
             CINDCD_CONFM_FLORS             DECIMAL(1, 0),
             CINDCD_CONFM_IBAMA             DECIMAL(1, 0),
             CINDCD_CONFM_CSVAO             DECIMAL(1, 0),
             RREGLZ_IBAMA                   VARCHAR(300),
             COUTRO_TPO_CONFM               DECIMAL(5, 0),
             CCNPJ_INSTC_EXIGB              DECIMAL(8, 0),
             CINDCD_CONFM_RURAL             CHAR(1),
             CFLIAL_CNPJ_EXIGB              DECIMAL(4, 0),
             CCTRL_CNPJ_EXIGB               DECIMAL(2, 0),
             QUND_ANMAL                     DECIMAL(11, 2),
             PBONUS_SUSTBD                  DECIMAL(5, 2),
             CCNPJ_INSTC_EXIGB_ST           CHAR(8),
             CFLIAL_CNPJ_EXIGB_ST           CHAR(4),
             CCTRL_CNPJ_EXIGB_ST            DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMANUT_CONTR_RECOR          *
      ******************************************************************
       01  RUCAB015.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CSIT-ALT-REG         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VFINAN               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CESPCE-TITLO-GARNT   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VRECTA-BRUTA-EMPTO   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CMUN-RURAL           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 MAREA-TOT            PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QUND-FINCD-RURAL     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QUND-PROD-PROVV      PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DINIC-PLATI          PIC X(10).
      *    *************************************************************
           10 DFIM-PLATI           PIC X(10).
      *    *************************************************************
           10 DINIC-COLHE          PIC X(10).
      *    *************************************************************
           10 DFIM-COLHE           PIC X(10).
      *    *************************************************************
           10 CTPO-AGROP-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-INTGC-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-UND-PRDTV     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-IGCAO-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CTIVO-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCICLO-PROD-RURAL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VPCELA-LIBRC         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VGARNT-RENDA-MIN     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PJURO-ENCAR-FINCR    PIC S9(3)V9(5) USAGE COMP-3.
      *    *************************************************************
           10 DEVNTO               PIC X(10).
      *    *************************************************************
           10 CTPO-EVNTO-RURAL     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO-AUTRZ    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 PRISCO-FUNDO-CNSTT   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PRISCO-TSORO-NACIO   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PJURO-POS-ENCAR      PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ENCAR-COMPL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 NBASE-LEGAL-RENEG    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-BASE            PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-STUDO-NEGOC     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-CONTR          PIC X(10).
      *    *************************************************************
           10 DMOVTO-RECOR         PIC X(10).
      *    *************************************************************
           10 CFUNC-ATULZ          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCETFC-ORGNC-EMPTO   PIC X(80).
      *    *************************************************************
           10 EURL-CETFC-ORGNC.
              49 EURL-CETFC-ORGNC-LEN
                 PIC S9(4) USAGE COMP-5.
              49 EURL-CETFC-ORGNC-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CRSTRB-EMPTO-RURAL   PIC X(80).
      *    *************************************************************
           10 EURL-RSTRB-EMPTO.
              49 EURL-RSTRB-EMPTO-LEN
                 PIC S9(4) USAGE COMP-5.
              49 EURL-RSTRB-EMPTO-TEXT
                 PIC X(256).
      *    *************************************************************
           10 PENERG-RENOV-EMPTO   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-SOLO-AGCLT      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCICLO-CTVDA         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CMANUT-SOLO          PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-SEGUR-CONTT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSCRET-TSORO-NACIO   PIC X(13).
      *    *************************************************************
           10 CCLIMT-RURAL-BACEN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 COUTRO-TRATO-SOLO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CONFM-FLORS   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CONFM-IBAMA   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CONFM-CSVAO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RREGLZ-IBAMA.
              49 RREGLZ-IBAMA-LEN
                 PIC S9(4) USAGE COMP-5.
              49 RREGLZ-IBAMA-TEXT
                 PIC X(300).
      *    *************************************************************
           10 COUTRO-TPO-CONFM     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-INSTC-EXIGB    PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CONFM-RURAL   PIC X(1).
      *    *************************************************************
           10 CFLIAL-CNPJ-EXIGB    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-EXIGB     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QUND-ANMAL           PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PBONUS-SUSTBD        PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-INSTC-EXIGB-ST
              PIC X(8).
      *    *************************************************************
           10 CFLIAL-CNPJ-EXIGB-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-EXIGB-ST  PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 66      *
      ******************************************************************
