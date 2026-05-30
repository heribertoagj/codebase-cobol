      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CONTRATO_RURAL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RUFIB002))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUFIB002)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CONTRATO_RURAL TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             CCDULA                         DECIMAL(9, 0) NOT NULL,
             DCDULA                         DATE NOT NULL,
             DINIC_SAFRA                    DECIMAL(6, 0),
             MAREA_TOT                      DECIMAL(9, 2) NOT NULL,
             VTOT_GARNT                     DECIMAL(15, 2) NOT NULL,
             DVCTO_CONTR                    DATE NOT NULL,
             CSIT_FSCAL                     DECIMAL(2, 0) NOT NULL,
             VFINAN                         DECIMAL(15, 2) NOT NULL,
             QUND_FINCD_RURAL               DECIMAL(11, 2) NOT NULL,
             QUND_PROD_PROVV                DECIMAL(11, 2) NOT NULL,
             DLIM_FSCAL                     DATE NOT NULL,
             VREC_PPRIO                     DECIMAL(15, 2) NOT NULL,
             DFIM_SAFRA                     DECIMAL(6, 0),
             CINDCD_LAUDO_FSCAL             CHAR(1) NOT NULL,
             DSIT_FSCAL                     DATE NOT NULL,
             CSIT_ANLSE                     DECIMAL(2, 0) NOT NULL,
             QCAPAC_PROD                    DECIMAL(11, 0) NOT NULL,
             DINCL                          DATE NOT NULL,
             IPPRIE                         CHAR(40) NOT NULL,
             QTOT_GARNT                     DECIMAL(9, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CGARTD                         DECIMAL(3, 0),
             CCTRO_FSCAL_RURAL              DECIMAL(3, 0),
             CMUN_RURAL                     DECIMAL(9, 0),
             CFNALD_RURAL                   DECIMAL(3, 0),
             CATVDD_RURAL                   DECIMAL(3, 0),
             CORIGE_REC                     DECIMAL(3, 0),
             CPRODT_RURAL                   DECIMAL(3, 0),
             CUND_MEDD_FINCD                DECIMAL(3, 0),
             CUND_MEDD_PROVV                DECIMAL(3, 0),
             CTPO_EMBAL                     DECIMAL(3, 0),
             CCATEG_MUTUA                   DECIMAL(3, 0),
             CTPO_CONDC_FSCAL               DECIMAL(2, 0),
             CESPCE_TITLO_GARNT             DECIMAL(3, 0),
             CINCID_PROAGRO                 DECIMAL(2, 0),
             CTPO_GARNT                     DECIMAL(3, 0),
             CTPO_PROTR_RURAL               DECIMAL(2, 0),
             CCTA_CTBIL_VINC                DECIMAL(7, 0),
             CSTTUS_CONTR_RURAL             DECIMAL(1, 0) NOT NULL,
             CESPCE_EXPLO_IMOV              DECIMAL(1, 0),
             CAUTRZ_OPER                    DECIMAL(15, 0),
             CFUNC_BDSCO                    DECIMAL(9, 0),
             CFUNC_BDSCO_RESP               DECIMAL(9, 0),
             RESPCE_EXPLO_IMOV              CHAR(15) NOT NULL,
             VEGF                           DECIMAL(11, 2),
             CCLASF_PRODT_RURAL             DECIMAL(5, 0),
             DVCTO_ORIGN_CONTR              DATE NOT NULL,
             CCONDC_ESPCL_RURAL             CHAR(1) NOT NULL,
             CREFT_BACEN                    DECIMAL(9, 0) NOT NULL,
             VFINAN_DOLAR                   DECIMAL(15, 2) NOT NULL,
             VORCAM                         DECIMAL(15, 2) NOT NULL,
             CCREA_AGRON_RURAL              DECIMAL(10, 0),
             CRGIAO_CREA                    CHAR(2),
             CEMPR_FSCAL_RURAL              DECIMAL(5, 0) NOT NULL,
             DLAUDO_FSCAL                   DATE NOT NULL,
             CCTRO_FSCAL_ANLSE              DECIMAL(3, 0) NOT NULL,
             CCONTR_PSIVO                   DECIMAL(9, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CCONTR_BNDES                   CHAR(15) NOT NULL,
             DANO_BASE_NEGOC                DECIMAL(4, 0) NOT NULL,
             CDEPDC_NEGOC                   DECIMAL(5, 0) NOT NULL,
             CSEQ_STUDO_NEGOC               DECIMAL(9, 0) NOT NULL,
             CSEGDR_CREDT_RURAL             DECIMAL(3, 0) NOT NULL,
             CFUNC_BDSCO_AUTRZ              DECIMAL(9, 0),
             DLIBRC_SEM_CARTR               DATE,
             DIMPRE_CONTR_RURAL             DATE NOT NULL,
             CINDCD_ALT_RECOR               CHAR(1) NOT NULL,
             CCTRO_CUSTO                    CHAR(4) NOT NULL,
             DANO_LIM_OPER                  DECIMAL(4, 0),
             CDEPDC_LIM_OPER                DECIMAL(5, 0),
             CAPROV_LIM_OPER                DECIMAL(9, 0),
             CCESTA_SAFRA_RURAL             DECIMAL(5, 0) NOT NULL,
             CMODLD_RURAL                   DECIMAL(5, 0) NOT NULL,
             CVAR_PRODT_RURAL               DECIMAL(5, 0) NOT NULL,
             CINDCD_ZNMTO                   DECIMAL(1, 0) NOT NULL,
             CINDCD_CSCIO                   DECIMAL(1, 0) NOT NULL,
             CTPO_AGROP_RURAL               DECIMAL(5, 0) NOT NULL,
             CTPO_INTGC_RURAL               DECIMAL(5, 0) NOT NULL,
             CINDCD_UND_PRDTV               DECIMAL(1, 0) NOT NULL,
             CTPO_IGCAO_RURAL               DECIMAL(5, 0) NOT NULL,
             CTPO_CTIVO_RURAL               DECIMAL(5, 0) NOT NULL,
             CCICLO_PROD_RURAL              DECIMAL(5, 0) NOT NULL,
             CREFT_RECOR                    CHAR(11) NOT NULL,
             CTPO_CDULA_RECOR               CHAR(3) NOT NULL,
             VRECTA_BRUTA_EMPTO             DECIMAL(15, 2),
             CTPO_BNEFC_RURAL               DECIMAL(2, 0),
             CDECLR_AGCLT_FAMLR             CHAR(25),
             DVALDD_DECLR_FAMLR             DATE,
             QPROTV_ADQUI                   DECIMAL(15, 2),
             CGRP_AGCLT_FAMLR               DECIMAL(2, 0),
             DREG_COOP_BACEN                DATE,
             CFORMA_LIQDC                   DECIMAL(2, 0),
             CCETFC_ORGNC_EMPTO             CHAR(80),
             EURL_CETFC_ORGNC               VARCHAR(256),
             CRSTRB_EMPTO_RURAL             CHAR(80),
             EURL_RSTRB_EMPTO               VARCHAR(256),
             PENERG_RENOV_EMPTO             DECIMAL(5, 2),
             CTPO_SOLO_AGCLT                DECIMAL(2, 0),
             CCICLO_CTVDA                   DECIMAL(2, 0),
             CMANUT_SOLO                    DECIMAL(2, 0),
             CSCRET_TSORO_NACIO             CHAR(13),
             CCLIMT_RURAL_BACEN             DECIMAL(2, 0) NOT NULL,
             PREDC_JURO_AMBTL               DECIMAL(5, 2),
             CINDCD_CONFM_FLORS             DECIMAL(1, 0),
             CINDCD_CONFM_IBAMA             DECIMAL(1, 0),
             CINDCD_CONFM_CSVAO             DECIMAL(1, 0),
             RREGLZ_IBAMA                   VARCHAR(300),
             COUTRO_TRATO_SOLO              DECIMAL(5, 0),
             COUTRO_TPO_CONFM               DECIMAL(5, 0),
             CCNPJ_INSTC_EXIGB              DECIMAL(8, 0),
             CINDCD_CONFM_RURAL             CHAR(1),
             CFLIAL_CNPJ_EXIGB              DECIMAL(4, 0),
             CCTRL_CNPJ_EXIGB               DECIMAL(2, 0),
             QUND_ANMAL                     DECIMAL(11, 2),
             PBONUS_SUSTBD                  DECIMAL(5, 2),
             CCNPJ_EXIGB_ST                 CHAR(8),
             CFLIAL_EXIGB_ST                CHAR(4),
             CCTRL_EXIGB_ST                 DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CONTRATO_RURAL              *
      ******************************************************************
       01  RUFIB002.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DCDULA               PIC X(10).
      *    *************************************************************
           10 DINIC-SAFRA          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 MAREA-TOT            PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-GARNT           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-CONTR          PIC X(10).
      *    *************************************************************
           10 CSIT-FSCAL           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VFINAN               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QUND-FINCD-RURAL     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QUND-PROD-PROVV      PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DLIM-FSCAL           PIC X(10).
      *    *************************************************************
           10 VREC-PPRIO           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DFIM-SAFRA           PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-LAUDO-FSCAL   PIC X(1).
      *    *************************************************************
           10 DSIT-FSCAL           PIC X(10).
      *    *************************************************************
           10 CSIT-ANLSE           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QCAPAC-PROD          PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 DINCL                PIC X(10).
      *    *************************************************************
           10 IPPRIE               PIC X(40).
      *    *************************************************************
           10 QTOT-GARNT           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CGARTD               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRO-FSCAL-RURAL    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMUN-RURAL           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFNALD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CATVDD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD-FINCD      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD-PROVV      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-EMBAL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCATEG-MUTUA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONDC-FSCAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CESPCE-TITLO-GARNT   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINCID-PROAGRO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-GARNT           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CTBIL-VINC      PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSTTUS-CONTR-RURAL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CESPCE-EXPLO-IMOV    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CAUTRZ-OPER          PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO-RESP     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 RESPCE-EXPLO-IMOV    PIC X(15).
      *    *************************************************************
           10 VEGF                 PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCLASF-PRODT-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-ORIGN-CONTR    PIC X(10).
      *    *************************************************************
           10 CCONDC-ESPCL-RURAL   PIC X(1).
      *    *************************************************************
           10 CREFT-BACEN          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 VFINAN-DOLAR         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VORCAM               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCREA-AGRON-RURAL    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-CREA          PIC X(2).
      *    *************************************************************
           10 CEMPR-FSCAL-RURAL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DLAUDO-FSCAL         PIC X(10).
      *    *************************************************************
           10 CCTRO-FSCAL-ANLSE    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCONTR-PSIVO         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCONTR-BNDES         PIC X(15).
      *    *************************************************************
           10 DANO-BASE-NEGOC      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-NEGOC         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-STUDO-NEGOC     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSEGDR-CREDT-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO-AUTRZ    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DLIBRC-SEM-CARTR     PIC X(10).
      *    *************************************************************
           10 DIMPRE-CONTR-RURAL   PIC X(10).
      *    *************************************************************
           10 CINDCD-ALT-RECOR     PIC X(1).
      *    *************************************************************
           10 CCTRO-CUSTO          PIC X(4).
      *    *************************************************************
           10 DANO-LIM-OPER        PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-LIM-OPER      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAPROV-LIM-OPER      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCESTA-SAFRA-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CMODLD-RURAL         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CVAR-PRODT-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ZNMTO         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CSCIO         PIC S9(1)V USAGE COMP-3.
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
           10 CREFT-RECOR          PIC X(11).
      *    *************************************************************
           10 CTPO-CDULA-RECOR     PIC X(3).
      *    *************************************************************
           10 VRECTA-BRUTA-EMPTO   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BNEFC-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CDECLR-AGCLT-FAMLR   PIC X(25).
      *    *************************************************************
           10 DVALDD-DECLR-FAMLR   PIC X(10).
      *    *************************************************************
           10 QPROTV-ADQUI         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CGRP-AGCLT-FAMLR     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DREG-COOP-BACEN      PIC X(10).
      *    *************************************************************
           10 CFORMA-LIQDC         PIC S9(2)V USAGE COMP-3.
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
           10 CSCRET-TSORO-NACIO   PIC X(13).
      *    *************************************************************
           10 CCLIMT-RURAL-BACEN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 PREDC-JURO-AMBTL     PIC S9(3)V9(2) USAGE COMP-3.
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
           10 COUTRO-TRATO-SOLO    PIC S9(5)V USAGE COMP-3.
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
           10 CCNPJ-EXIGB-ST       PIC X(8).
      *    *************************************************************
           10 CFLIAL-EXIGB-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-EXIGB-ST       PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 120     *
      ******************************************************************
