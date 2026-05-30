      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CEDULA_TEMPR)                              *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB007))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB007)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CEDULA_TEMPR TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCDULA                         DECIMAL(9, 0) NOT NULL,
             DCDULA                         DATE NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CTPO_PROTR_RURAL               DECIMAL(2, 0) NOT NULL,
             CATVDD_RURAL                   DECIMAL(3, 0) NOT NULL,
             CFNALD_RURAL                   DECIMAL(3, 0) NOT NULL,
             CPRODT_RURAL                   DECIMAL(3, 0) NOT NULL,
             QUND_FINCD_RURAL               DECIMAL(11, 2) NOT NULL,
             CUND_MEDD_FINCD                DECIMAL(3, 0) NOT NULL,
             QUND_PROD_PROVV                DECIMAL(11, 2) NOT NULL,
             CUND_MEDD_PROVV                DECIMAL(3, 0) NOT NULL,
             DINIC_SAFRA                    DECIMAL(6, 0) NOT NULL,
             DFIM_SAFRA                     DECIMAL(6, 0) NOT NULL,
             VFINAN                         DECIMAL(15, 2) NOT NULL,
             VREC_PPRIO                     DECIMAL(15, 2) NOT NULL,
             IPPRIE                         CHAR(40) NOT NULL,
             MAREA_TOT                      DECIMAL(9, 2) NOT NULL,
             CMUN_RURAL                     DECIMAL(9, 0) NOT NULL,
             CESPCE_TITLO_GARNT             DECIMAL(3, 0) NOT NULL,
             CTPO_GARNT                     DECIMAL(3, 0) NOT NULL,
             CCREA_AGRON_RURAL              DECIMAL(10, 0),
             CRGIAO_CREA                    CHAR(2),
             CINCID_PROAGRO                 DECIMAL(2, 0) NOT NULL,
             CORIGE_REC                     DECIMAL(3, 0) NOT NULL,
             CGARTD                         DECIMAL(3, 0) NOT NULL,
             VCOBER_PROAGRO                 DECIMAL(15, 2),
             VREC_ENQUA                     DECIMAL(15, 2),
             CESPCE_EXPLO_IMOV              DECIMAL(1, 0) NOT NULL,
             CAUTRZ_OPER                    DECIMAL(15, 0) NOT NULL,
             CPROG                          CHAR(8) NOT NULL,
             CTPO_CONDC_FSCAL               DECIMAL(2, 0) NOT NULL,
             RESPCE_EXPLO_IMOV              CHAR(15) NOT NULL,
             CAPLIC_RURAL                   DECIMAL(3, 0) NOT NULL,
             CCLASF_PRODT_RURAL             DECIMAL(5, 0),
             CTPO_EMBAL                     DECIMAL(3, 0),
             QCAPAC_PROD                    DECIMAL(11, 0),
             CCATEG_MUTUA                   DECIMAL(3, 0),
             VEGF                           DECIMAL(11, 2),
             CIMPRE_CDULA_TEMPR             DECIMAL(1, 0) NOT NULL,
             CMATR_IMOV_RURAL               CHAR(15) NOT NULL,
             CPSSOA_CADTR                   TIMESTAMP NOT NULL,
             CSIT_CDULA_TEMPR               DECIMAL(2, 0) NOT NULL,
             CINDCD_RESP_APROV              DECIMAL(1, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CINDCD_TPO_ORCAM               DECIMAL(1, 0) NOT NULL,
             CCONTR                         DECIMAL(9, 0),
             DANO_BASE_NEGOC                DECIMAL(4, 0) NOT NULL,
             CDEPDC_NEGOC                   DECIMAL(5, 0) NOT NULL,
             CSEQ_STUDO_NEGOC               DECIMAL(9, 0) NOT NULL,
             CCLI_UNIC_BDSCO                DECIMAL(10, 0) NOT NULL,
             DULT_VALDC_CDULA               TIMESTAMP NOT NULL,
             CINDCD_FONE_RESID              CHAR(1) NOT NULL,
             CINDCD_FONE_COML               CHAR(1) NOT NULL,
             CINDCD_ENDER_RESID             CHAR(1) NOT NULL,
             CINDCD_ENDER_COML              CHAR(1) NOT NULL,
             CINDCD_QUALF_PROFS             CHAR(1) NOT NULL,
             QBLOCO_PATRM_IMOV              DECIMAL(3, 0) NOT NULL,
             QBLOCO_PATRM_VEIC              DECIMAL(3, 0) NOT NULL,
             QBLOCO_LSNG_CLI                DECIMAL(3, 0) NOT NULL,
             QBLOCO_SEGUR_CLI               DECIMAL(3, 0) NOT NULL,
             QBLOCO_QUADR_SCIAL             DECIMAL(3, 0) NOT NULL,
             DDSPDO_AG                      DATE NOT NULL,
             CSEGDR_CREDT_RURAL             DECIMAL(3, 0) NOT NULL,
             DIMPRE_CONTR_RURAL             DATE NOT NULL,
             CSIT_CDULA_ANTER               DECIMAL(2, 0) NOT NULL,
             CINDCD_CTRL_FORML              CHAR(1) NOT NULL,
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
             CTPO_CDULA_RECOR               CHAR(3) NOT NULL,
             VRECTA_BRUTA_EMPTO             DECIMAL(15, 2),
             CPLATI_CULTR_ITCAL             DECIMAL(1, 0),
             CTPO_CICLO_RURAL               DECIMAL(3, 0),
             CINDCD_ENVIO_BNDES             DECIMAL(1, 0),
             CTPO_BNEFC_RURAL               DECIMAL(2, 0),
             CDECLR_AGCLT_FAMLR             CHAR(25),
             DVALDD_DECLR_FAMLR             DATE,
             CINDCD_RENDA_PREVT             CHAR(1),
             CGRP_AGCLT_FAMLR               DECIMAL(2, 0),
             CFORMA_LIQDC                   DECIMAL(2, 0),
             CINDCD_OPTE_CHEQ               DECIMAL(1, 0),
             CINDCD_OPTE_REENV              DECIMAL(1, 0),
             CCETFC_ORGNC_EMPTO             CHAR(80),
             EURL_CETFC_ORGNC               VARCHAR(256),
             CRSTRB_EMPTO_RURAL             CHAR(80),
             EURL_RSTRB_EMPTO               VARCHAR(256),
             PENERG_RENOV_EMPTO             DECIMAL(5, 2),
             CTPO_SOLO_AGCLT                DECIMAL(2, 0),
             CCICLO_CTVDA                   DECIMAL(2, 0),
             CMANUT_SOLO                    DECIMAL(2, 0),
             CCLIMT_RURAL_BACEN             DECIMAL(2, 0) NOT NULL,
             COUTRO_TRATO_SOLO              DECIMAL(5, 0),
             QUND_ANMAL                     DECIMAL(11, 2),
             PBONUS_SUSTBD                  DECIMAL(5, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CEDULA_TEMPR                *
      ******************************************************************
       01  RUECB007.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DCDULA               PIC X(10).
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CATVDD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFNALD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QUND-FINCD-RURAL     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD-FINCD      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QUND-PROD-PROVV      PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD-PROVV      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-SAFRA          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-SAFRA           PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VFINAN               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VREC-PPRIO           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 IPPRIE               PIC X(40).
      *    *************************************************************
           10 MAREA-TOT            PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CMUN-RURAL           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CESPCE-TITLO-GARNT   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-GARNT           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCREA-AGRON-RURAL    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-CREA          PIC X(2).
      *    *************************************************************
           10 CINCID-PROAGRO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CGARTD               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VCOBER-PROAGRO       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VREC-ENQUA           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CESPCE-EXPLO-IMOV    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CAUTRZ-OPER          PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 CPROG                PIC X(8).
      *    *************************************************************
           10 CTPO-CONDC-FSCAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RESPCE-EXPLO-IMOV    PIC X(15).
      *    *************************************************************
           10 CAPLIC-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCLASF-PRODT-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-EMBAL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QCAPAC-PROD          PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CCATEG-MUTUA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VEGF                 PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CIMPRE-CDULA-TEMPR   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CMATR-IMOV-RURAL     PIC X(15).
      *    *************************************************************
           10 CPSSOA-CADTR         PIC X(26).
      *    *************************************************************
           10 CSIT-CDULA-TEMPR     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-RESP-APROV    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CINDCD-TPO-ORCAM     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-BASE-NEGOC      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-NEGOC         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-STUDO-NEGOC     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCLI-UNIC-BDSCO      PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 DULT-VALDC-CDULA     PIC X(26).
      *    *************************************************************
           10 CINDCD-FONE-RESID    PIC X(1).
      *    *************************************************************
           10 CINDCD-FONE-COML     PIC X(1).
      *    *************************************************************
           10 CINDCD-ENDER-RESID   PIC X(1).
      *    *************************************************************
           10 CINDCD-ENDER-COML    PIC X(1).
      *    *************************************************************
           10 CINDCD-QUALF-PROFS   PIC X(1).
      *    *************************************************************
           10 QBLOCO-PATRM-IMOV    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QBLOCO-PATRM-VEIC    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QBLOCO-LSNG-CLI      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QBLOCO-SEGUR-CLI     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QBLOCO-QUADR-SCIAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DDSPDO-AG            PIC X(10).
      *    *************************************************************
           10 CSEGDR-CREDT-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DIMPRE-CONTR-RURAL   PIC X(10).
      *    *************************************************************
           10 CSIT-CDULA-ANTER     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CTRL-FORML    PIC X(1).
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
           10 CTPO-CDULA-RECOR     PIC X(3).
      *    *************************************************************
           10 VRECTA-BRUTA-EMPTO   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CPLATI-CULTR-ITCAL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CICLO-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ENVIO-BNDES   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BNEFC-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CDECLR-AGCLT-FAMLR   PIC X(25).
      *    *************************************************************
           10 DVALDD-DECLR-FAMLR   PIC X(10).
      *    *************************************************************
           10 CINDCD-RENDA-PREVT   PIC X(1).
      *    *************************************************************
           10 CGRP-AGCLT-FAMLR     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-LIQDC         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-OPTE-CHEQ     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-OPTE-REENV    PIC S9(1)V USAGE COMP-3.
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
           10 CCLIMT-RURAL-BACEN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 COUTRO-TRATO-SOLO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QUND-ANMAL           PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PBONUS-SUSTBD        PIC S9(3)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 107     *
      ******************************************************************
