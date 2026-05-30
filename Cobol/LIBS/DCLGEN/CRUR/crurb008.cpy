      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONTR_CREDT_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB008))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB008)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONTR_CREDT_RURAL TABLE
           ( CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             CPERDC                         DECIMAL(2, 0) NOT NULL,
             CORIGE_REC_CREDT               DECIMAL(3, 0) NOT NULL,
             CMOTVO_SIT_RURAL               DECIMAL(3, 0),
             CSIT_OPER_RURAL                DECIMAL(2, 0) NOT NULL,
             CIDIOM                         DECIMAL(3, 0) NOT NULL,
             NCONTR_FUMAG_RURAL             DECIMAL(5, 0),
             VCONTR_RURAL                   DECIMAL(15, 2) NOT NULL,
             CREFT_BACEN_CONTR              DECIMAL(9, 0),
             CSCRTZ_CONTR_RURAL             CHAR(1) NOT NULL,
             CCONVE_CONTR_CREDT             CHAR(1) NOT NULL,
             CUSO_LIM_RURAL                 DECIMAL(1, 0) NOT NULL,
             PSPREAD_CONTR                  DECIMAL(7, 4),
             DEFTVC_CONTR_RURAL             DATE NOT NULL,
             DGERAC_DOCTO_CONTR             DATE,
             DVLR_CONTR_NEGOC               DATE NOT NULL,
             PTX_EFETV_JURO                 DECIMAL(7, 4),
             VCTBIL_CONTR_CREDT             DECIMAL(15, 2) NOT NULL,
             DENVIO_REG_BACEN               DATE,
             CSIT_REG_BACEN                 DECIMAL(1, 0) NOT NULL,
             NLOTE_CONTR_RURAL              DECIMAL(3, 0),
             QCOOP_LOTE_RURAL               DECIMAL(4, 0),
             DLIM_APROV_CONTR               DATE NOT NULL,
             CGRAU_SVRDE_INCON              DECIMAL(1, 0) NOT NULL,
             NCDULA_CREDT_RURAL             DECIMAL(9, 0),
             CCDULA_RURAL_BACEN             DECIMAL(5, 0),
             CMOTVO_EXCL_BACEN              DECIMAL(3, 0),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             RMOTVO_SIT_OPER                VARCHAR(256),
             CFORMA_LIQDC_CONTR             DECIMAL(2, 0) NOT NULL,
             DEXCL_CONTR_BACEN              DATE,
             CPSSOA                         DECIMAL(10, 0),
             CFICHA_CAD_RURAL               DECIMAL(3, 0),
             CINDCD_RENDA_PREVT             CHAR(1),
             CALT_PERM_PPSTA                DECIMAL(1, 0),
             DREG_COOP_BACEN                DATE,
             CINDCD_CONFM_FLORS             DECIMAL(1, 0),
             CINDCD_CONFM_IBAMA             DECIMAL(1, 0),
             CINDCD_CONFM_CSVAO             DECIMAL(1, 0),
             RREGLZ_IBAMA                   VARCHAR(300),
             PREDC_JURO_AMBTL               DECIMAL(5, 2),
             COUTRO_TPO_CONFM               DECIMAL(5, 0),
             CINDCD_CONFM_RURAL             CHAR(1),
             PBONUS_SUSTB                   DECIMAL(5, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONTR_CREDT_RURAL          *
      ******************************************************************
       01  CRURB008.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CPERDC               PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC-CREDT     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-SIT-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-OPER-RURAL      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIDIOM               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NCONTR-FUMAG-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VCONTR-RURAL         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CREFT-BACEN-CONTR    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSCRTZ-CONTR-RURAL   PIC X(1).
      *    *************************************************************
           10 CCONVE-CONTR-CREDT   PIC X(1).
      *    *************************************************************
           10 CUSO-LIM-RURAL       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 PSPREAD-CONTR        PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 DEFTVC-CONTR-RURAL   PIC X(10).
      *    *************************************************************
           10 DGERAC-DOCTO-CONTR   PIC X(10).
      *    *************************************************************
           10 DVLR-CONTR-NEGOC     PIC X(10).
      *    *************************************************************
           10 PTX-EFETV-JURO       PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 VCTBIL-CONTR-CREDT   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DENVIO-REG-BACEN     PIC X(10).
      *    *************************************************************
           10 CSIT-REG-BACEN       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NLOTE-CONTR-RURAL    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QCOOP-LOTE-RURAL     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 DLIM-APROV-CONTR     PIC X(10).
      *    *************************************************************
           10 CGRAU-SVRDE-INCON    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NCDULA-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA-RURAL-BACEN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-EXCL-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 RMOTVO-SIT-OPER.
              49 RMOTVO-SIT-OPER-LEN
                 PIC S9(4) USAGE COMP-5.
              49 RMOTVO-SIT-OPER-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CFORMA-LIQDC-CONTR   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DEXCL-CONTR-BACEN    PIC X(10).
      *    *************************************************************
           10 CPSSOA               PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CFICHA-CAD-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-RENDA-PREVT   PIC X(1).
      *    *************************************************************
           10 CALT-PERM-PPSTA      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DREG-COOP-BACEN      PIC X(10).
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
           10 PREDC-JURO-AMBTL     PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 COUTRO-TPO-CONFM     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CONFM-RURAL   PIC X(1).
      *    *************************************************************
           10 PBONUS-SUSTB         PIC S9(3)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      ******************************************************************
