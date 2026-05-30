      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TORCAM_CONTR_CREDT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0MB))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0MB)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TORCAM_CONTR_CREDT TABLE
           ( CORCAM_CONTR_RURAL             DECIMAL(10, 0) NOT NULL,
             CPERDC                         DECIMAL(2, 0) NOT NULL,
             CUF                            DECIMAL(6, 0),
             CRGIAO_GEOGR_RURAL             DECIMAL(3, 0),
             CRGIAO_GEOGR_BACEN             DECIMAL(3, 0),
             CPSSOA_DETNT_REC               DECIMAL(10, 0) NOT NULL,
             CREC_FINCR_FINAN               DECIMAL(8, 0) NOT NULL,
             CUND_MEDD                      DECIMAL(3, 0) NOT NULL,
             CMODLD_RURAL                   DECIMAL(5, 0) NOT NULL,
             CORCAM_PDRAO_RURAL             DECIMAL(10, 0),
             CTPO_ORCAM_CONTR               DECIMAL(1, 0) NOT NULL,
             PTX_JURO_EMPTO                 DECIMAL(5, 2) NOT NULL,
             DEFTVC_TX_JURO                 DATE NOT NULL,
             CINDCD_ECONM_NUMRC             DECIMAL(5, 0) NOT NULL,
             PAPLIC_INDCD_ECONM             DECIMAL(9, 6) NOT NULL,
             DEFTVC_INDCD_ECONM             DATE NOT NULL,
             PAPLIC_ASSTN_TECNC             DECIMAL(9, 6) NOT NULL,
             DEFTVC_ASSTN_TECNC             DATE NOT NULL,
             PAPLIC_IOF_ORCAM               DECIMAL(9, 6) NOT NULL,
             DEFTVC_IOF_ORCAM               DATE NOT NULL,
             PAPLIC_IOF_COMPL               DECIMAL(9, 6) NOT NULL,
             DEFTVC_IOF_COMPL               DATE,
             QCAPAC_PROD_ORCAM              DECIMAL(15, 2),
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             RORCAM_CONTR_RURAL             VARCHAR(256),
             CEMPTO_RURAL_BACEN             DECIMAL(9, 0) NOT NULL,
             CPLATI_CULTR_ITCAL             DECIMAL(1, 0),
             CTPO_CICLO_RURAL               DECIMAL(3, 0),
             CASSTN_TECNC_PRODT             DECIMAL(1, 0),
             NAGRON_CREDT_RURAL             DECIMAL(10, 0),
             CDECLR_AGCLT_FAMLR             CHAR(25),
             DVALDD_DECLR_FAMLR             DATE,
             CGRP_ESPCL_RURAL               DECIMAL(2, 0),
             CCETFC_ORGNC_EMPTO             CHAR(80),
             EURL_CETFC_ORGNC               VARCHAR(256),
             CRSTRB_EMPTO_RURAL             CHAR(80),
             EURL_RSTRB_EMPTO               VARCHAR(256),
             PENERG_RENOV_EMPTO             DECIMAL(5, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TORCAM_CONTR_CREDT          *
      ******************************************************************
       01  CRURB0MB.
      *    *************************************************************
           10 CORCAM-CONTR-RURAL   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CPERDC               PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CUF                  PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-GEOGR-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-GEOGR-BACEN   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-DETNT-REC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CREC-FINCR-FINAN     PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMODLD-RURAL         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CORCAM-PDRAO-RURAL   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ORCAM-CONTR     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 PTX-JURO-EMPTO       PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DEFTVC-TX-JURO       PIC X(10).
      *    *************************************************************
           10 CINDCD-ECONM-NUMRC   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 PAPLIC-INDCD-ECONM   PIC S9(3)V9(6) USAGE COMP-3.
      *    *************************************************************
           10 DEFTVC-INDCD-ECONM   PIC X(10).
      *    *************************************************************
           10 PAPLIC-ASSTN-TECNC   PIC S9(3)V9(6) USAGE COMP-3.
      *    *************************************************************
           10 DEFTVC-ASSTN-TECNC   PIC X(10).
      *    *************************************************************
           10 PAPLIC-IOF-ORCAM     PIC S9(3)V9(6) USAGE COMP-3.
      *    *************************************************************
           10 DEFTVC-IOF-ORCAM     PIC X(10).
      *    *************************************************************
           10 PAPLIC-IOF-COMPL     PIC S9(3)V9(6) USAGE COMP-3.
      *    *************************************************************
           10 DEFTVC-IOF-COMPL     PIC X(10).
      *    *************************************************************
           10 QCAPAC-PROD-ORCAM    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 RORCAM-CONTR-RURAL.
              49 RORCAM-CONTR-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 RORCAM-CONTR-RURAL-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CEMPTO-RURAL-BACEN   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CPLATI-CULTR-ITCAL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CICLO-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CASSTN-TECNC-PRODT   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NAGRON-CREDT-RURAL   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CDECLR-AGCLT-FAMLR   PIC X(25).
      *    *************************************************************
           10 DVALDD-DECLR-FAMLR   PIC X(10).
      *    *************************************************************
           10 CGRP-ESPCL-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCETFC-ORGNC-EMPTO   PIC X(80).
      *    *************************************************************
           10 EURL-CETFC-ORGNC.
              49 EURL-CETFC-ORGNC-LEN
                 PIC S9(4) USAGE COMP.
              49 EURL-CETFC-ORGNC-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CRSTRB-EMPTO-RURAL   PIC X(80).
      *    *************************************************************
           10 EURL-RSTRB-EMPTO.
              49 EURL-RSTRB-EMPTO-LEN
                 PIC S9(4) USAGE COMP.
              49 EURL-RSTRB-EMPTO-TEXT
                 PIC X(256).
      *    *************************************************************
           10 PENERG-RENOV-EMPTO   PIC S9(3)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 42      *
      ******************************************************************
