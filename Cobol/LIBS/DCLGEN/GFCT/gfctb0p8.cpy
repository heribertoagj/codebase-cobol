***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TFLEXZ_TARIF_AVULS)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0P8))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0P8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TFLEXZ_TARIF_AVULS TABLE
           ( CBCO_SALRL                     DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA_SALRL                DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_SALRL               DECIMAL(13, 0) NOT NULL,
             NFLEXZ_TARIF_AVULS             DECIMAL(10, 0) NOT NULL,
             CBCO_CLI                       DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             CGRP_SERVC                     DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_SERVC              DATE NOT NULL,
             DINIC_VGCIA_FLEXZ              DATE,
             DFIM_VGCIA_FLEXZ               DATE,
             CINDCD_TPO_NEGOC               DECIMAL(1, 0) NOT NULL,
             QFRANQ_TARIF_CADTR             DECIMAL(7, 0) NOT NULL,
             CSIT_NEGOC_TARIF               DECIMAL(1, 0) NOT NULL,
             CSIST                          CHAR(4) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             RJUSTF_INCL_NEGOC              VARCHAR(256),
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             RJUSTF_ENCRR_NEGOC             VARCHAR(256)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TFLEXZ_TARIF_AVULS          *
      ******************************************************************
       01  GFCTB0P8.
      *    *************************************************************
           10 CBCO-SALRL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA-SALRL      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-SALRL     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 NFLEXZ-TARIF-AVULS   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-CLI             PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-SERVC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-SERVC    PIC X(10).
      *    *************************************************************
           10 DINIC-VGCIA-FLEXZ    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-FLEXZ     PIC X(10).
      *    *************************************************************
           10 CINDCD-TPO-NEGOC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QFRANQ-TARIF-CADTR   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-NEGOC-TARIF     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSIST                PIC X(4).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 RJUSTF-INCL-NEGOC.
              49 RJUSTF-INCL-NEGOC-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-INCL-NEGOC-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 RJUSTF-ENCRR-NEGOC.
              49 RJUSTF-ENCRR-NEGOC-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-ENCRR-NEGOC-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
