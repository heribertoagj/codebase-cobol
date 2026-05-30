     ******************************************************************
      * DCLGEN TABLE(DB2PRD.PDIDO_FLEXZ_CTA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0B5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0B5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PDIDO_FLEXZ_CTA TABLE
           ( CBCO_CTA_CLI                   DECIMAL(3, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_FLEXZ_CTA               TIMESTAMP NOT NULL,
             DINIC_FLEXZ_CTA                DATE NOT NULL,
             DFIM_FLEXZ_CTA                 DATE NOT NULL,
             QFRANQ_FLEXZ_CTA               DECIMAL(3, 0) NOT NULL,
             CPER_FRANQ_CTA                 DECIMAL(1, 0) NOT NULL,
             VNEGOC_FLEXZ_CTA               DECIMAL(11, 2) NOT NULL,
             PDESC_FLEXZ_CTA                DECIMAL(7, 4) NOT NULL,
             DDIA_AGNDA                     DECIMAL(2, 0) NOT NULL,
             QDIA_FLEXZ_CTA                 DECIMAL(2, 0) NOT NULL,
             CEMPR_ORIGE_FLEXZ              DECIMAL(5, 0) NOT NULL,
             CDEPDC_ORIGE_FLEXZ             DECIMAL(5, 0) NOT NULL,
             CFUNC_SOLTC_FLEXZ              DECIMAL(9, 0) NOT NULL,
             HSOLTC_FLEXZ                   TIMESTAMP NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_JUSTF                   DECIMAL(2, 0) NOT NULL,
             RJUSTF_SOLTC_FLEXZ             VARCHAR(255) NOT NULL,
             CEMPR_PAREC_FLEXZ              DECIMAL(5, 0) NOT NULL,
             CIDTFD_DEPDC                   CHAR(2) NOT NULL,
             CDEPDC_PAREC_FLEXZ             DECIMAL(5, 0) NOT NULL,
             CFUNC_PAREC_FLEXZ              DECIMAL(9, 0) NOT NULL,
             HPAREC_FLEXZ                   TIMESTAMP NOT NULL,
             RJUSTF_PAREC_FLEXZ             VARCHAR(255) NOT NULL,
             CSIT_SOLTC_FLEXZ               DECIMAL(2, 0) NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             CINIC_PER_FRANQ                DECIMAL(1, 0) NOT NULL,
             CPER_COBR_TARIF                DECIMAL(1, 0) NOT NULL,
             CINIC_PER_COBR                 DECIMAL(1, 0) NOT NULL,
             QDEP_MES_TARIF                 DECIMAL(9, 0) NOT NULL,
             NLOTE_FLEXZ_CTA                DECIMAL(11, 0),
             CFUNC_BDSCO_ANLSE              DECIMAL(9, 0) NOT NULL,
             HENVIO_DEFER_ANLSE             TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PDIDO_FLEXZ_CTA             *
      ******************************************************************
       01  GFCTB0B5.
      *    *************************************************************
           10 CBCO-CTA-CLI         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-FLEXZ-CTA     PIC X(26).
      *    *************************************************************
           10 DINIC-FLEXZ-CTA      PIC X(10).
      *    *************************************************************
           10 DFIM-FLEXZ-CTA       PIC X(10).
      *    *************************************************************
           10 QFRANQ-FLEXZ-CTA     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPER-FRANQ-CTA       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VNEGOC-FLEXZ-CTA     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDESC-FLEXZ-CTA      PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 DDIA-AGNDA           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QDIA-FLEXZ-CTA       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPR-ORIGE-FLEXZ    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-ORIGE-FLEXZ   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-SOLTC-FLEXZ    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HSOLTC-FLEXZ         PIC X(26).
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-JUSTF         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-SOLTC-FLEXZ.
              49 RJUSTF-SOLTC-FLEXZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-SOLTC-FLEXZ-TEXT
                 PIC X(255).
      *    *************************************************************
           10 CEMPR-PAREC-FLEXZ    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-DEPDC         PIC X(2).
      *    *************************************************************
           10 CDEPDC-PAREC-FLEXZ   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-PAREC-FLEXZ    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HPAREC-FLEXZ         PIC X(26).
      *    *************************************************************
           10 RJUSTF-PAREC-FLEXZ.
              49 RJUSTF-PAREC-FLEXZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-PAREC-FLEXZ-TEXT
                 PIC X(255).
      *    *************************************************************
           10 CSIT-SOLTC-FLEXZ     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CINIC-PER-FRANQ      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPER-COBR-TARIF      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINIC-PER-COBR       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QDEP-MES-TARIF       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NLOTE-FLEXZ-CTA      PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO-ANLSE    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HENVIO-DEFER-ANLSE   PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 35      *
      ******************************************************************
