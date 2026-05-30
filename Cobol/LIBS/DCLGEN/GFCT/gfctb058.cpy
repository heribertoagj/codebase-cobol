***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.FLEXZ_INDVD_CTA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB058))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB058)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.FLEXZ_INDVD_CTA TABLE
           ( CBCO_CTA_CLI                   DECIMAL(3, 0) NOT NULL,
             CDEPDC_CTA_CLI                 DECIMAL(5, 0) NOT NULL,
             CNRO_CTA_CLI                   DECIMAL(13, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_FLEXZ_CTA                DATE NOT NULL,
             DFIM_FLEXZ_CTA                 DATE NOT NULL,
             QFRANQ_FLEXZ_CTA               DECIMAL(3, 0) NOT NULL,
             CPER_FRANQ_CTA                 DECIMAL(1, 0) NOT NULL,
             VNEGOC_FLEXZ_CTA               DECIMAL(11, 2) NOT NULL,
             PDESC_FLEXZ_CTA                DECIMAL(7, 4) NOT NULL,
             DDIA_AGNDA                     DECIMAL(2, 0) NOT NULL,
             QDIA_FLEXZ_CTA                 DECIMAL(2, 0) NOT NULL,
             HIDTFD_FLEXZ_CTA               TIMESTAMP NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC_ENCRR_VGCIA             DECIMAL(5, 0) NOT NULL,
             CFUNC_ENCRR_VGCIA              DECIMAL(9, 0) NOT NULL,
             HVGCIA_REGRA                   TIMESTAMP NOT NULL,
             CINIC_PER_FRANQ                DECIMAL(1, 0) NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_JUSTF                   DECIMAL(2, 0) NOT NULL,
             CPER_COBR_TARIF                DECIMAL(1, 0) NOT NULL,
             CINIC_PER_COBR                 DECIMAL(1, 0) NOT NULL,
             QDEP_MES_TARIF                 DECIMAL(9, 0) NOT NULL,
             NLOTE_FLEXZ_CTA                DECIMAL(11, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.FLEXZ_INDVD_CTA             *
      ******************************************************************
       01  GFCTB058.
      *    *************************************************************
           10 CBCO-CTA-CLI         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-CTA-CLI       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTA-CLI         PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
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
           10 HIDTFD-FLEXZ-CTA     PIC X(26).
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-ENCRR-VGCIA   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-ENCRR-VGCIA    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HVGCIA-REGRA         PIC X(26).
      *    *************************************************************
           10 CINIC-PER-FRANQ      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-JUSTF         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CPER-COBR-TARIF      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINIC-PER-COBR       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QDEP-MES-TARIF       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NLOTE-FLEXZ-CTA      PIC S9(11)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
