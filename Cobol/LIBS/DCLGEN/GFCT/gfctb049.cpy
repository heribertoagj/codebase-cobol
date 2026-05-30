***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.FLEXZ_AGPTO_CTA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB049))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB049)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.FLEXZ_AGPTO_CTA TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             HIDTFD_FLEXZ_AGPTO             TIMESTAMP NOT NULL,
             DINIC_FLEXZ_AGPTO              DATE NOT NULL,
             DFIM_FLEXZ_AGPTO               DATE NOT NULL,
             QFRANQ_FLEXZ_AGPTO             DECIMAL(3, 0) NOT NULL,
             CPER_FRANQ_AGPTO               DECIMAL(1, 0) NOT NULL,
             VNEGOC_FLEXZ_AGPTO             DECIMAL(11, 2) NOT NULL,
             PDESC_FLEXZ_AGPTO              DECIMAL(7, 4) NOT NULL,
             QDIA_FLEXZ_AGPTO               DECIMAL(2, 0) NOT NULL,
             CINDCD_AGPTO_TOT               CHAR(1) NOT NULL,
             DDIA_AGNDA                     DECIMAL(2, 0) NOT NULL,
             CINIC_PER_FRANQ                DECIMAL(1, 0) NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_JUSTF                   DECIMAL(2, 0) NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC_ENCRR_VGCIA             DECIMAL(5, 0) NOT NULL,
             CFUNC_ENCRR_VGCIA              DECIMAL(9, 0) NOT NULL,
             HVGCIA_REGRA                   TIMESTAMP NOT NULL,
             CPER_COBR_TARIF                DECIMAL(1, 0) NOT NULL,
             CINIC_PER_COBR                 DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.FLEXZ_AGPTO_CTA             *
      ******************************************************************
       01  GFCTB049.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-FLEXZ-AGPTO   PIC X(26).
      *    *************************************************************
           10 DINIC-FLEXZ-AGPTO    PIC X(10).
      *    *************************************************************
           10 DFIM-FLEXZ-AGPTO     PIC X(10).
      *    *************************************************************
           10 QFRANQ-FLEXZ-AGPTO   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPER-FRANQ-AGPTO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VNEGOC-FLEXZ-AGPTO   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDESC-FLEXZ-AGPTO    PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 QDIA-FLEXZ-AGPTO     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AGPTO-TOT     PIC X(1).
      *    *************************************************************
           10 DDIA-AGNDA           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINIC-PER-FRANQ      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-JUSTF         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-ENCRR-VGCIA   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-ENCRR-VGCIA    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HVGCIA-REGRA         PIC X(26).
      *    *************************************************************
           10 CPER-COBR-TARIF      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINIC-PER-COBR       PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 21      *
      ******************************************************************
