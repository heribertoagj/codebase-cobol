      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRESUL_MNTRC_DIA_CATAO)                    *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB093))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB093)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRESUL_MNTRC_DIA_CATAO TABLE
           ( NRESUL_MNTRC_ARQ               DECIMAL(18, 0) NOT NULL,
             NMNTRC_TRFGO_ARQ               DECIMAL(18, 0),
             CSNLZD_MSGRA                   DECIMAL(3, 0),
             CFASE_PROCM                    DECIMAL(4, 0),
             CISPB_EMISR_MSGEM              CHAR(8),
             CISPB_DSTNA_MSGEM              CHAR(8),
             CSIST_EXTER_CMARA              CHAR(4) NOT NULL,
             CEVNTO_EXTER_CMARA             CHAR(3) NOT NULL,
             CFLUXO_EXTER_CMARA             CHAR(3) NOT NULL,
             CCTRL_RQUIS_ORIGN              CHAR(20) NOT NULL,
             CCTRL_RQUIS_INSTC              CHAR(20) NOT NULL,
             QMSGEM_ENVID                   DECIMAL(10, 0) NOT NULL,
             QMSGEM_RECBD                   DECIMAL(10, 0) NOT NULL,
             DINCL_RESUL_MNTRC              DATE NOT NULL,
             HINCL_RESUL_MNTRC              TIME NOT NULL,
             NRELTV_PROCM_RESUL             DECIMAL(15, 0) NOT NULL,
             HINCL_RESUL_BACEN              TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRESUL_MNTRC_DIA_CATAO      *
      ******************************************************************
       01  BVVEB093.
      *    *************************************************************
           10 NRESUL-MNTRC-ARQ     PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 NMNTRC-TRFGO-ARQ     PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CSNLZD-MSGRA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFASE-PROCM          PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CISPB-EMISR-MSGEM    PIC X(8).
      *    *************************************************************
           10 CISPB-DSTNA-MSGEM    PIC X(8).
      *    *************************************************************
           10 CSIST-EXTER-CMARA    PIC X(4).
      *    *************************************************************
           10 CEVNTO-EXTER-CMARA   PIC X(3).
      *    *************************************************************
           10 CFLUXO-EXTER-CMARA   PIC X(3).
      *    *************************************************************
           10 CCTRL-RQUIS-ORIGN    PIC X(20).
      *    *************************************************************
           10 CCTRL-RQUIS-INSTC    PIC X(20).
      *    *************************************************************
           10 QMSGEM-ENVID         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 QMSGEM-RECBD         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 DINCL-RESUL-MNTRC    PIC X(10).
      *    *************************************************************
           10 HINCL-RESUL-MNTRC    PIC X(8).
      *    *************************************************************
           10 NRELTV-PROCM-RESUL   PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-RESUL-BACEN    PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
