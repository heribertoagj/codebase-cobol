      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRESUL_MSGRA_MES)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB095))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB095)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRESUL_MSGRA_MES TABLE
           ( NDIA_MNTRC_MSGRA               DECIMAL(2, 0) NOT NULL,
             NRESUL_MNTRC_MSGRA             DECIMAL(18, 0) NOT NULL,
             CMNTRC_MSGRA                   CHAR(23),
             CFASE_PROCM                    DECIMAL(4, 0),
             CSNLZD_MSGRA                   DECIMAL(3, 0),
             CISPB_EMISR_MSGEM              CHAR(8),
             CISPB_DSTNA_MSGEM              CHAR(8),
             CSIST_EXTER                    CHAR(3),
             CEVNTO_MSGEM                   CHAR(4),
             CFLUXO_MSGEM                   CHAR(2),
             CUNIC_TRFGO_MSGEM              CHAR(23) NOT NULL,
             CCTRL_RQUIS_INSTC              CHAR(20) NOT NULL,
             QMSGEM_ENVID                   DECIMAL(10, 0) NOT NULL,
             QMSGEM_RECBD                   DECIMAL(10, 0) NOT NULL,
             DINCL_RESUL_MNTRC              DATE NOT NULL,
             HINCL_RESUL_MNTRC              TIME NOT NULL,
             NRELTV_PROCM_RESUL             DECIMAL(15, 0) NOT NULL,
             HINCL_RESUL_BACEN              TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRESUL_MSGRA_MES            *
      ******************************************************************
       01  BVVEB095.
      *    *************************************************************
           10 NDIA-MNTRC-MSGRA     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 NRESUL-MNTRC-MSGRA   PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CMNTRC-MSGRA         PIC X(23).
      *    *************************************************************
           10 CFASE-PROCM          PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CSNLZD-MSGRA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CISPB-EMISR-MSGEM    PIC X(8).
      *    *************************************************************
           10 CISPB-DSTNA-MSGEM    PIC X(8).
      *    *************************************************************
           10 CSIST-EXTER          PIC X(3).
      *    *************************************************************
           10 CEVNTO-MSGEM         PIC X(4).
      *    *************************************************************
           10 CFLUXO-MSGEM         PIC X(2).
      *    *************************************************************
           10 CUNIC-TRFGO-MSGEM    PIC X(23).
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
