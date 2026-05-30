      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMNTRC_TRFGO_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB084))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB084)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMNTRC_TRFGO_CATAO TABLE
           ( NMNTRC_TRFGO_ARQ               DECIMAL(18, 0) NOT NULL,
             NESTRT_INFO_BACEN              DECIMAL(3, 0) NOT NULL,
             NVRSAO_ESTRT_INFO              DECIMAL(3, 0) NOT NULL,
             CERRO_MSGEM_SIST               CHAR(8),
             CIDTFD_ORIGE_ARQ               DECIMAL(1, 0),
             IARQ_MNTRC                     CHAR(80),
             CCTRL_RQUIS_INSTC              CHAR(20),
             CCTRL_RQUIS_ORIGN              CHAR(20),
             CSIT_RQUIS_ARQ                 DECIMAL(2, 0),
             HPROCM_ARQ                     TIMESTAMP,
             DMOVTO_INFO_ARQ                DATE,
             CIDTFD_SEQ_ARQ                 CHAR(1),
             NTMNHO_ARQ_ORIGN               DECIMAL(7, 3),
             NTMNHO_ARQ_FNAL                DECIMAL(7, 3),
             EARQ_ORIGN                     CHAR(200),
             EARQ_FNAL                      CHAR(200),
             EARQ_PROCM                     CHAR(200),
             CSNLZD_MSGRA                   DECIMAL(3, 0),
             CPROCS_ENTRG_BACEN             CHAR(100),
             NMNTRC_TRFGO_ORIGE             DECIMAL(18, 0),
             HENVIO_REG_PLATF               TIMESTAMP,
             CINDCD_PLATF_ORIGE             DECIMAL(1, 0),
             CISPB_EMISR_MSGEM              CHAR(8),
             CISPB_DSTNA_MSGEM              CHAR(8)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMNTRC_TRFGO_CATAO          *
      ******************************************************************
       01  BVVEB084.
      *    *************************************************************
           10 NMNTRC-TRFGO-ARQ     PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 NESTRT-INFO-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NVRSAO-ESTRT-INFO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CERRO-MSGEM-SIST     PIC X(8).
      *    *************************************************************
           10 CIDTFD-ORIGE-ARQ     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IARQ-MNTRC           PIC X(80).
      *    *************************************************************
           10 CCTRL-RQUIS-INSTC    PIC X(20).
      *    *************************************************************
           10 CCTRL-RQUIS-ORIGN    PIC X(20).
      *    *************************************************************
           10 CSIT-RQUIS-ARQ       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HPROCM-ARQ           PIC X(26).
      *    *************************************************************
           10 DMOVTO-INFO-ARQ      PIC X(10).
      *    *************************************************************
           10 CIDTFD-SEQ-ARQ       PIC X(1).
      *    *************************************************************
           10 NTMNHO-ARQ-ORIGN     PIC S9(4)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 NTMNHO-ARQ-FNAL      PIC S9(4)V9(3) USAGE COMP-3.
      *    *************************************************************
           10 EARQ-ORIGN           PIC X(200).
      *    *************************************************************
           10 EARQ-FNAL            PIC X(200).
      *    *************************************************************
           10 EARQ-PROCM           PIC X(200).
      *    *************************************************************
           10 CSNLZD-MSGRA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPROCS-ENTRG-BACEN   PIC X(100).
      *    *************************************************************
           10 NMNTRC-TRFGO-ORIGE   PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 HENVIO-REG-PLATF     PIC X(26).
      *    *************************************************************
           10 CINDCD-PLATF-ORIGE   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CISPB-EMISR-MSGEM    PIC X(8).
      *    *************************************************************
           10 CISPB-DSTNA-MSGEM    PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
