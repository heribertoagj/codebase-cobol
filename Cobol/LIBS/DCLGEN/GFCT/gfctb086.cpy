***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_PARM_TARIF)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB086))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB086)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_PARM_TARIF TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_MANUT_SERVC             TIMESTAMP NOT NULL,
             CSEQ_MANUT_SERVC               DECIMAL(2, 0) NOT NULL,
             DINIC_VGCIA_TARIF              DATE NOT NULL,
             DFIM_VGCIA_TARIF               DATE NOT NULL,
             CNATUZ_SERVC                   DECIMAL(3, 0) NOT NULL,
             CCJTO_SERVC                    DECIMAL(3, 0) NOT NULL,
             QMAX_TENTV_DEB                 DECIMAL(3, 0) NOT NULL,
             QFRANQ_TARIF                   DECIMAL(3, 0) NOT NULL,
             CPER_FRANQ_TARIF               DECIMAL(1, 0) NOT NULL,
             CPER_CALC_TARIF                DECIMAL(1, 0) NOT NULL,
             CINIC_PER_CALC                 DECIMAL(1, 0) NOT NULL,
             DDIA_COBR_TARIF                DECIMAL(2, 0) NOT NULL,
             QDIA_CAREN_TARIF               DECIMAL(3, 0) NOT NULL,
             PDESC_CAREN_TARIF              DECIMAL(7, 4) NOT NULL,
             CINDCD_INIC_CAREN              DECIMAL(1, 0) NOT NULL,
             DINIC_CAREN_TARIF              DATE NOT NULL,
             CINDCD_DEB_CONSL               CHAR(1) NOT NULL,
             CINDCD_EXCED_CONSL             CHAR(1) NOT NULL,
             RSEGDA_LIN_EXTRT               CHAR(32) NOT NULL,
             QDIA_RTCAO_COBR                DECIMAL(2, 0) NOT NULL,
             CINIC_PER_FRANQ                DECIMAL(1, 0) NOT NULL,
             VMIN_ENVIO_COBR                DECIMAL(11, 2) NOT NULL,
             CINDCD_TARIF_PROML             CHAR(1) NOT NULL,
             CINDCD_TENTV_COBR              CHAR(1) NOT NULL,
             QTENTV_SEM_COBR                DECIMAL(3, 0) NOT NULL,
             CINDCD_RETOR_COBR              CHAR(1) NOT NULL,
             CINDCD_CALC_PRORT              CHAR(1) NOT NULL,
             CINDCD_PER_CAREN               DECIMAL(1, 0) NOT NULL,
             QMES_CAREN_TARIF               DECIMAL(2, 0) NOT NULL,
             QDIA_COBR_TARIF                DECIMAL(2, 0) NOT NULL,
             CIDTFD_COBR_FERI               CHAR(1) NOT NULL,
             CINDCD_PCMTO_COBR              CHAR(1) NOT NULL,
             QPCELA_COBR_TARIF              DECIMAL(2, 0) NOT NULL,
             CIDTFD_COBR_PCELA              CHAR(1) NOT NULL,
             QDIA_COBR_PCELA                DECIMAL(3, 0) NOT NULL,
             CINDCD_CREDT_CLULR             CHAR(1) NOT NULL,
             CINDCD_REPAS_CLULR             DECIMAL(1, 0) NOT NULL,
             VREPAS_CREDT_CLULR             DECIMAL(11, 2) NOT NULL,
             QDIA_RTCAO_CREDT               DECIMAL(3, 0) NOT NULL,
             CTPO_FRANQ_TARIF               DECIMAL(1, 0) NOT NULL,
             CINDCD_PER_COBR                DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_PARM_TARIF             *
      ******************************************************************
       01  GFCTB086.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-MANUT-SERVC   PIC X(26).
      *    *************************************************************
           10 CSEQ-MANUT-SERVC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-TARIF    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-TARIF     PIC X(10).
      *    *************************************************************
           10 CNATUZ-SERVC         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCJTO-SERVC          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-TENTV-DEB       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QFRANQ-TARIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPER-FRANQ-TARIF     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPER-CALC-TARIF      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINIC-PER-CALC       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DDIA-COBR-TARIF      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QDIA-CAREN-TARIF     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 PDESC-CAREN-TARIF    PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-INIC-CAREN    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-CAREN-TARIF    PIC X(10).
      *    *************************************************************
           10 CINDCD-DEB-CONSL     PIC X(1).
      *    *************************************************************
           10 CINDCD-EXCED-CONSL   PIC X(1).
      *    *************************************************************
           10 RSEGDA-LIN-EXTRT     PIC X(32).
      *    *************************************************************
           10 QDIA-RTCAO-COBR      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINIC-PER-FRANQ      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VMIN-ENVIO-COBR      PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TARIF-PROML   PIC X(1).
      *    *************************************************************
           10 CINDCD-TENTV-COBR    PIC X(1).
      *    *************************************************************
           10 QTENTV-SEM-COBR      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-RETOR-COBR    PIC X(1).
      *    *************************************************************
           10 CINDCD-CALC-PRORT    PIC X(1).
      *    *************************************************************
           10 CINDCD-PER-CAREN     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QMES-CAREN-TARIF     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QDIA-COBR-TARIF      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-COBR-FERI     PIC X(1).
      *    *************************************************************
           10 CINDCD-PCMTO-COBR    PIC X(1).
      *    *************************************************************
           10 QPCELA-COBR-TARIF    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-COBR-PCELA    PIC X(1).
      *    *************************************************************
           10 QDIA-COBR-PCELA      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CREDT-CLULR   PIC X(1).
      *    *************************************************************
           10 CINDCD-REPAS-CLULR   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VREPAS-CREDT-CLULR   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QDIA-RTCAO-CREDT     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-FRANQ-TARIF     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PER-COBR      PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 42      *
      ******************************************************************
