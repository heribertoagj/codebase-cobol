***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_COMPS_PCOTE)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB084))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB084)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_COMPS_PCOTE TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_MANUT_SERVC             TIMESTAMP NOT NULL,
             CSEQ_MANUT_SERVC               DECIMAL(2, 0) NOT NULL,
             CSEQ_COMPO_PCOTE               DECIMAL(3, 0) NOT NULL,
             DINIC_VGCIA_COMPS              DATE NOT NULL,
             DFIM_VGCIA_COMPS               DATE NOT NULL,
             CGRP_SERVC                     DECIMAL(5, 0) NOT NULL,
             CTARIF_COMPO_PCOTE             DECIMAL(5, 0) NOT NULL,
             QFRANQ_COMPO_PCOTE             DECIMAL(3, 0) NOT NULL,
             CPER_COBR_COMPO                DECIMAL(1, 0) NOT NULL,
             VEXCED_COMPO_PCOTE             DECIMAL(11, 2) NOT NULL,
             PEXCED_COMPO_PCOTE             DECIMAL(7, 4) NOT NULL,
             QDIA_COBR_COMPO                DECIMAL(2, 0) NOT NULL,
             RSEGDA_LIN_EXCED               CHAR(32) NOT NULL,
             DDIA_COBR_TARIF                DECIMAL(2, 0) NOT NULL,
             CIDTFD_COBR_FERI               CHAR(1) NOT NULL,
             CIDTFD_VLR_EXCED               DECIMAL(1, 0) NOT NULL,
             CIDTFD_CTRL_UTILZ              DECIMAL(1, 0) NOT NULL,
             CINDCD_ISENC_UTILZ             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_COMPS_PCOTE            *
      ******************************************************************
       01  GFCTB084.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-MANUT-SERVC   PIC X(26).
      *    *************************************************************
           10 CSEQ-MANUT-SERVC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-COMPO-PCOTE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-COMPS    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-COMPS     PIC X(10).
      *    *************************************************************
           10 CGRP-SERVC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTARIF-COMPO-PCOTE   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QFRANQ-COMPO-PCOTE   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPER-COBR-COMPO      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VEXCED-COMPO-PCOTE   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PEXCED-COMPO-PCOTE   PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 QDIA-COBR-COMPO      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RSEGDA-LIN-EXCED     PIC X(32).
      *    *************************************************************
           10 DDIA-COBR-TARIF      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-COBR-FERI     PIC X(1).
      *    *************************************************************
           10 CIDTFD-VLR-EXCED     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-CTRL-UTILZ    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ISENC-UTILZ   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
