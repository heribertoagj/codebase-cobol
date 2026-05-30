***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_FRANQ_FLEXZ)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0P7))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0P7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_FRANQ_FLEXZ TABLE
           ( CBCO_CLI                       DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             DMES_ANO_FRANQ                 DECIMAL(6, 0) NOT NULL,
             NCTRL_FRANQ_FLEXZ              DECIMAL(10, 0) NOT NULL,
             CGRP_SERVC                     DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_SERVC              DATE NOT NULL,
             QFRANQ_TARIF_CADTR             DECIMAL(7, 0) NOT NULL,
             QFRANQ_TARIF_UTLZD             DECIMAL(7, 0) NOT NULL,
             QFRANQ_TARIF_EXCED             DECIMAL(7, 0) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_FRANQ_FLEXZ           *
      ******************************************************************
       01  GFCTB0P7.
      *    *************************************************************
           10 CBCO-CLI             PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-ANO-FRANQ       PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 NCTRL-FRANQ-FLEXZ    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-SERVC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-SERVC    PIC X(10).
      *    *************************************************************
           10 QFRANQ-TARIF-CADTR   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QFRANQ-TARIF-UTLZD   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QFRANQ-TARIF-EXCED   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
