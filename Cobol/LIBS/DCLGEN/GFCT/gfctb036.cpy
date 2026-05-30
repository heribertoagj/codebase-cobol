***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.DEB_GRP_SERVC)                             *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB036))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB036)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.DEB_GRP_SERVC TABLE
           ( CGRP_SERVC                     DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_SERVC              DATE NOT NULL,
             CBCO_CCREN                     DECIMAL(3, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             CIDTFD_SNAL_LCTO               DECIMAL(1, 0) NOT NULL,
             CTPO_DEB_COBR                  DECIMAL(1, 0) NOT NULL,
             CINDCD_DEB_CONSL               CHAR(1) NOT NULL,
             QMAX_TENTV_DEB                 DECIMAL(3, 0) NOT NULL,
             QDIA_RTCAO_COBR                DECIMAL(2, 0) NOT NULL,
             VEXCED_COMPO_PCOTE             DECIMAL(11, 2) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CTPO_LCTO_COBR                 DECIMAL(2, 0) NOT NULL,
             CTPO_LCTO_ESTRN                DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.DEB_GRP_SERVC               *
      ******************************************************************
       01  GFCTB036.
      *    *************************************************************
           10 CGRP-SERVC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-SERVC    PIC X(10).
      *    *************************************************************
           10 CBCO-CCREN           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SNAL-LCTO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-DEB-COBR        PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-DEB-CONSL     PIC X(1).
      *    *************************************************************
           10 QMAX-TENTV-DEB       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QDIA-RTCAO-COBR      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VEXCED-COMPO-PCOTE   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CTPO-LCTO-COBR       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-LCTO-ESTRN      PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
