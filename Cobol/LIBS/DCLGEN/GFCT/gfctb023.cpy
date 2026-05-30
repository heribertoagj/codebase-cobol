***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.BONIF_SERVC_TARIF)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB023))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB023)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.BONIF_SERVC_TARIF TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CTPO_BONIF                     DECIMAL(3, 0) NOT NULL,
             CCRITR_BONIF                   DECIMAL(3, 0) NOT NULL,
             DINIC_REGRA_BONIF              DATE NOT NULL,
             CSEQ_BONIF_SERVC               DECIMAL(3, 0) NOT NULL,
             DFIM_REGRA_BONIF               DATE NOT NULL,
             VCOBR_BONIF                    DECIMAL(11, 2) NOT NULL,
             PDESC_BONIF                    DECIMAL(7, 4) NOT NULL,
             QFRANQ_BONIF                   DECIMAL(3, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CCART_ANLSE_GERC               DECIMAL(5, 0) NOT NULL,
             CGRP_CART_SIST                 DECIMAL(5, 0) NOT NULL,
             CIDTFD_TPO_FAIXA               DECIMAL(1, 0) NOT NULL,
             CINDCD_DESC_RLCTO              CHAR(1) NOT NULL,
             CINDCD_PERDC_COBR              CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.BONIF_SERVC_TARIF           *
      ******************************************************************
       01  GFCTB023.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BONIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCRITR-BONIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-REGRA-BONIF    PIC X(10).
      *    *************************************************************
           10 CSEQ-BONIF-SERVC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-REGRA-BONIF     PIC X(10).
      *    *************************************************************
           10 VCOBR-BONIF          PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDESC-BONIF          PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 QFRANQ-BONIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CCART-ANLSE-GERC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CART-SIST       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-TPO-FAIXA     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-DESC-RLCTO    PIC X(1).
      *    *************************************************************
           10 CINDCD-PERDC-COBR    PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
