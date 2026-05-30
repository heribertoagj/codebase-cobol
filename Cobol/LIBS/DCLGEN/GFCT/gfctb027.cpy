***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.COMPS_GRP_LCTO)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB027))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB027)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.COMPS_GRP_LCTO TABLE
           ( CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             CIDTFD_SNAL_LCTO               DECIMAL(1, 0) NOT NULL,
             CBCO_CCREN                     DECIMAL(3, 0) NOT NULL,
             CTPO_PERIF                     DECIMAL(3, 0) NOT NULL,
             DINIC_GRP_LCTO                 DATE NOT NULL,
             CGRP_LCTO                      DECIMAL(5, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             DFIM_GRP_LCTO                  DATE NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.COMPS_GRP_LCTO              *
      ******************************************************************
       01  GFCTB027.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SNAL-LCTO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-CCREN           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PERIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-GRP-LCTO       PIC X(10).
      *    *************************************************************
           10 CGRP-LCTO            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-GRP-LCTO        PIC X(10).
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
