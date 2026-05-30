***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.COMPS_GRP_SERVC)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB028))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB028)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.COMPS_GRP_SERVC TABLE
           ( CGRP_SERVC                     DECIMAL(5, 0) NOT NULL,
             CSEQ_GRP_SERVC                 DECIMAL(3, 0) NOT NULL,
             DINIC_DEB_SERVC                DATE NOT NULL,
             DFIM_DEB_SERVC                 DATE NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CGRP_LCTO                      DECIMAL(5, 0) NOT NULL,
             QREST_UTILZ_TARIF              DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.COMPS_GRP_SERVC             *
      ******************************************************************
       01  GFCTB028.
      *    *************************************************************
           10 CGRP-SERVC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-GRP-SERVC       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-DEB-SERVC      PIC X(10).
      *    *************************************************************
           10 DFIM-DEB-SERVC       PIC X(10).
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
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
           10 CGRP-LCTO            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QREST-UTILZ-TARIF    PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
