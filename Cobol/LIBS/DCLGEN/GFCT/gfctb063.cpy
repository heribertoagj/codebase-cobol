***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.GRP_SERVC_PCOTE)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB063))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB063)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.GRP_SERVC_PCOTE TABLE
           ( CGRP_SERVC                     DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_SERVC              DATE NOT NULL,
             DFIM_VGCIA_SERVC               DATE NOT NULL,
             RGRP_SERVC                     CHAR(30) NOT NULL,
             CINDCD_GRP_TARIF               CHAR(1) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.GRP_SERVC_PCOTE             *
      ******************************************************************
       01  GFCTB063.
      *    *************************************************************
           10 CGRP-SERVC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-SERVC    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-SERVC     PIC X(10).
      *    *************************************************************
           10 RGRP-SERVC           PIC X(30).
      *    *************************************************************
           10 CINDCD-GRP-TARIF     PIC X(1).
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
