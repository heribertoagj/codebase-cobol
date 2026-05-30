***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_EVNTO_CTBIL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB085))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB085)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_EVNTO_CTBIL TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_MANUT_SERVC             TIMESTAMP NOT NULL,
             CSEQ_MANUT_SERVC               DECIMAL(2, 0) NOT NULL,
             CTPO_LCTO_CTBIL                DECIMAL(2, 0) NOT NULL,
             CSEQ_EVNTO_CTBIL               DECIMAL(2, 0) NOT NULL,
             CINDCD_DEPDC_CTBIL             DECIMAL(1, 0) NOT NULL,
             PATRIC_CTBIL                   DECIMAL(7, 4) NOT NULL,
             CDEPDC_CTLZO                   DECIMAL(5, 0) NOT NULL,
             CPRODT_CTBIL                   DECIMAL(4, 0) NOT NULL,
             COPER_CTBIL                    CHAR(7) NOT NULL,
             CEMPR                          DECIMAL(5, 0) NOT NULL,
             CGRP_RZ_CTBIL                  DECIMAL(3, 0) NOT NULL,
             CSGRP_RZ_CTBIL                 DECIMAL(3, 0) NOT NULL,
             CCTA_EVNTO_CTBIL               DECIMAL(7, 0) NOT NULL,
             CDOCTO_CTBIL                   DECIMAL(7, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_EVNTO_CTBIL            *
      ******************************************************************
       01  GFCTB085.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-MANUT-SERVC   PIC X(26).
      *    *************************************************************
           10 CSEQ-MANUT-SERVC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-LCTO-CTBIL      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-EVNTO-CTBIL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-DEPDC-CTBIL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 PATRIC-CTBIL         PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-CTLZO         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-CTBIL         PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-CTBIL          PIC X(7).
      *    *************************************************************
           10 CEMPR                PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-RZ-CTBIL        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-RZ-CTBIL       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-EVNTO-CTBIL     PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-CTBIL         PIC S9(7)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
