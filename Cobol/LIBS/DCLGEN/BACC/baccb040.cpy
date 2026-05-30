      ******************************************************************
      * DCLGEN TABLE(DB2PRD.LIM_CTA_CATAO)                             *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB040))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB040)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.LIM_CTA_CATAO TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CIDTFD_PRODT_CATAO             DECIMAL(5, 0) NOT NULL,
             PSDO_ANTEC_CREDT               DECIMAL(5, 2) NOT NULL,
             VMOVTC_MAX_ANTEC               DECIMAL(15, 2) NOT NULL,
             VTOT_CREDT_ESTBL               DECIMAL(15, 2) NOT NULL,
             VTOT_DEB_ESTBL                 DECIMAL(15, 2) NOT NULL,
             VDISPN_ANTEC_CREDT             DECIMAL(15, 2) NOT NULL,
             VTOT_MOVTC_ANTEC               DECIMAL(15, 2) NOT NULL,
             VTOT_MOVTO_CREDT               DECIMAL(15, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.LIM_CTA_CATAO               *
      ******************************************************************
       01  BACCB040.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-CATAO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 PSDO-ANTEC-CREDT     PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMOVTC-MAX-ANTEC     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-CREDT-ESTBL     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-DEB-ESTBL       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VDISPN-ANTEC-CREDT   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-MOVTC-ANTEC     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-MOVTO-CREDT     PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
