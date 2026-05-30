***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONSL_PCELA_TARIF)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0I2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0I2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONSL_PCELA_TARIF TABLE
           ( CINDCD_TARIF_GRP               DECIMAL(1, 0) NOT NULL,
             CTARIF_GRP_SERVC               DECIMAL(5, 0) NOT NULL,
             DREFT_CONSL_PCELA              DECIMAL(6, 0) NOT NULL,
             CTPO_CLI                       DECIMAL(2, 0) NOT NULL,
             CTOT_PCELA_TARIF               DECIMAL(5, 0) NOT NULL,
             CPCELA_TARIF                   DECIMAL(3, 0) NOT NULL,
             VTARIF_MOVTO_BRUTO             DECIMAL(12, 2) NOT NULL,
             VTARIF_MOVTO_LIQ               DECIMAL(12, 2) NOT NULL,
             VTARIF_MOVTO_DEB               DECIMAL(12, 2) NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONSL_PCELA_TARIF          *
      ******************************************************************
       01  GFCTB0I2.
      *    *************************************************************
           10 CINDCD-TARIF-GRP     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTARIF-GRP-SERVC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DREFT-CONSL-PCELA    PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CLI             PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTOT-PCELA-TARIF     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPCELA-TARIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-MOVTO-BRUTO   PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-MOVTO-LIQ     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-MOVTO-DEB     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
