      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ESPCE_TITLO_GARNT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB018))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB018)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ESPCE_TITLO_GARNT TABLE
           ( CESPCE_TITLO_GARNT             DECIMAL(3, 0) NOT NULL,
             IESPCE_TITLO_GARNT             CHAR(40) NOT NULL,
             CTPO_GARNT                     DECIMAL(3, 0),
             CTPO_GARNT_RECOR               DECIMAL(3, 0) NOT NULL,
             RRSUMO_ESPCE_TITLO             CHAR(5)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ESPCE_TITLO_GARNT           *
      ******************************************************************
       01  RURCB018.
      *    *************************************************************
           10 CESPCE-TITLO-GARNT   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IESPCE-TITLO-GARNT   PIC X(40).
      *    *************************************************************
           10 CTPO-GARNT           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-GARNT-RECOR     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RRSUMO-ESPCE-TITLO   PIC X(5).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
