      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01PROVS_DIA_INSTC)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV001))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV001)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01PROVS_DIA_INSTC TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             DCALC_PROVS                    DATE NOT NULL,
             VPROVS_MOEDA_CORR              DECIMAL(15, 2) NOT NULL,
             VPROVS_CALCD_MOEDA             DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01PROVS_DIA_INSTC          *
      ******************************************************************
       01  BACCV001.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DCALC-PROVS          PIC X(10).
      *    *************************************************************
           10 VPROVS-MOEDA-CORR    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPROVS-CALCD-MOEDA   PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
