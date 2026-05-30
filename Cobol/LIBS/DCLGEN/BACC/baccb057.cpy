      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SEDEX_CATAO_PTDOR)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB057))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB057)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SEDEX_CATAO_PTDOR TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             DEMIS                          DATE NOT NULL,
             DVALDD_CATAO                   DECIMAL(6, 0) NOT NULL,
             CSEDEX_CATAO_CREDT             DECIMAL(9, 0) NOT NULL,
             CORIGE_SEDEX_CATAO             DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SEDEX_CATAO_PTDOR           *
      ******************************************************************
       01  BACCB057.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 DEMIS                PIC X(10).
      *    *************************************************************
           10 DVALDD-CATAO         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CSEDEX-CATAO-CREDT   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-SEDEX-CATAO   PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
