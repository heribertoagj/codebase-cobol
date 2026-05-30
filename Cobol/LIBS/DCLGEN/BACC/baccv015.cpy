      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01TPO_CATAO_CREDT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV015))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV015)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01TPO_CATAO_CREDT TABLE
           ( CTPO_CATAO_CREDT               DECIMAL(2, 0) NOT NULL,
             RTPO_CATAO_CREDT               CHAR(20) NOT NULL,
             CTRATO_DSBTO_ORPAG             DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01TPO_CATAO_CREDT          *
      ******************************************************************
       01  BACCV015.
      *    *************************************************************
           10 CTPO-CATAO-CREDT     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-CATAO-CREDT     PIC X(20).
      *    *************************************************************
           10 CTRATO-DSBTO-ORPAG   PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
