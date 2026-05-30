      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PLANO_REFIN)                               *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB028))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB028)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PLANO_REFIN TABLE
           ( CPLANO_REFIN                   DECIMAL(3, 0) NOT NULL,
             IPLANO_REFIN                   CHAR(40) NOT NULL,
             CPLANO_REFIN_BACEN             DECIMAL(5, 0) NOT NULL,
             CPLANO_REFIN_RECOR             CHAR(4) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PLANO_REFIN                 *
      ******************************************************************
       01  RURCB028.
      *    *************************************************************
           10 CPLANO-REFIN         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IPLANO-REFIN         PIC X(40).
      *    *************************************************************
           10 CPLANO-REFIN-BACEN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPLANO-REFIN-RECOR   PIC X(4).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
