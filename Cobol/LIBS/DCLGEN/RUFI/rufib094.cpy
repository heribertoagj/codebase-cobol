      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONTR_RURAL_OUTRO_CONFM)                  *
      *        LIBRARY(AD.DB2.DCLGEN(RUFIB094))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUFIB094)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONTR_RURAL_OUTRO_CONFM TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             COUTRO_TPO_CONFM               DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONTR_RURAL_OUTRO_CONFM    *
      ******************************************************************
       01  RUFIB094.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 COUTRO-TPO-CONFM     PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
