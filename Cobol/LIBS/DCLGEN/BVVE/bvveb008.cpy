      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ERRO_PREEN_AFLIA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB008))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB008)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ERRO_PREEN_AFLIA TABLE
           ( CERRO_PREEN_AFLIA              DECIMAL(2, 0) NOT NULL,
             RERRO_PREEN_AFLIA              CHAR(60) NOT NULL,
             DDSATV_ERRO_PREEN              DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ERRO_PREEN_AFLIA            *
      ******************************************************************
       01  BVVEB008.
      *    *************************************************************
           10 CERRO-PREEN-AFLIA    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RERRO-PREEN-AFLIA    PIC X(60).
      *    *************************************************************
           10 DDSATV-ERRO-PREEN    PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
