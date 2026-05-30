      ******************************************************************
      * DCLGEN TABLE(DB2PRD.NAVEGACAO_RURAL)                           *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB042))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB042)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.NAVEGACAO_RURAL TABLE
           ( CTERM                          CHAR(8) NOT NULL,
             WNAVG_CREDT_RURAL              VARCHAR(4000) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.NAVEGACAO_RURAL             *
      ******************************************************************
       01  RUECB042.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 WNAVG-CREDT-RURAL.
              49 WNAVG-CREDT-RURAL-LEN  PIC S9(4) USAGE COMP.
              49 WNAVG-CREDT-RURAL-TEXT  PIC X(4000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
