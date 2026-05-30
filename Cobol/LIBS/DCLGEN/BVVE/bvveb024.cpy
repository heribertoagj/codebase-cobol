      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSIT_PPSTA_ESTBL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB024))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB024)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSIT_PPSTA_ESTBL TABLE
           ( CSIT_AFLIA_PCERO               CHAR(1) NOT NULL,
             CSIT_PPSTA_AFLIA               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSIT_PPSTA_ESTBL            *
      ******************************************************************
       01  BVVEB024.
      *    *************************************************************
           10 CSIT-AFLIA-PCERO     PIC X(1).
      *    *************************************************************
           10 CSIT-PPSTA-AFLIA     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
