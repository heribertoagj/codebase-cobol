      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSIT_PPSTA_PCERO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB023))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB023)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSIT_PPSTA_PCERO TABLE
           ( CSIT_AFLIA_PCERO               CHAR(1) NOT NULL,
             ISIT_AFLIA_PCERO               CHAR(60) NOT NULL,
             CACTAC_CORRC_PPSTA             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSIT_PPSTA_PCERO            *
      ******************************************************************
       01  BVVEB023.
      *    *************************************************************
           10 CSIT-AFLIA-PCERO     PIC X(1).
      *    *************************************************************
           10 ISIT-AFLIA-PCERO     PIC X(60).
      *    *************************************************************
           10 CACTAC-CORRC-PPSTA   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
