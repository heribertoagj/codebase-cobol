      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SIT_PPSTA_AFLIA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB012))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB012)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SIT_PPSTA_AFLIA TABLE
           ( CSIT_PPSTA_AFLIA               DECIMAL(2, 0) NOT NULL,
             RSIT_PPSTA_AFLIA               CHAR(20) NOT NULL,
             DDSATV_SIT_PPSTA               DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SIT_PPSTA_AFLIA             *
      ******************************************************************
       01  BVVEB012.
      *    *************************************************************
           10 CSIT-PPSTA-AFLIA     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RSIT-PPSTA-AFLIA     PIC X(20).
      *    *************************************************************
           10 DDSATV-SIT-PPSTA     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
