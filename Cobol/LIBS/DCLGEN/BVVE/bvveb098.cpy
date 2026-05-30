      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSNLZD_MSGRA_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB098))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB098)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSNLZD_MSGRA_CATAO TABLE
           ( CSNLZD_MSGRA                   DECIMAL(3, 0) NOT NULL,
             RSNLZD_MSGRA                   CHAR(100) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSNLZD_MSGRA_CATAO          *
      ******************************************************************
       01  BVVEB098.
      *    *************************************************************
           10 CSNLZD-MSGRA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RSNLZD-MSGRA         PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
