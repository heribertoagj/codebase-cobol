      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01TPO_SIT_REMESSA)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV010))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV010)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01TPO_SIT_REMESSA TABLE
           ( CSIT_REMSS_ORPAG               DECIMAL(2, 0) NOT NULL,
             RSIT_REMSS_ORPAG               CHAR(15) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01TPO_SIT_REMESSA          *
      ******************************************************************
       01  BACCV010.
      *    *************************************************************
           10 CSIT-REMSS-ORPAG     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RSIT-REMSS-ORPAG     PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
