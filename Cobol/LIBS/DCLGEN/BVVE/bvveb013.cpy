      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_ENDER_AFLIA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB013))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB013)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_ENDER_AFLIA TABLE
           ( CTPO_ENDER_AFLIA               DECIMAL(1, 0) NOT NULL,
             RTPO_ENDER_AFLIA               CHAR(20) NOT NULL,
             DDSATV_TPO_ENDER               DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_ENDER_AFLIA             *
      ******************************************************************
       01  BVVEB013.
      *    *************************************************************
           10 CTPO-ENDER-AFLIA     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-ENDER-AFLIA     PIC X(20).
      *    *************************************************************
           10 DDSATV-TPO-ENDER     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
