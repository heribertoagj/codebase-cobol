      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_DEPDC_BCO)                             *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0F7))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0F7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_DEPDC_BCO TABLE
           ( CIDTFD_DEPDC                   CHAR(2) NOT NULL,
             RIDTFD_DEPDC                   CHAR(30) NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_DEPDC_BCO               *
      ******************************************************************
       01  GFCTB0F7.
      *    *************************************************************
           10 CIDTFD-DEPDC         PIC X(2).
      *    *************************************************************
           10 RIDTFD-DEPDC         PIC X(30).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
