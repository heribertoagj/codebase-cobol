      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TALCAN_OPER_ATIVO_GARNT)                   *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09I))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09I)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TALCAN_OPER_ATIVO_GARNT TABLE
           ( COPER_ENVID_CIPAG              CHAR(20) NOT NULL,
             CINDCD_ALCAN_CRDRA             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TALCAN_OPER_ATIVO_GARNT     *
      ******************************************************************
       01  BVVEB09I.
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      *    *************************************************************
           10 CINDCD-ALCAN-CRDRA   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
