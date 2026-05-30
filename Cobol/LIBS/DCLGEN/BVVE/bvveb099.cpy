      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TUSUAR_PRFIL_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB099))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB099)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TUSUAR_PRFIL_CATAO TABLE
           ( CUSUAR_MANUT                   CHAR(7) NOT NULL,
             CPRFIL_ACSSO_MSGRA             DECIMAL(4, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TUSUAR_PRFIL_CATAO          *
      ******************************************************************
       01  BVVEB099.
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 CPRFIL-ACSSO-MSGRA   PIC S9(4)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
