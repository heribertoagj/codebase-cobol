      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRFIL_ACSSO_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB091))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB091)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRFIL_ACSSO_CATAO TABLE
           ( CPRFIL_ACSSO_MSGRA             DECIMAL(4, 0) NOT NULL,
             IPRFIL_ACSSO_MSGRA             CHAR(25) NOT NULL,
             CSIT_REG                       DECIMAL(1, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRFIL_ACSSO_CATAO          *
      ******************************************************************
       01  BVVEB091.
      *    *************************************************************
           10 CPRFIL-ACSSO-MSGRA   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 IPRFIL-ACSSO-MSGRA   PIC X(25).
      *    *************************************************************
           10 CSIT-REG             PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
