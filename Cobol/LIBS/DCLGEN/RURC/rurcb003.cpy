      ******************************************************************
      * DCLGEN TABLE(DB2PRD.FNALD_RURAL)                               *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB003))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB003)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.FNALD_RURAL TABLE
           ( CFNALD_RURAL                   DECIMAL(3, 0) NOT NULL,
             IFNALD_RURAL                   CHAR(40) NOT NULL,
             CAPLIC_RURAL                   DECIMAL(3, 0) NOT NULL,
             CAGRIC_RECOR                   DECIMAL(5, 0) NOT NULL,
             CPCUAR_RECOR                   DECIMAL(3, 0) NOT NULL,
             CFNALD_RECOR                   CHAR(5) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.FNALD_RURAL                 *
      ******************************************************************
       01  RURCB003.
      *    *************************************************************
           10 CFNALD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IFNALD-RURAL         PIC X(40).
      *    *************************************************************
           10 CAPLIC-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAGRIC-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPCUAR-RECOR         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFNALD-RECOR         PIC X(5).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
