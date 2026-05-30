      ******************************************************************
      * DCLGEN TABLE(DB2PRD.OBS_DEVLC_RURAL)                           *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB045))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB045)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.OBS_DEVLC_RURAL TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             CLIN_OBS_DEVLC                 DECIMAL(2, 0) NOT NULL,
             RLIN_OBS_DEVLC                 CHAR(79) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.OBS_DEVLC_RURAL             *
      ******************************************************************
       01  RUECB045.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CLIN-OBS-DEVLC       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RLIN-OBS-DEVLC       PIC X(79).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
