      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01ATIVIDADE_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RURCV004))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCV004)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01ATIVIDADE_RURAL TABLE
           ( CATVDD_RURAL                   DECIMAL(3, 0) NOT NULL,
             IATVDD_RURAL                   CHAR(40) NOT NULL,
             CATVDD_RECOR                   DECIMAL(5, 0) NOT NULL,
             CATVDD_BACEN                   CHAR(5) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01ATIVIDADE_RURAL          *
      ******************************************************************
       01  RURCV004.
      *    *************************************************************
           10 CATVDD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IATVDD-RURAL         PIC X(40).
      *    *************************************************************
           10 CATVDD-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CATVDD-BACEN         PIC X(5).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
