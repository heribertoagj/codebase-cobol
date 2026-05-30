      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CTRL_CDULA)                             *
      *        LIBRARY(AD.DB2.DCLGEN(RUECV001))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECV001)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CTRL_CDULA TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCDULA                         DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CTRL_CDULA               *
      ******************************************************************
       01  RUECV001.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
