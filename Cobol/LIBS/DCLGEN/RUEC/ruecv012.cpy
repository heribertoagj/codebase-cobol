      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01EVNTO_RURAL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RUECV012))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECV012)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01EVNTO_RURAL TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             CTPO_EVNTO_RURAL               DECIMAL(4, 0) NOT NULL,
             DEVNTO                         DATE NOT NULL,
             VEVNTO_RURAL                   DECIMAL(15, 2) NOT NULL,
             VPCELA_REC_PPRIO               DECIMAL(15, 2) NOT NULL,
             DFIM_EVNTO_CDULA               DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01EVNTO_RURAL              *
      ******************************************************************
       01  RUECV012.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-EVNTO-RURAL     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 DEVNTO               PIC X(10).
      *    *************************************************************
           10 VEVNTO-RURAL         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPCELA-REC-PPRIO     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DFIM-EVNTO-CDULA     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
