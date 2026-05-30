***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOPER_FONE_CREDT)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0M4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0M4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOPER_FONE_CREDT TABLE
           ( COPER_FONE_MOVEL               DECIMAL(9, 0) NOT NULL,
             CDDD_FONE_MOVEL                DECIMAL(3, 0) NOT NULL,
             ROPER_FONE_MOVEL               CHAR(40) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOPER_FONE_CREDT            *
      ******************************************************************
       01  GFCTB0M4.
      *    *************************************************************
           10 COPER-FONE-MOVEL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CDDD-FONE-MOVEL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 ROPER-FONE-MOVEL     PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
