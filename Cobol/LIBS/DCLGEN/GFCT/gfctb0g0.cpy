      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_OPER_MOTVO)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0G0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0G0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_OPER_MOTVO TABLE
           ( CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             RTPO_OPER_MOTVO                CHAR(100) NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL,
             CIDTFD_UTILZ_ALCAD             DECIMAL(1, 0) NOT NULL,
             CIDTFD_UTILZ_GSTAO             DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_OPER_MOTVO              *
      ******************************************************************
       01  GFCTB0G0.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-OPER-MOTVO      PIC X(100).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-UTILZ-ALCAD   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-UTILZ-GSTAO   PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
