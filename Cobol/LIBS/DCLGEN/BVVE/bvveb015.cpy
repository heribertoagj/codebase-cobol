      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PARM_AFLIA_ESTBL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB015))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB015)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PARM_AFLIA_ESTBL TABLE
           ( CPARM_AFLIA_ESTBL              DECIMAL(3, 0) NOT NULL,
             CTPO_DADO_PARM                 CHAR(1),
             QTMNHO_MAX_PARM                DECIMAL(3, 0),
             QESCAL_PARM_NUMRC              DECIMAL(1, 0),
             IPARM_AFLIA_ESTBL              CHAR(30) NOT NULL,
             RPARM_AFLIA_ESTBL              CHAR(100)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PARM_AFLIA_ESTBL            *
      ******************************************************************
       01  BVVEB015.
      *    *************************************************************
           10 CPARM-AFLIA-ESTBL    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-DADO-PARM       PIC X(1).
      *    *************************************************************
           10 QTMNHO-MAX-PARM      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QESCAL-PARM-NUMRC    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IPARM-AFLIA-ESTBL    PIC X(30).
      *    *************************************************************
           10 RPARM-AFLIA-ESTBL    PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
