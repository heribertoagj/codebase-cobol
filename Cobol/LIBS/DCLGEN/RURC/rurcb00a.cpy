      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TBASE_LEGAL_RENEG)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB00A))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB00A)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TBASE_LEGAL_RENEG TABLE
           ( NBASE_LEGAL_RENEG              DECIMAL(5, 0) NOT NULL,
             RBASE_LEGAL_RENEG              CHAR(100) NOT NULL,
             CBASE_LEGAL_RENEG              CHAR(5) NOT NULL,
             CSIT_BASE_LEGAL                DECIMAL(1, 0) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TBASE_LEGAL_RENEG           *
      ******************************************************************
       01  RURCB00A.
      *    *************************************************************
           10 NBASE-LEGAL-RENEG    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RBASE-LEGAL-RENEG    PIC X(100).
      *    *************************************************************
           10 CBASE-LEGAL-RENEG    PIC X(5).
      *    *************************************************************
           10 CSIT-BASE-LEGAL      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
