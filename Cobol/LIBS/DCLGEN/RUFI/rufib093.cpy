      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRENEG_CONTR_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUFIB093))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUFIB093)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRENEG_CONTR_RURAL TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             NRENEG_CONTR_RURAL             DECIMAL(5, 0) NOT NULL,
             NBASE_LEGAL_RENEG              DECIMAL(5, 0),
             DRENEG_CONTR_RURAL             DATE NOT NULL,
             VRENEG_CONTR_RURAL             DECIMAL(17, 2) NOT NULL,
             DVCTO_RENEG_RURAL              DATE,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRENEG_CONTR_RURAL          *
      ******************************************************************
       01  RUFIB093.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NRENEG-CONTR-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NBASE-LEGAL-RENEG    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DRENEG-CONTR-RURAL   PIC X(10).
      *    *************************************************************
           10 VRENEG-CONTR-RURAL   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-RENEG-RURAL    PIC X(10).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
