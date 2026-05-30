      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOTVO_DCLAS_CONTR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUFIB044))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUFIB044)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOTVO_DCLAS_CONTR TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             NMOTVO_DCLAS_CONTR             DECIMAL(5, 0) NOT NULL,
             CMOTVO_DCLAS_OPER              DECIMAL(5, 0) NOT NULL,
             DDCLAS_OPER_CREDT              DATE NOT NULL,
             VPRINC_DCLAS_CONTR             DECIMAL(15, 2) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL                          TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CREFT_RECOR                    CHAR(11)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOTVO_DCLAS_CONTR          *
      ******************************************************************
       01  RUFIB044.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NMOTVO-DCLAS-CONTR   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-DCLAS-OPER    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DDCLAS-OPER-CREDT    PIC X(10).
      *    *************************************************************
           10 VPRINC-DCLAS-CONTR   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CREFT-RECOR          PIC X(11).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
