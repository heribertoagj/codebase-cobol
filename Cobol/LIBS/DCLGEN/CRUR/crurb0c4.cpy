      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPPRIE_CREDT_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0C4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0C4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPPRIE_CREDT_RURAL TABLE
           ( CIMOV_CREDT_RURAL              DECIMAL(10, 0) NOT NULL,
             CPSSOA_PPRIE_IMOV              DECIMAL(10, 0) NOT NULL,
             PPRTCP_PPRIE_RURAL             DECIMAL(5, 2) NOT NULL,
             DVCTO_CONTR_EXPLO              DATE,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPPRIE_CREDT_RURAL          *
      ******************************************************************
       01  CRURB0C4.
      *    *************************************************************
           10 CIMOV-CREDT-RURAL    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-PPRIE-IMOV    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 PPRTCP-PPRIE-RURAL   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-CONTR-EXPLO    PIC X(10).
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
