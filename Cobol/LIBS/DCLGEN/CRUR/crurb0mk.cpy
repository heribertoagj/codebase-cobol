      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPCELA_PGTO_CONTR)                         *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0MK))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0MK)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPCELA_PGTO_CONTR TABLE
           ( CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             NPCELA_CONTR_RURAL             DECIMAL(3, 0) NOT NULL,
             CPSSOA_JURID_ORIGN             DECIMAL(10, 0),
             CTPO_CONTR_ORIGN               DECIMAL(3, 0),
             NCONTR_CREDT_ORIGN             DECIMAL(10, 0),
             NPCELA_CONTR_ORIGN             DECIMAL(3, 0),
             CORCAM_CONTR_RURAL             DECIMAL(10, 0) NOT NULL,
             CSIT_PCELA_RURAL               DECIMAL(2, 0) NOT NULL,
             CFORMA_LIQDC                   DECIMAL(2, 0),
             CSIT_GERAC_PCELA               CHAR(1) NOT NULL,
             DVCTO_PCELA_CONTR              DATE NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPCELA_PGTO_CONTR           *
      ******************************************************************
       01  CRURB0MK.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-CONTR-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-JURID-ORIGN   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-ORIGN     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NCONTR-CREDT-ORIGN   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-CONTR-ORIGN   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CORCAM-CONTR-RURAL   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PCELA-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-LIQDC         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-GERAC-PCELA     PIC X(1).
      *    *************************************************************
           10 DVCTO-PCELA-CONTR    PIC X(10).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
