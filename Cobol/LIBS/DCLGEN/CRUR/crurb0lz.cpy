      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLCTO_PCELA_RURAL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0LZ))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0LZ)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLCTO_PCELA_RURAL TABLE
           ( CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             NPCELA_CONTR_RURAL             DECIMAL(3, 0) NOT NULL,
             CLCTO_CONTR_RURAL              DECIMAL(5, 0) NOT NULL,
             VLCTO_PCELA_EMPTO              DECIMAL(15, 2) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLCTO_PCELA_RURAL           *
      ******************************************************************
       01  CRURB0LZ.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-CONTR-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-CONTR-RURAL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VLCTO-PCELA-EMPTO    PIC S9(13)V9(2) USAGE COMP-3.
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
