      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRTCP_PSSOA_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0MS))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0MS)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRTCP_PSSOA_RURAL TABLE
           ( CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             CTPO_PRTCP_PSSOA               DECIMAL(3, 0) NOT NULL,
             CPSSOA                         DECIMAL(10, 0) NOT NULL,
             CSIT_PRTCP_CONTR               DECIMAL(2, 0) NOT NULL,
             CTPO_BNEFC_RURAL               DECIMAL(2, 0),
             CQUALF_BNEFC_RURAL             DECIMAL(2, 0),
             DINIC_PRTCP_CONTR              DATE NOT NULL,
             DFIM_PSSOA_CONTR               DATE,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRTCP_PSSOA_RURAL          *
      ******************************************************************
       01  CRURB0MS.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PRTCP-PSSOA     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA               PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PRTCP-CONTR     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BNEFC-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CQUALF-BNEFC-RURAL   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-PRTCP-CONTR    PIC X(10).
      *    *************************************************************
           10 DFIM-PSSOA-CONTR     PIC X(10).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
