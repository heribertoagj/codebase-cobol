      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRESP_UND_RURAL)                           *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0MV))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0MV)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRESP_UND_RURAL TABLE
           ( CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             CPSSOA_JURID                   DECIMAL(10, 0) NOT NULL,
             NSEQ_UND_ORGNZ                 DECIMAL(8, 0) NOT NULL,
             CTPO_RESP_UND                  DECIMAL(3, 0) NOT NULL,
             DINIC_RESP_UND                 DATE NOT NULL,
             DFNAL_RESP_UND                 DATE NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRESP_UND_RURAL             *
      ******************************************************************
       01  CRURB0MV.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-JURID         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-UND-ORGNZ       PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-RESP-UND        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-RESP-UND       PIC X(10).
      *    *************************************************************
           10 DFNAL-RESP-UND       PIC X(10).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
