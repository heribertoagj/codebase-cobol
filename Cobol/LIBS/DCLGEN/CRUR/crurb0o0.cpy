      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCLAUS_CONTR_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0O0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0O0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCLAUS_CONTR_RURAL TABLE
           ( CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             CPSSOA_JURID                   DECIMAL(10, 0) NOT NULL,
             CPRODT_SERVC_OPER              DECIMAL(8, 0) NOT NULL,
             NCLAUS_CONTR_PRODT             DECIMAL(5, 0) NOT NULL,
             NVRSAO_CLAUS_CONTR             DECIMAL(5, 0) NOT NULL,
             CDOCTO_CREDT_RURAL             DECIMAL(3, 0) NOT NULL,
             NORD_CLAUS_RURAL               DECIMAL(3, 0) NOT NULL,
             RJUSTF_CLAUS_CONTR             VARCHAR(256),
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCLAUS_CONTR_RURAL          *
      ******************************************************************
       01  CRURB0O0.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-JURID         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-SERVC-OPER    PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 NCLAUS-CONTR-PRODT   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NVRSAO-CLAUS-CONTR   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-CREDT-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NORD-CLAUS-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-CLAUS-CONTR.
              49 RJUSTF-CLAUS-CONTR-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-CLAUS-CONTR-TEXT
                 PIC X(256).
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
