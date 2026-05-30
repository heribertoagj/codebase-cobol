      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLIN_MSGEM_REMSS)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0UA))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0UA)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLIN_MSGEM_REMSS TABLE
           ( CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             NENVIO_INFO_RECOR              DECIMAL(9, 0) NOT NULL,
             NLIN_MSGEM_REMSS               DECIMAL(3, 0) NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0) NOT NULL,
             CTRNSM_INFO_BACEN              CHAR(23) NOT NULL,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             RLIN_MSGEM_ENVID               VARCHAR(32500) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLIN_MSGEM_REMSS            *
      ******************************************************************
       01  CRURB0UA.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NENVIO-INFO-RECOR    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NLIN-MSGEM-REMSS     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTRNSM-INFO-BACEN    PIC X(23).
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
      *    *************************************************************
           10 RLIN-MSGEM-ENVID.
              49 RLIN-MSGEM-ENVID-LEN
                 PIC S9(4) USAGE COMP.
              49 RLIN-MSGEM-ENVID-TEXT
                 PIC X(32500).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
