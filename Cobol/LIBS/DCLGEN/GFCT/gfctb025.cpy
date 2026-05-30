***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CANCT_MOVTO_ORIGE)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB025))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB025)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CANCT_MOVTO_ORIGE TABLE
           ( CROTNA_ORIGE_CANCT             CHAR(4) NOT NULL,
             DENVIO_MOVTO_CANCT             DATE NOT NULL,
             CNRO_ARQ_CANCT                 DECIMAL(2, 0) NOT NULL,
             CSEQ_CANCT                     DECIMAL(11, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             RJUSTF_MANUT_SERVC             VARCHAR(255) NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_JUSTF                   DECIMAL(2, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CANCT_MOVTO_ORIGE           *
      ******************************************************************
       01  GFCTB025.
      *    *************************************************************
           10 CROTNA-ORIGE-CANCT   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-CANCT   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-CANCT       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-CANCT           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-MANUT-SERVC.
              49 RJUSTF-MANUT-SERVC-LEN  PIC S9(4) USAGE COMP.
              49 RJUSTF-MANUT-SERVC-TEXT  PIC X(255).
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-JUSTF         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
