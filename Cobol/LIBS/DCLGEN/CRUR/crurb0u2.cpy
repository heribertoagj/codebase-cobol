      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_ENVIO_INFO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0U2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0U2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_ENVIO_INFO TABLE
           ( CPSSOA_JURID_CONTR             DECIMAL(10, 0) NOT NULL,
             CTPO_CONTR_NEGOC               DECIMAL(3, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             NENVIO_INFO_RECOR              DECIMAL(9, 0) NOT NULL,
             CMSGEM_SIST_BACEN              CHAR(9) NOT NULL,
             CRQUIS_FINCR_BACEN             CHAR(20),
             CSIT_MSGEM_BACEN               DECIMAL(2, 0) NOT NULL,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_ENVIO_INFO            *
      ******************************************************************
       01  CRURB0U2.
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NENVIO-INFO-RECOR    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CMSGEM-SIST-BACEN    PIC X(9).
      *    *************************************************************
           10 CRQUIS-FINCR-BACEN   PIC X(20).
      *    *************************************************************
           10 CSIT-MSGEM-BACEN     PIC S9(2)V USAGE COMP-3.
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
