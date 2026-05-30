      * DCLGEN TABLE(DB2PRD.TSIT_TRNSF_SDO_CREDR_TED)                  *
      *        LIBRARY(AD.DB2.DCLGEN(CTECB030))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      ******************************************************************
      *        STRUCTURE(CTECB030)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSIT_TRNSF_SDO_CREDR_TED TABLE
           ( HINCL_REG                      TIMESTAMP NOT NULL,
             NSOLTC_CTA_BCRIA               DECIMAL(17, 0) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             CRETOR_PRINC                   DECIMAL(5, 0),
             CRETOR_SECUN                   DECIMAL(4, 0),
             RSIT_PROCS_TRNSF               CHAR(80) NOT NULL,
             CDEVLC_TRNSF                   DECIMAL(2, 0),
             RDEVLC_TRNSF                   CHAR(30),
             VLCTO_TRNSF_DEVLC              DECIMAL(17, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSIT_TRNSF_SDO_CREDR_TED    *
      ******************************************************************
       01  CTECB030.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 NSOLTC-CTA-BCRIA     PIC S9(17)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 CRETOR-PRINC         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CRETOR-SECUN         PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 RSIT-PROCS-TRNSF     PIC X(80).
      *    *************************************************************
           10 CDEVLC-TRNSF         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RDEVLC-TRNSF         PIC X(30).
      *    *************************************************************
           10 VLCTO-TRNSF-DEVLC    PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
