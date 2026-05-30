      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPARM_ENCRR_CTA_BLOQ)                      *
      *        LIBRARY(AD.DB2.DCLGEN(CTECB028))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CTECB028)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPARM_ENCRR_CTA_BLOQ TABLE
           ( CPARM_ENCRR_BLOQ               DECIMAL(3, 0) NOT NULL,
             CMOTVO_SIT_CTA                 DECIMAL(3, 0),
             CTPO_BLOQ_OPER                 DECIMAL(1,0) NOT NULL,
             CINDCD_TPO_JUDIC               CHAR(1),
             CSIT_PARM_ENCRR                CHAR(1),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HINCL_REG                      TIMESTAMP NOT NULL,
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPARM_ENCRR_CTA_BLOQ        *
      ******************************************************************
       01  CTECB028.
      *    *************************************************************
           10 CPARM-ENCRR-BLOQ     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-SIT-CTA       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BLOQ-OPER       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TPO-JUDIC     PIC X(1).
      *    *************************************************************
           10 CSIT-PARM-ENCRR      PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
