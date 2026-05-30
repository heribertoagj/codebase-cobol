      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01TPO_OPER_ORCAM)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RUIAV005))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUIAV005)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01TPO_OPER_ORCAM TABLE
           ( CTPO_OPER_ORCAM                DECIMAL(3, 0) NOT NULL,
             RTPO_OPER_ORCAM                CHAR(40) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01TPO_OPER_ORCAM           *
      ******************************************************************
       01  RUIAV005.
      *    *************************************************************
           10 CTPO-OPER-ORCAM      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-OPER-ORCAM      PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
