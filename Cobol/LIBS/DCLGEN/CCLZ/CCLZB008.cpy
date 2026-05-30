      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSIT_PROCM_RECTA_FEDRL)                    *
      *        LIBRARY(AD.DB2.DCLGEN(CCLZB008))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CCLZB008)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSIT_PROCM_RECTA_FEDRL TABLE
           ( CSIT_PROCM_RECTA               DECIMAL(2, 0) NOT NULL,
             RSIT_PROCM_RECTA               CHAR(35) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSIT_PROCM_RECTA_FEDRL      *
      ******************************************************************
       01  CCLZB008.
      *    *************************************************************
           10 CSIT-PROCM-RECTA     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RSIT-PROCM-RECTA     PIC X(35).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
