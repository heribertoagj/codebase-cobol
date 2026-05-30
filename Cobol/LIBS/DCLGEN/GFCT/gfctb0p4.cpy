***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOG_EVNTO_ACSSO_COBR_TARIF)               *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0P4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0P4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOG_EVNTO_ACSSO_COBR_TARIF TABLE
           ( HEVNTO_LOG                     TIMESTAMP NOT NULL,
             CUSUAR_LOG                     CHAR(9) NOT NULL,
             CIP_LOG                        CHAR(20)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOG_EVNTO_ACSSO_COBR_TARIF *
      ******************************************************************
       01  GFCTB0P4.
      *    *************************************************************
           10 HEVNTO-LOG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-LOG           PIC X(9).
      *    *************************************************************
           10 CIP-LOG              PIC X(20).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
