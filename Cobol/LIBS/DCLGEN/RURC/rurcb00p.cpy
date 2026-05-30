      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPER_ALERT_SUSTBD)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB00P))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB00P)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPER_ALERT_SUSTBD TABLE
           ( NPER_ALERT_SUSTBD              DECIMAL(3, 0) NOT NULL,
             CALERT_SUSTBD_PER              CHAR(1) NOT NULL,
             DINIC_VGCIA_ALERT              DATE NOT NULL,
             DFIM_VGCIA_ALERT               DATE NOT NULL,
             CUSUAR_MANUT                   CHAR(9) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPER_ALERT_SUSTBD           *
      ******************************************************************
       01  RURCB00P.
      *    *************************************************************
           10 NPER-ALERT-SUSTBD    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CALERT-SUSTBD-PER    PIC X(1).
      *    *************************************************************
           10 DINIC-VGCIA-ALERT    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-ALERT     PIC X(10).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
