      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CTRL_ROTNA_GFCT)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CTRL_ROTNA_GFCT TABLE
           ( CPROG                          CHAR(8) NOT NULL,
             CJOB                           CHAR(8) NOT NULL,
             QREG_COMIT                     DECIMAL(5, 0) NOT NULL,
             CSIT_RESTT                     CHAR(1) NOT NULL,
             CUSUAR_SENHA                   CHAR(7),
             HATULZ                         TIMESTAMP,
             WCHAVE_RESTT                   VARCHAR(250)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CTRL_ROTNA_GFCT             *
      ******************************************************************
       01  GFCTB000.
      *    *************************************************************
           10 CPROG                PIC X(8).
      *    *************************************************************
           10 CJOB                 PIC X(8).
      *    *************************************************************
           10 QREG-COMIT           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-RESTT           PIC X(1).
      *    *************************************************************
           10 CUSUAR-SENHA         PIC X(7).
      *    *************************************************************
           10 HATULZ               PIC X(26).
      *    *************************************************************
           10 WCHAVE-RESTT.
              49 WCHAVE-RESTT-LEN  PIC S9(4) USAGE COMP.
              49 WCHAVE-RESTT-TEXT  PIC X(250).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
