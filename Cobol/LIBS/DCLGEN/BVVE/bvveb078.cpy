      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOG_PROCM_MNTRC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB078))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB078)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOG_PROCM_MNTRC TABLE
           ( DPROCM_RESUL_MNTRC             DATE NOT NULL,
             CINDCD_RESUL_PROCM             DECIMAL(1, 0) NOT NULL,
             QMSGEM_ENVID                   DECIMAL(10, 0) NOT NULL,
             QMSGEM_RECBD                   DECIMAL(10, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOG_PROCM_MNTRC            *
      ******************************************************************
       01  BVVEB078.
      *    *************************************************************
           10 DPROCM-RESUL-MNTRC   PIC X(10).
      *    *************************************************************
           10 CINDCD-RESUL-PROCM   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QMSGEM-ENVID         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 QMSGEM-RECBD         PIC S9(10)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
