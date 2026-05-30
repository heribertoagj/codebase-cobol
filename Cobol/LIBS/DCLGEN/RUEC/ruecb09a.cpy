      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TENVIO_RECOR_TEMPR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB09A))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB09A)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TENVIO_RECOR_TEMPR TABLE
           ( CCONTR_CREDT_RURAL             DECIMAL(9, 0) NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             NREG_MSGEM_RECOR               DECIMAL(5, 0) NOT NULL,
             CSIT_MSGEM                     DECIMAL(1, 0),
             HINCL                          TIMESTAMP,
             WCONTD_MSGEM_RURAL             VARCHAR(32500) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TENVIO_RECOR_TEMPR          *
      ******************************************************************
       01  RUECB09A.
      *    *************************************************************
           10 CCONTR-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NREG-MSGEM-RECOR     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-MSGEM           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 WCONTD-MSGEM-RURAL.
              49 WCONTD-MSGEM-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 WCONTD-MSGEM-RURAL-TEXT
                 PIC X(32500).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
