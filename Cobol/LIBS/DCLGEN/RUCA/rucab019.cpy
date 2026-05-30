      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSIT_MANUT_RECOR)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB019))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB019)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSIT_MANUT_RECOR TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             NREG_MSGEM_RECOR               DECIMAL(5, 0) NOT NULL,
             CSIT_MSGEM                     DECIMAL(1, 0) NOT NULL,
             CMSGEM_RECOR                   CHAR(9)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSIT_MANUT_RECOR            *
      ******************************************************************
       01  RUCAB019.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NREG-MSGEM-RECOR     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-MSGEM           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CMSGEM-RECOR         PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
