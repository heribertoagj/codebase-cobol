      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01RAZMES_ORIGREC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RURCV040))                        *
      *        STRUCTURE(RURCV040)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01RAZMES_ORIGREC TABLE
           ( CATVDD_RURAL                   DECIMAL(3, 0) NOT NULL,
             CFNALD_RURAL                   DECIMAL(3, 0) NOT NULL,
             CORIGE_REC                     DECIMAL(3, 0) NOT NULL,
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL                    DECIMAL(3, 0) NOT NULL,
             CGRP_CTBIL_ANTER               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL_ANTER              DECIMAL(3, 0) NOT NULL,
             DINCL                          DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01RAZMES_ORIGREC           *
      ******************************************************************
       01  RURCV040.
      *    *************************************************************
           10 CATVDD-RURAL         PIC S999V USAGE COMP-3.
      *    *************************************************************
           10 CFNALD-RURAL         PIC S999V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC           PIC S999V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CTBIL-ATUAL     PIC S999V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-CTBIL          PIC S999V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CTBIL-ANTER     PIC S999V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-CTBIL-ANTER    PIC S999V USAGE COMP-3.
      *    *************************************************************
           10 DINCL                PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
