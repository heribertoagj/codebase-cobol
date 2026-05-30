      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01GRP_REJEI_ORPAG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV019))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV019)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01GRP_REJEI_ORPAG TABLE
           ( CGRP_REJEI_BACC                DECIMAL(1, 0) NOT NULL,
             CPRIOR_TRATO_REJEI             DECIMAL(2, 0) NOT NULL,
             RGRP_REJEI_ORPAG               CHAR(40) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01GRP_REJEI_ORPAG          *
      ******************************************************************
       01  BACCV019.
      *    *************************************************************
           10 CGRP-REJEI-BACC      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPRIOR-TRATO-REJEI   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RGRP-REJEI-ORPAG     PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
