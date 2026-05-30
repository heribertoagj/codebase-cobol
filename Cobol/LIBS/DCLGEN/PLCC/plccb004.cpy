      ******************************************************************
      * DCLGEN TABLE(DB2PRD.RAZAO_CONTABIL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(PLCCB004))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(PLCCB004)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.RAZAO_CONTABIL TABLE
           ( CEMPR                          DECIMAL(5, 0) NOT NULL,
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL                    DECIMAL(3, 0) NOT NULL,
             IRZ_CTBIL                      CHAR(40) NOT NULL,
             CSIT_ANLSE_ORCAM               CHAR(1)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.RAZAO_CONTABIL              *
      ******************************************************************
       01  PLCCB004.
      *    *************************************************************
           10 CEMPR                PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IRZ-CTBIL            PIC X(40).
      *    *************************************************************
           10 CSIT-ANLSE-ORCAM     PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
