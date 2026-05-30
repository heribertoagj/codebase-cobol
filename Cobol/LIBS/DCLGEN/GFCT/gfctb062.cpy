***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.GRP_COD_LCTO)                              *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB062))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB062)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.GRP_COD_LCTO TABLE
           ( CGRP_LCTO                      DECIMAL(5, 0) NOT NULL,
             RGRP_LCTO                      CHAR(30) NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.GRP_COD_LCTO                *
      ******************************************************************
       01  GFCTB062.
      *    *************************************************************
           10 CGRP-LCTO            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RGRP-LCTO            PIC X(30).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
