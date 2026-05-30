***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TASSOC_GRP_CART)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0I5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0I5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TASSOC_GRP_CART TABLE
           ( CGRP_CART_SIST                 DECIMAL(5, 0) NOT NULL,
             CCART_ANLSE_GERC               DECIMAL(5, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TASSOC_GRP_CART             *
      ******************************************************************
       01  GFCTB0I5.
      *    *************************************************************
           10 CGRP-CART-SIST       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCART-ANLSE-GERC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
