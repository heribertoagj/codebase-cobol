***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TGRP_CART_SIST)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0I6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0I6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TGRP_CART_SIST TABLE
           ( CGRP_CART_SIST                 DECIMAL(5, 0) NOT NULL,
             RGRP_CART_SIST                 CHAR(30) NOT NULL,
             CIDTFD_SIT_GRP                 DECIMAL(2, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             HEFTVC_REG_SIST                TIMESTAMP NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TGRP_CART_SIST              *
      ******************************************************************
       01  GFCTB0I6.
      *    *************************************************************
           10 CGRP-CART-SIST       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RGRP-CART-SIST       PIC X(30).
      *    *************************************************************
           10 CIDTFD-SIT-GRP       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 HEFTVC-REG-SIST      PIC X(26).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
