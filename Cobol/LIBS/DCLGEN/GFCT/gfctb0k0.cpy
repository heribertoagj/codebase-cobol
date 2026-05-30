***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCOMPO_EXTRT_PCOTE)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0K0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0K0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCOMPO_EXTRT_PCOTE TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CTPO_COMPO_PCOTE               DECIMAL(1, 0) NOT NULL,
             CCOMPO_PCOTE                   DECIMAL(5, 0) NOT NULL,
             ICOMPO_EXTRT_PCOTE             CHAR(50) NOT NULL,
             CGRP_COMPO_EXTRT               DECIMAL(3, 0) NOT NULL,
             NORD_EXIBC_EXTRT               DECIMAL(3, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCOMPO_EXTRT_PCOTE          *
      ******************************************************************
       01  GFCTB0K0.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-COMPO-PCOTE     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPO-PCOTE         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 ICOMPO-EXTRT-PCOTE   PIC X(50).
      *    *************************************************************
           10 CGRP-COMPO-EXTRT     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NORD-EXIBC-EXTRT     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
