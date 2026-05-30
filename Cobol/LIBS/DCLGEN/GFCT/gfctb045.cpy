***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.EVNTO_CTBIL_CONSL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB045))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB045)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.EVNTO_CTBIL_CONSL TABLE
           ( CTPO_DEB_CONSL                 DECIMAL(2, 0) NOT NULL,
             CTPO_LCTO_CTBIL                DECIMAL(2, 0) NOT NULL,
             CSEQ_EVNTO_CTBIL               DECIMAL(2, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CPRODT_CTBIL_ELEMT             CHAR(4) NOT NULL,
             COPER_CTBIL                    CHAR(7) NOT NULL,
             CINDCD_DEPDC_CTBIL             DECIMAL(1, 0) NOT NULL,
             PATRIC_CTBIL                   DECIMAL(7, 4) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.EVNTO_CTBIL_CONSL           *
      ******************************************************************
       01  GFCTB045.
      *    *************************************************************
           10 CTPO-DEB-CONSL       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-LCTO-CTBIL      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-EVNTO-CTBIL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-CTBIL-ELEMT   PIC X(4).
      *    *************************************************************
           10 COPER-CTBIL          PIC X(7).
      *    *************************************************************
           10 CINDCD-DEPDC-CTBIL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 PATRIC-CTBIL         PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
