***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CTRL_MOVTO_RECBD)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB033))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB033)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CTRL_MOVTO_RECBD TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CTRATO_MOVTO                   DECIMAL(2, 0) NOT NULL,
             CIDTFD_MOVTO_CRRTT             CHAR(1) NOT NULL,
             QEVNTO_RECBD                   DECIMAL(10, 0) NOT NULL,
             QEVNTO_VALDD                   DECIMAL(10, 0) NOT NULL,
             CSIT_MOVTO_RECBD               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CTRL_MOVTO_RECBD            *
      ******************************************************************
       01  GFCTB033.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTRATO-MOVTO         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-MOVTO-CRRTT   PIC X(1).
      *    *************************************************************
           10 QEVNTO-RECBD         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 QEVNTO-VALDD         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-MOVTO-RECBD     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
