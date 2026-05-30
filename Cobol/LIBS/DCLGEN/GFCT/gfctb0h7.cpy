***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDEB_PCIAL_TARIF)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0H7))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0H7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDEB_PCIAL_TARIF TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             DDEB_PCIAL_TARIF               DATE NOT NULL,
             VDEB_PCIAL_TARIF               DECIMAL(11, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDEB_PCIAL_TARIF            *
      ******************************************************************
       01  GFCTB0H7.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 DDEB-PCIAL-TARIF     PIC X(10).
      *    *************************************************************
           10 VDEB-PCIAL-TARIF     PIC S9(9)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
