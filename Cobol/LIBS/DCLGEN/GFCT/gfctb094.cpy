***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MOVTO_ROTNA_ORIGE)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB094))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB094)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MOVTO_ROTNA_ORIGE TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CTARIF_MOVTO                   DECIMAL(5, 0) NOT NULL,
             CBCO_ORIGE_MOVTO               DECIMAL(3, 0) NOT NULL,
             CAG_ORIGE_MOVTO                DECIMAL(5, 0) NOT NULL,
             CPAB_ORIGE_MOVTO               DECIMAL(5, 0) NOT NULL,
             CINDCD_CANCT_MOVTO             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MOVTO_ROTNA_ORIGE           *
      ******************************************************************
       01  GFCTB094.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CTARIF-MOVTO         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-ORIGE-MOVTO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-ORIGE-MOVTO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPAB-ORIGE-MOVTO     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CANCT-MOVTO   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
