      ******************************************************************
      * DCLGEN TABLE(DB2PRD.VLR_EVNTO_CRRTT)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0G4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0G4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.VLR_EVNTO_CRRTT TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CPTCAO_TBELA                   DECIMAL(2, 0) NOT NULL,
             VEVNTO_ORIGN_MOVTO             DECIMAL(11, 2) NOT NULL,
             VEVNTO_CORR_MOVTO              DECIMAL(11, 2) NOT NULL,
             DCONVS_MOEDA_MOVTO             DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.VLR_EVNTO_CRRTT             *
      ******************************************************************
       01  GFCTB0G4.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VEVNTO-ORIGN-MOVTO   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VEVNTO-CORR-MOVTO    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DCONVS-MOEDA-MOVTO   PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
