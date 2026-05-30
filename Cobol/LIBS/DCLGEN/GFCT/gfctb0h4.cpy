      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THIST_SIT_EVNTO)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0H4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0H4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THIST_SIT_EVNTO TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             HTROCA_SIT_EVNTO               TIMESTAMP NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0) NOT NULL,
             CSIT_EVNTO_RECBD               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THIST_SIT_EVNTO             *
      ******************************************************************
       01  GFCTB0H4.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 HTROCA-SIT-EVNTO     PIC X(26).
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-EVNTO-RECBD     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
