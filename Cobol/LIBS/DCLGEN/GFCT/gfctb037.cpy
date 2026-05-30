***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.DEB_RTEIO_EMPR)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB037))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB037)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.DEB_RTEIO_EMPR TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CROTNA_EMPR_RTEIO              CHAR(4) NOT NULL,
             DMOVTO_EMPR_RTEIO              DATE NOT NULL,
             CNRO_EMPR_RTEIO                DECIMAL(2, 0) NOT NULL,
             CSEQ_EMPR_RTEIO                DECIMAL(11, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.DEB_RTEIO_EMPR              *
      ******************************************************************
       01  GFCTB037.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CROTNA-EMPR-RTEIO    PIC X(4).
      *    *************************************************************
           10 DMOVTO-EMPR-RTEIO    PIC X(10).
      *    *************************************************************
           10 CNRO-EMPR-RTEIO      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-EMPR-RTEIO      PIC S9(11)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
