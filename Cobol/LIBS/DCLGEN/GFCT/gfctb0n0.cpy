***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TESTRN_COBRD_EXTRT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TESTRN_COBRD_EXTRT TABLE
           ( CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CSEQ_ESTRN_EVNTO               DECIMAL(2, 0) NOT NULL,
             CORIGE_PDIDO_ESTRN             DECIMAL(1, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DEFETV_CREDT_ESTRN             DATE NOT NULL,
             VRTEIO_VLR_SOLTD               DECIMAL(11, 2) NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_JUSTF                   DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TESTRN_COBRD_EXTRT          *
      ******************************************************************
       01  GFCTB0N0.
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-ESTRN-EVNTO     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-PDIDO-ESTRN   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-CREDT-ESTRN   PIC X(10).
      *    *************************************************************
           10 VRTEIO-VLR-SOLTD     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-JUSTF         PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
