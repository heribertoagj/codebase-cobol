***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCREDT_OPER_FONE)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0M6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0M6)                                     *
      *        APOST                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCREDT_OPER_FONE TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DOCOR_EVNTO                    DATE NOT NULL,
             NSEQ_SOLTC_CREDT               DECIMAL(3, 0) NOT NULL,
             CSIT_CREDT_OPER                DECIMAL(1, 0) NOT NULL,
             CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             COPER_FONE_MOVEL               DECIMAL(9, 0) NOT NULL,
             CDDD_FONE_MOVEL                DECIMAL(3, 0) NOT NULL,
             VTARIF_DEB_MOVTO               DECIMAL(11, 2) NOT NULL,
             DEFETV_DEB_MOVTO               DATE NOT NULL,
             DENVIO_CREDT_FONE              DATE NOT NULL,
             DRETOR_CREDT_FONE              DATE NOT NULL,
             RMSGEM_CREDT_FONE              CHAR(85) NOT NULL,
             NLIN_TFONI                     DECIMAL(11, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCREDT_OPER_FONE            *
      ******************************************************************
       01  GFCTB0M6.
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
           10 DOCOR-EVNTO          PIC X(10).
           10 NSEQ-SOLTC-CREDT     PIC S9(3)V USAGE COMP-3.
           10 CSIT-CREDT-OPER      PIC S9(1)V USAGE COMP-3.
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
           10 DENVIO-MOVTO-TARIF   PIC X(10).
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
           10 COPER-FONE-MOVEL     PIC S9(9)V USAGE COMP-3.
           10 CDDD-FONE-MOVEL      PIC S9(3)V USAGE COMP-3.
           10 VTARIF-DEB-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
           10 DEFETV-DEB-MOVTO     PIC X(10).
           10 DENVIO-CREDT-FONE    PIC X(10).
           10 DRETOR-CREDT-FONE    PIC X(10).
           10 RMSGEM-CREDT-FONE    PIC X(85).
           10 NLIN-TFONI           PIC S9(11)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
