      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_NGTVC_CLI)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_NGTVC_CLI TABLE
           ( CBCO_DEB_MOVTO                 DECIMAL(3, 0) NOT NULL,
             CAG_DEB_MOVTO                  DECIMAL(5, 0) NOT NULL,
             CCTA_DEB_MOVTO                 DECIMAL(13, 0) NOT NULL,
             DCONS_NGTVC_CLI                DATE NOT NULL,
             CCNPJ_CPF_DEB                  DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_DEB                DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF_DEB             DECIMAL(2, 0) NOT NULL,
             CINDCD_RESUL_NGTVC             CHAR(1) NOT NULL,
             RRESUL_CONS_NGTVC              CHAR(60) NOT NULL,
             CROTNA_ORIGE_MOVTO             CHAR(4) NOT NULL,
             DENVIO_MOVTO_TARIF             DATE NOT NULL,
             CNRO_ARQ_MOVTO                 DECIMAL(2, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             CCNPJ_CPF_DEB_ST               CHAR(9) NOT NULL,
             CFLIAL_CNPJ_DEB_ST             CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_DEB_ST          DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_NGTVC_CLI             *
      ******************************************************************
       01  GFCTB0N2.
      *    *************************************************************
           10 CBCO-DEB-MOVTO       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-DEB-MOVTO        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-DEB-MOVTO       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DCONS-NGTVC-CLI      PIC X(10).
      *    *************************************************************
           10 CCNPJ-CPF-DEB        PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-DEB      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-DEB   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-RESUL-NGTVC   PIC X(1).
      *    *************************************************************
           10 RRESUL-CONS-NGTVC    PIC X(60).
      *    *************************************************************
           10 CROTNA-ORIGE-MOVTO   PIC X(4).
      *    *************************************************************
           10 DENVIO-MOVTO-TARIF   PIC X(10).
      *    *************************************************************
           10 CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-DEB-ST        PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-DEB-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-DEB-ST   PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
