      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PDIDO_ESTRN_CTA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0A5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0A5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PDIDO_ESTRN_CTA TABLE
           ( CBCO_CTA_CLI                   DECIMAL(3, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_ESTRN_CTA               TIMESTAMP NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_JUSTF                   DECIMAL(2, 0) NOT NULL,
             VESTRN_SOLTD                   DECIMAL(13, 2) NOT NULL,
             DRECEB_SOLTC_ESTRN             DATE NOT NULL,
             DEFETV_DEB_MOVTO               DATE NOT NULL,
             CTPO_SOLTC_ESTRN               DECIMAL(1, 0) NOT NULL,
             CSIT_SOLTC_ESTRN               DECIMAL(2, 0) NOT NULL,
             CSIT_PROCM_ESTRN               DECIMAL(1, 0) NOT NULL,
             CDEPDC_SOLTC_ESTRN             DECIMAL(5, 0) NOT NULL,
             CFUNC_SOLTC_ESTRN              DECIMAL(9, 0) NOT NULL,
             HSOLTC_ESTRN                   TIMESTAMP NOT NULL,
             RJUSTF_SOLTC_ESTRN             VARCHAR(255) NOT NULL,
             CIDTFD_DEPDC                   CHAR(2) NOT NULL,
             CDEPDC_PAREC_ESTRN             DECIMAL(5, 0) NOT NULL,
             CFUNC_PAREC_ESTRN              DECIMAL(9, 0) NOT NULL,
             HPAREC_ESTRN                   TIMESTAMP NOT NULL,
             RJUSTF_PAREC_ESTRN             VARCHAR(255) NOT NULL,
             QEVNTO_AGRUP                   DECIMAL(6, 0) NOT NULL,
             DEFETV_CREDT_ESTRN             DATE NOT NULL,
             DOCOR_EVNTO                    DATE NOT NULL,
             VEVNTO_SEM_REG                 DECIMAL(11, 2) NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PDIDO_ESTRN_CTA             *
      ******************************************************************
       01  GFCTB0A5.
      *    *************************************************************
           10 CBCO-CTA-CLI         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-ESTRN-CTA     PIC X(26).
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-JUSTF         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VESTRN-SOLTD         PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DRECEB-SOLTC-ESTRN   PIC X(10).
      *    *************************************************************
           10 DEFETV-DEB-MOVTO     PIC X(10).
      *    *************************************************************
           10 CTPO-SOLTC-ESTRN     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-SOLTC-ESTRN     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PROCM-ESTRN     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-SOLTC-ESTRN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-SOLTC-ESTRN    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HSOLTC-ESTRN         PIC X(26).
      *    *************************************************************
           10 RJUSTF-SOLTC-ESTRN.
              49 RJUSTF-SOLTC-ESTRN-LEN  PIC S9(4) USAGE COMP.
              49 RJUSTF-SOLTC-ESTRN-TEXT  PIC X(255).
      *    *************************************************************
           10 CIDTFD-DEPDC         PIC X(2).
      *    *************************************************************
           10 CDEPDC-PAREC-ESTRN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-PAREC-ESTRN    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HPAREC-ESTRN         PIC X(26).
      *    *************************************************************
           10 RJUSTF-PAREC-ESTRN.
              49 RJUSTF-PAREC-ESTRN-LEN  PIC S9(4) USAGE COMP.
              49 RJUSTF-PAREC-ESTRN-TEXT  PIC X(255).
      *    *************************************************************
           10 QEVNTO-AGRUP         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-CREDT-ESTRN   PIC X(10).
      *    *************************************************************
           10 DOCOR-EVNTO          PIC X(10).
      *    *************************************************************
           10 VEVNTO-SEM-REG       PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 27      *
      ******************************************************************
