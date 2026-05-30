      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PDIDO_ESTRN_GRP)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0A7))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0A7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PDIDO_ESTRN_GRP TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DOCOR_EVNTO                    DATE NOT NULL,
             HIDTFD_ESTRN_AGPTO             TIMESTAMP NOT NULL,
             CSIT_SOLTC_ESTRN               DECIMAL(2, 0) NOT NULL,
             CSIT_PROCM_ESTRN               DECIMAL(1, 0) NOT NULL,
             CINDCD_AGPTO_TOT               CHAR(1) NOT NULL,
             CDEPDC_SOLTC_ESTRN             DECIMAL(5, 0) NOT NULL,
             CFUNC_SOLTC_ESTRN              DECIMAL(9, 0) NOT NULL,
             HSOLTC_ESTRN                   TIMESTAMP NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_JUSTF                   DECIMAL(2, 0) NOT NULL,
             RJUSTF_SOLTC_ESTRN             VARCHAR(255) NOT NULL,
             CDEPDC_PAREC_ESTRN             DECIMAL(5, 0) NOT NULL,
             CFUNC_PAREC_ESTRN              DECIMAL(9, 0) NOT NULL,
             HPAREC_ESTRN                   TIMESTAMP NOT NULL,
             RJUSTF_PAREC_ESTRN             VARCHAR(255) NOT NULL,
             QESTRN_AGPTO                   DECIMAL(11, 0) NOT NULL,
             VESTRN_SOLTD                   DECIMAL(13, 2) NOT NULL,
             QCANCT_AGPTO                   DECIMAL(11, 0) NOT NULL,
             VCANCT_SOLTD                   DECIMAL(13, 2) NOT NULL,
             DEFETV_CREDT_ESTRN             DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PDIDO_ESTRN_GRP             *
      ******************************************************************
       01  GFCTB0A7.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DOCOR-EVNTO          PIC X(10).
      *    *************************************************************
           10 HIDTFD-ESTRN-AGPTO   PIC X(26).
      *    *************************************************************
           10 CSIT-SOLTC-ESTRN     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PROCM-ESTRN     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AGPTO-TOT     PIC X(1).
      *    *************************************************************
           10 CDEPDC-SOLTC-ESTRN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-SOLTC-ESTRN    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HSOLTC-ESTRN         PIC X(26).
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-JUSTF         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-SOLTC-ESTRN.
              49 RJUSTF-SOLTC-ESTRN-LEN  PIC S9(4) USAGE COMP.
              49 RJUSTF-SOLTC-ESTRN-TEXT  PIC X(255).
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
           10 QESTRN-AGPTO         PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 VESTRN-SOLTD         PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QCANCT-AGPTO         PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 VCANCT-SOLTD         PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-CREDT-ESTRN   PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 22      *
      ******************************************************************
