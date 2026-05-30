***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_TARIF_PRINC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB087))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB087)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_TARIF_PRINC TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_MANUT_SERVC             TIMESTAMP NOT NULL,
             CSEQ_MANUT_SERVC               DECIMAL(2, 0) NOT NULL,
             CFUNC_MANUT_SERVC              DECIMAL(9, 0) NOT NULL,
             CFUNC_PAREC_SERVC              DECIMAL(9, 0) NOT NULL,
             CBCO_CCREN                     DECIMAL(3, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             CIDTFD_SNAL_LCTO               DECIMAL(1, 0) NOT NULL,
             CTPO_SERVC_TARIF               DECIMAL(1, 0) NOT NULL,
             RSERVC_TARIF                   VARCHAR(255) NOT NULL,
             RSERVC_TARIF_REDZD             CHAR(100) NOT NULL,
             CPSSOA_SERVC_TARIF             CHAR(1) NOT NULL,
             CTPO_CTA_DEB_TARIF             DECIMAL(1, 0) NOT NULL,
             CTPO_DEB_COBR                  DECIMAL(1, 0) NOT NULL,
             RUND_COBR_SERVC                CHAR(20) NOT NULL,
             CEMPR_GSTAO_SERVC              DECIMAL(5, 0) NOT NULL,
             CDEPDC_GSTAO_SERVC             DECIMAL(5, 0) NOT NULL,
             CLCTO_SERVC                    DECIMAL(5, 0) NOT NULL,
             CDEPDC_MANUT_SERVC             DECIMAL(5, 0) NOT NULL,
             CTPO_MANUT_SERVC               DECIMAL(1, 0) NOT NULL,
             CTERM_MANUT_SERVC              CHAR(8) NOT NULL,
             HMANUT_SERVC                   TIMESTAMP NOT NULL,
             CDEPDC_PAREC_SERVC             DECIMAL(5, 0) NOT NULL,
             CTERM_PAREC_SERVC              CHAR(8) NOT NULL,
             HPAREC_SERVC                   TIMESTAMP NOT NULL,
             RJUSTF_MANUT_SERVC             VARCHAR(255) NOT NULL,
             CSIT_LIBRC_TARIF               DECIMAL(2, 0) NOT NULL,
             CPAREC_GSTAO_SERVC             DECIMAL(1, 0) NOT NULL,
             RJUSTF_PAREC_SERVC             VARCHAR(255) NOT NULL,
             CTPO_LCTO_COBR                 DECIMAL(2, 0) NOT NULL,
             CTPO_LCTO_ESTRN                DECIMAL(2, 0) NOT NULL,
             CCART_DEB_TARIF                DECIMAL(5, 0) NOT NULL,
             CINDCD_DEB_PCIAL               CHAR(1) NOT NULL,
             CINDCD_PCIAL_ULT               CHAR(1) NOT NULL,
             CINDCD_EVNTO_PCELD             CHAR(1) NOT NULL,
             CINDCD_QTD_REG                 CHAR(1) NOT NULL,
             CLCTO_COBR_PJ                  DECIMAL(2, 0) NOT NULL,
             CLCTO_ESTRN_PJ                 DECIMAL(2, 0) NOT NULL,
             CINDCD_NRO_DOCTO               CHAR(1) NOT NULL,
             CINDCD_CONS_IMPED              CHAR(1) NOT NULL,
             CLCTO_ESTRN_ANTER              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_TARIF_PRINC            *
      ******************************************************************
       01  GFCTB087.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-MANUT-SERVC   PIC X(26).
      *    *************************************************************
           10 CSEQ-MANUT-SERVC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-SERVC    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-PAREC-SERVC    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-CCREN           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SNAL-LCTO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-SERVC-TARIF     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RSERVC-TARIF.
              49 RSERVC-TARIF-LEN
                 PIC S9(4) USAGE COMP.
              49 RSERVC-TARIF-TEXT
                 PIC X(255).
      *    *************************************************************
           10 RSERVC-TARIF-REDZD   PIC X(100).
      *    *************************************************************
           10 CPSSOA-SERVC-TARIF   PIC X(1).
      *    *************************************************************
           10 CTPO-CTA-DEB-TARIF   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-DEB-COBR        PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RUND-COBR-SERVC      PIC X(20).
      *    *************************************************************
           10 CEMPR-GSTAO-SERVC    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-GSTAO-SERVC   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-SERVC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-MANUT-SERVC   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-MANUT-SERVC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-MANUT-SERVC    PIC X(8).
      *    *************************************************************
           10 HMANUT-SERVC         PIC X(26).
      *    *************************************************************
           10 CDEPDC-PAREC-SERVC   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-PAREC-SERVC    PIC X(8).
      *    *************************************************************
           10 HPAREC-SERVC         PIC X(26).
      *    *************************************************************
           10 RJUSTF-MANUT-SERVC.
              49 RJUSTF-MANUT-SERVC-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-MANUT-SERVC-TEXT
                 PIC X(255).
      *    *************************************************************
           10 CSIT-LIBRC-TARIF     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CPAREC-GSTAO-SERVC   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-PAREC-SERVC.
              49 RJUSTF-PAREC-SERVC-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-PAREC-SERVC-TEXT
                 PIC X(255).
      *    *************************************************************
           10 CTPO-LCTO-COBR       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-LCTO-ESTRN      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCART-DEB-TARIF      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-DEB-PCIAL     PIC X(1).
      *    *************************************************************
           10 CINDCD-PCIAL-ULT     PIC X(1).
      *    *************************************************************
           10 CINDCD-EVNTO-PCELD   PIC X(1).
      *    *************************************************************
           10 CINDCD-QTD-REG       PIC X(1).
      *    *************************************************************
           10 CLCTO-COBR-PJ        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-ESTRN-PJ       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-NRO-DOCTO     PIC X(1).
      *    *************************************************************
           10 CINDCD-CONS-IMPED    PIC X(1).
      *    *************************************************************
           10 CLCTO-ESTRN-ANTER    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 41      *
      ******************************************************************
