      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PDIDO_ESTRN_EMPR)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0A6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0A6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PDIDO_ESTRN_EMPR TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DOCOR_EVNTO                    DATE NOT NULL,
             HIDTFD_ESTRN_AGPTO             TIMESTAMP NOT NULL,
             CAG_EMPR                       DECIMAL(5, 0) NOT NULL,
             CRZ_CTA_EMPR                   DECIMAL(5, 0) NOT NULL,
             CNRO_CTA_DEB                   DECIMAL(13, 0) NOT NULL,
             CLCTO_CTA_EMPR                 DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PDIDO_ESTRN_EMPR            *
      ******************************************************************
       01  GFCTB0A6.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DOCOR-EVNTO          PIC X(10).
      *    *************************************************************
           10 HIDTFD-ESTRN-AGPTO   PIC X(26).
      *    *************************************************************
           10 CAG-EMPR             PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CRZ-CTA-EMPR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTA-DEB         PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-CTA-EMPR       PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
