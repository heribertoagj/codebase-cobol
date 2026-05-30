***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.FLEXZ_NRO_CONTR)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB059))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB059)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.FLEXZ_NRO_CONTR TABLE
           ( CBCO_CTA_CLI                   DECIMAL(3, 0) NOT NULL,
             CDEPDC_CTA_CLI                 DECIMAL(5, 0) NOT NULL,
             CNRO_CTA_CLI                   DECIMAL(13, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CROTNA_ORIGE_CONTR             CHAR(4) NOT NULL,
             CIDTFD_CONTR_FLEXZ             CHAR(15) NOT NULL,
             HIDTFD_FLEXZ_CONTR             TIMESTAMP NOT NULL,
             VNEGOC_FLEXZ_CONTR             DECIMAL(11, 2) NOT NULL,
             PDESC_FLEXZ_CONTR              DECIMAL(7, 4) NOT NULL,
             QDIA_FLEXZ_CONTR               DECIMAL(2, 0) NOT NULL,
             DUTILZ_FLEXZ_CONTR             DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.FLEXZ_NRO_CONTR             *
      ******************************************************************
       01  GFCTB059.
      *    *************************************************************
           10 CBCO-CTA-CLI         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-CTA-CLI       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTA-CLI         PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CROTNA-ORIGE-CONTR   PIC X(4).
      *    *************************************************************
           10 CIDTFD-CONTR-FLEXZ   PIC X(15).
      *    *************************************************************
           10 HIDTFD-FLEXZ-CONTR   PIC X(26).
      *    *************************************************************
           10 VNEGOC-FLEXZ-CONTR   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDESC-FLEXZ-CONTR    PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 QDIA-FLEXZ-CONTR     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DUTILZ-FLEXZ-CONTR   PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
