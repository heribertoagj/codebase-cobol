***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.FLEXZ_GRP_EMPR)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB052))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB052)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.FLEXZ_GRP_EMPR TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             HIDTFD_FLEXZ_AGPTO             TIMESTAMP NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CAG_EMPR                       DECIMAL(5, 0) NOT NULL,
             CRZ_CTA_EMPR                   DECIMAL(5, 0) NOT NULL,
             CNRO_CTA_DEB                   DECIMAL(13, 0) NOT NULL,
             CLCTO_EMPR                     DECIMAL(3, 0) NOT NULL,
             PPGTO_RTEIO_EMPR               DECIMAL(7, 4) NOT NULL,
             RSEGDA_LIN_EXTRT               CHAR(32) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.FLEXZ_GRP_EMPR              *
      ******************************************************************
       01  GFCTB052.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-FLEXZ-AGPTO   PIC X(26).
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-EMPR             PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CRZ-CTA-EMPR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTA-DEB         PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-EMPR           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 PPGTO-RTEIO-EMPR     PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 RSEGDA-LIN-EXTRT     PIC X(32).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
