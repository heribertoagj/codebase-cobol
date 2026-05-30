      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TARIF_CTA_AGNDA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0F1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0F1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TARIF_CTA_AGNDA TABLE
           ( CBCO_CTA_CLI                   DECIMAL(3, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_FLEXZ_CTA               TIMESTAMP NOT NULL,
             DINIC_VGCIA_AGNDA              DATE NOT NULL,
             DDIA_AGNDA                     DECIMAL(2, 0) NOT NULL,
             DFIM_VGCIA_AGNDA               DATE NOT NULL,
             CDEPDC_CTA_CLI                 DECIMAL(5, 0) NOT NULL,
             CNRO_CTA_CLI                   DECIMAL(13, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TARIF_CTA_AGNDA             *
      ******************************************************************
       01  GFCTB0F1.
      *    *************************************************************
           10 CBCO-CTA-CLI         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-FLEXZ-CTA     PIC X(26).
      *    *************************************************************
           10 DINIC-VGCIA-AGNDA    PIC X(10).
      *    *************************************************************
           10 DDIA-AGNDA           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-VGCIA-AGNDA     PIC X(10).
      *    *************************************************************
           10 CDEPDC-CTA-CLI       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTA-CLI         PIC S9(13)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
