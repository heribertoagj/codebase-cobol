***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.GSTAO_SGMTO_DESC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB073))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB073)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.GSTAO_SGMTO_DESC TABLE
           ( CSGMTO_GSTAO                   DECIMAL(3, 0) NOT NULL,
             CSUB_SGMTO_GSTAO               DECIMAL(3, 0) NOT NULL,
             CTARIF_GSTAO                   DECIMAL(5, 0) NOT NULL,
             DMES_FCHTO_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_FCHTO_GSTAO               DECIMAL(4, 0) NOT NULL,
             DMES_SERVC_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_SERVC_GSTAO               DECIMAL(4, 0) NOT NULL,
             CMOTVO_AGPTO_GSTAO             DECIMAL(2, 0) NOT NULL,
             CTPO_DESC                      DECIMAL(1, 0) NOT NULL,
             CIDTFD_DESC                    DECIMAL(5, 0) NOT NULL,
             CPARM_DESC                     DECIMAL(1, 0) NOT NULL,
             QTARIF_DESC_MES                DECIMAL(12, 0) NOT NULL,
             VTARIF_DESC_MES                DECIMAL(16, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.GSTAO_SGMTO_DESC            *
      ******************************************************************
       01  GFCTB073.
      *    *************************************************************
           10 CSGMTO-GSTAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSUB-SGMTO-GSTAO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTARIF-GSTAO         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-FCHTO-GSTAO     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-FCHTO-GSTAO     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-SERVC-GSTAO     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-SERVC-GSTAO     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-AGPTO-GSTAO   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-DESC            PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-DESC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPARM-DESC           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-DESC-MES      PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DESC-MES      PIC S9(16)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
