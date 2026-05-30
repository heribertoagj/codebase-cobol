***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.GSTAO_SGMTO_ESTRN)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB074))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB074)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.GSTAO_SGMTO_ESTRN TABLE
           ( CSGMTO_GSTAO                   DECIMAL(3, 0) NOT NULL,
             CSUB_SGMTO_GSTAO               DECIMAL(3, 0) NOT NULL,
             CTARIF_GSTAO                   DECIMAL(5, 0) NOT NULL,
             DMES_FCHTO_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_FCHTO_GSTAO               DECIMAL(4, 0) NOT NULL,
             DMES_SERVC_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_SERVC_GSTAO               DECIMAL(4, 0) NOT NULL,
             CMOTVO_AGPTO_GSTAO             DECIMAL(2, 0) NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_ESTRN_GSTAO             DECIMAL(2, 0) NOT NULL,
             QESTRN_TOT_MES                 DECIMAL(11, 0) NOT NULL,
             VESTRN_TOT_MES                 DECIMAL(16, 2) NOT NULL,
             QESTRN_PCIAL_MES               DECIMAL(11, 0) NOT NULL,
             VESTRN_PCIAL_MES               DECIMAL(16, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.GSTAO_SGMTO_ESTRN           *
      ******************************************************************
       01  GFCTB074.
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
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-ESTRN-GSTAO   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QESTRN-TOT-MES       PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 VESTRN-TOT-MES       PIC S9(14)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QESTRN-PCIAL-MES     PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 VESTRN-PCIAL-MES     PIC S9(14)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
