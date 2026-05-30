----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.GSTAO_MIDIA_ESTRN)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB071))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB071)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.GSTAO_MIDIA_ESTRN TABLE
           ( CMIDIA_GSTAO                   DECIMAL(3, 0) NOT NULL,
             CTARIF_GSTAO                   DECIMAL(5, 0) NOT NULL,
             DMES_FCHTO_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_FCHTO_GSTAO               DECIMAL(4, 0) NOT NULL,
             DMES_SERVC_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_SERVC_GSTAO               DECIMAL(4, 0) NOT NULL,
             CIDTFD_CRRTT_GSTAO             CHAR(1) NOT NULL,
             CMOTVO_AGPTO_GSTAO             DECIMAL(2, 0) NOT NULL,
             CTPO_CTA_GSTAO                 DECIMAL(1, 0) NOT NULL,
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_ESTRN_GSTAO             DECIMAL(2, 0) NOT NULL,
             QESTRN_TOT_MES                 DECIMAL(11, 0) NOT NULL,
             VESTRN_TOT_MES                 DECIMAL(16, 2) NOT NULL,
             QESTRN_PCIAL_MES               DECIMAL(11, 0) NOT NULL,
             VESTRN_PCIAL_MES               DECIMAL(16, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.GSTAO_MIDIA_ESTRN           *
      ******************************************************************
       01  GFCTB071.
      *    *************************************************************
           10 CMIDIA-GSTAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTARIF-GSTAO         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-FCHTO-GSTAO     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-FCHTO-GSTAO     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-SERVC-GSTAO     PIC S9(2)V USAGE COMP-3.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *    *************************************************************
           10 DANO-SERVC-GSTAO     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-CRRTT-GSTAO   PIC X(1).
      *    *************************************************************
           10 CMOTVO-AGPTO-GSTAO   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CTA-GSTAO       PIC S9(1)V USAGE COMP-3.
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
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
