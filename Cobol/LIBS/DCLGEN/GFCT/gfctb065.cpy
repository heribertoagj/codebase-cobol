***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.GSTAO_AG_ESTRN)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB065))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB065)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.GSTAO_AG_ESTRN TABLE
           ( CAG_GSTAO                      DECIMAL(5, 0) NOT NULL,
             CPAB_AG_GSTAO                  DECIMAL(5, 0) NOT NULL,
             CTARIF_GSTAO                   DECIMAL(5, 0) NOT NULL,
             DMES_FCHTO_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_FCHTO_GSTAO               DECIMAL(4, 0) NOT NULL,
             DMES_SERVC_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_SERVC_GSTAO               DECIMAL(4, 0) NOT NULL,
             CIDTFD_CRRTT_GSTAO             CHAR(1) NOT NULL,
             CMOTVO_AGPTO_GSTAO             DECIMAL(2, 0) NOT NULL,
             CTPO_CTA_GSTAO                 DECIMAL(1, 0) NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_ESTRN_GSTAO             DECIMAL(2, 0) NOT NULL,
             QESTRN_TOT_MES                 DECIMAL(11, 0) NOT NULL,
             VESTRN_TOT_MES                 DECIMAL(16, 2) NOT NULL,
             QESTRN_PCIAL_MES               DECIMAL(11, 0) NOT NULL,
             VESTRN_PCIAL_MES               DECIMAL(16, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.GSTAO_AG_ESTRN              *
      ******************************************************************
       01  GFCTB065.
      *    *************************************************************
           10 CAG-GSTAO            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPAB-AG-GSTAO        PIC S9(5)V USAGE COMP-3.
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
