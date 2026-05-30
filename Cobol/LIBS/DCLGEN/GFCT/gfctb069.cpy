***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.GSTAO_CLI_SERVC)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB069))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB069)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.GSTAO_CLI_SERVC TABLE
           ( CCGC_CPF_ST                    CHAR(9) NOT NULL,
             CFLIAL_CGC_ST                  CHAR(4) NOT NULL,
             CCTRL_CPF_CGC_ST               DECIMAL(2, 0) NOT NULL,
             CTARIF_GSTAO                   DECIMAL(5, 0) NOT NULL,
             DMES_FCHTO_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_FCHTO_GSTAO               DECIMAL(4, 0) NOT NULL,
             DMES_SERVC_GSTAO               DECIMAL(2, 0) NOT NULL,
             DANO_SERVC_GSTAO               DECIMAL(4, 0) NOT NULL,
             CMOTVO_AGPTO_GSTAO             DECIMAL(2, 0) NOT NULL,
             QTARIF_TOT_MES                 DECIMAL(12, 0) NOT NULL,
             VTARIF_BRUTO_MES               DECIMAL(16, 2) NOT NULL,
             QTARIF_DEB_TOT                 DECIMAL(11, 0) NOT NULL,
             VTARIF_DEB_TOT_MES             DECIMAL(16, 2) NOT NULL,
             QTARIF_DEB_PCIAL               DECIMAL(11, 0) NOT NULL,
             VTARIF_DEB_PCIAL               DECIMAL(11, 2) NOT NULL,
             QTARIF_PERDA_MES               DECIMAL(11, 0) NOT NULL,
             VTARIF_PERDA_MES               DECIMAL(16, 2) NOT NULL,
             QTARIF_PEND_MES                DECIMAL(11, 0) NOT NULL,
             VTARIF_PEND_MES                DECIMAL(16, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.GSTAO_CLI_SERVC             *
      ******************************************************************
       01  GFCTB069.
      *    *************************************************************
           10 CCGC-CPF-ST          PIC X(9).
      *    *************************************************************
           10 CFLIAL-CGC-ST        PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-CGC-ST     PIC S9(2)V USAGE COMP-3.
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
           10 QTARIF-TOT-MES       PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-BRUTO-MES     PIC S9(14)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-DEB-TOT       PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-TOT-MES   PIC S9(14)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-DEB-PCIAL     PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-DEB-PCIAL     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-PERDA-MES     PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-PERDA-MES     PIC S9(14)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTARIF-PEND-MES      PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-PEND-MES      PIC S9(14)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
