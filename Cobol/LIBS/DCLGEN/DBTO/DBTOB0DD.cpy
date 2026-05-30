      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_ARQ_DVDOR_TARIF)                     *
      *        LIBRARY(AD.DB2.DCLGEN(DBTOB0DD))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DBTOB0DD)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_ARQ_DVDOR_TARIF TABLE
           ( NCTRL_ARQ_DVDOR                DECIMAL(5, 0) NOT NULL,
             CSIST                          CHAR(4) NOT NULL,
             DANO_REFT_ARQ                  DECIMAL(4, 0) NOT NULL,
             DMES_REFT_ARQ                  DECIMAL(2, 0) NOT NULL,
             CCNPJ_REMET_ARQ                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_REMET              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_REMET               DECIMAL(2, 0) NOT NULL,
             HPROCM_ARQ                     TIMESTAMP NOT NULL,
             QREG_RECBD                     DECIMAL(9, 0) NOT NULL,
             CINDCD_GERAC_ARQ               DECIMAL(1, 0) NOT NULL,
             CINDCD_TRNSM_ARQ               CHAR(1) NOT NULL,
             CSIT_PROCM_ARQ                 DECIMAL(1, 0) NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             RMOTVO_MANUT                   CHAR(40)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_ARQ_DVDOR_TARIF       *
      ******************************************************************
       01  DBTOB0DD.
      *    *************************************************************
           10 NCTRL-ARQ-DVDOR      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIST                PIC X(4).
      *    *************************************************************
           10 DANO-REFT-ARQ        PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-REFT-ARQ        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-REMET-ARQ      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-REMET    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-REMET     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HPROCM-ARQ           PIC X(26).
      *    *************************************************************
           10 QREG-RECBD           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-GERAC-ARQ     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TRNSM-ARQ     PIC X(1).
      *    *************************************************************
           10 CSIT-PROCM-ARQ       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 RMOTVO-MANUT         PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
