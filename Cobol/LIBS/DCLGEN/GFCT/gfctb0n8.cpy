***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_PROCM_MASSV)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_PROCM_MASSV TABLE
           ( HINCL_REG                      TIMESTAMP NOT NULL,
             CTPO_OPER_MASSV                DECIMAL(1, 0) NOT NULL,
             DPROCM_AGNDA                   DATE NOT NULL,
             CSIT_PROCM_MASSV               DECIMAL(1, 0) NOT NULL,
             ROBS_PROCM_MASSV               CHAR(80) NOT NULL,
             QREG_LIDO_MASSV                DECIMAL(9, 0) NOT NULL,
             QREG_ACTAC_MASSV               DECIMAL(9, 0) NOT NULL,
             QREG_REJTD_MASSV               DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_PROCM_MASSV           *
      ******************************************************************
       01  GFCTB0N8.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CTPO-OPER-MASSV      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DPROCM-AGNDA         PIC X(10).
      *    *************************************************************
           10 CSIT-PROCM-MASSV     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 ROBS-PROCM-MASSV     PIC X(80).
      *    *************************************************************
           10 QREG-LIDO-MASSV      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QREG-ACTAC-MASSV     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QREG-REJTD-MASSV     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
