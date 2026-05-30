      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PDIDO_FLEXZ_CLI)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0B3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0B3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PDIDO_FLEXZ_CLI TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_FLEXZ_AGPTO             TIMESTAMP NOT NULL,
             CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CFLIAL_CGC                     DECIMAL(5, 0) NOT NULL,
             CCTRL_CPF_CGC                  DECIMAL(2, 0) NOT NULL,
             CCGC_CPF_ST                    CHAR(9) NOT NULL,
             CFLIAL_CGC_ST                  CHAR(4) NOT NULL,
             CCTRL_CPF_CGC_ST               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PDIDO_FLEXZ_CLI             *
      ******************************************************************
       01  GFCTB0B3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-FLEXZ-AGPTO   PIC X(26).
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CGC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-CGC        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-ST          PIC X(9).
      *    *************************************************************
           10 CFLIAL-CGC-ST        PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-CGC-ST     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
