      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAUTRZ_ENVIO_CLI)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0L5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0L5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAUTRZ_ENVIO_CLI TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CINDCD_AUTRZ_EMIS              DECIMAL(1, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CGC                     DECIMAL(5, 0) NOT NULL,
             CCTRL_CPF_CGC                  DECIMAL(2, 0) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CGC_ST                  CHAR(4) NOT NULL,
             CCTRL_CPF_CGC_ST               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAUTRZ_ENVIO_CLI            *
      ******************************************************************
       01  GFCTB0L5.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AUTRZ-EMIS    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CGC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-CGC        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CGC-ST        PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-CGC-ST     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
