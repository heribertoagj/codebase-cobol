***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CLI_VP)                                    *
      *        LIBRARY(AD.DB2.DCLGEN(SCMPB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SCMPB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CLI_VP TABLE
           ( CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CCTRL_CGC_CPF                  DECIMAL(2, 0) NOT NULL,
             DINCL                          DATE NOT NULL,
             DALT                           DATE NOT NULL,
             CIDTFD_ACAO_GSTOR              CHAR(1) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CCGC_CPF_ST                    CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CLI_VP                      *
      ******************************************************************
       01  SCMPB000.
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CGC-CPF        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DINCL                PIC X(10).
      *    *************************************************************
           10 DALT                 PIC X(10).
      *    *************************************************************
           10 CIDTFD-ACAO-GSTOR    PIC X(1).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-ST          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
