      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPSSOA_DESC_COML)                          *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0D4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0D4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPSSOA_DESC_COML TABLE
           ( NPSSOA_DESC_COML               DECIMAL(9, 0) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CDOCTO_ID                      CHAR(15) NOT NULL,
             IPSSOA_DESC_COML               CHAR(60) NOT NULL,
             CCLUB                          DECIMAL(10, 0) NOT NULL,
             DINCL_REG                      DATE NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CINDCD_TPO_PSSOA               CHAR(1) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPSSOA_DESC_COML            *
      ******************************************************************
       01  DCOMB0D4.
      *    *************************************************************
           10 NPSSOA-DESC-COML     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-ID            PIC X(15).
      *    *************************************************************
           10 IPSSOA-DESC-COML     PIC X(60).
      *    *************************************************************
           10 CCLUB                PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 DINCL-REG            PIC X(10).
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CINDCD-TPO-PSSOA     PIC X(1).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
