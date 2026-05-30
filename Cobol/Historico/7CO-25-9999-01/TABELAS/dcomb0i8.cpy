      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THIST_PSSOA_OPER)                          *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0I8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0I8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THIST_PSSOA_OPER TABLE
           ( DANO_OPER_DESC                 DECIMAL(4, 0) NOT NULL,
             NSEQ_OPER_DESC                 DECIMAL(9, 0) NOT NULL,
             NPCELA_DESC_COML               DECIMAL(5, 0) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             NPSSOA_DESC_COML               DECIMAL(9, 0) NOT NULL,
             NRELAC_PSSOA_DESC              DECIMAL(9, 0) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CDOCTO_ID                      CHAR(15) NOT NULL,
             IPSSOA_DESC_COML               CHAR(60) NOT NULL,
             CCLUB                          DECIMAL(10, 0) NOT NULL,
             CINDCD_PSSOA_DESC              CHAR(2) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(5, 0) NOT NULL,
             IPSSOA_CONJG                   CHAR(40) NOT NULL,
             CCPF_CONJG                     DECIMAL(9, 0) NOT NULL,
             CCTRL_CPF_CONJG                DECIMAL(2, 0) NOT NULL,
             CDOCTO_ID_CONJG                CHAR(15),
             CINDCD_CONJG_AUTRZ             CHAR(1) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             ELOGDR                         CHAR(40) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL,
             DINCL_REG                      DATE NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THIST_PSSOA_OPER            *
      ******************************************************************
       01  DCOMB0I8.
      *    *************************************************************
           10 DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 NPSSOA-DESC-COML     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NRELAC-PSSOA-DESC    PIC S9(9)V USAGE COMP-3.
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
           10 CINDCD-PSSOA-DESC    PIC X(2).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 IPSSOA-CONJG         PIC X(40).
      *    *************************************************************
           10 CCPF-CONJG           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-CONJG      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-ID-CONJG      PIC X(15).
      *    *************************************************************
           10 CINDCD-CONJG-AUTRZ   PIC X(1).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 ELOGDR               PIC X(40).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 DINCL-REG            PIC X(10).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************
