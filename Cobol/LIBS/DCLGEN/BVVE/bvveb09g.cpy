      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_OPER_EFETU_GARNT)                    *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09G))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09G)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_OPER_EFETU_GARNT TABLE
           ( COPER_ENVID_CIPAG              CHAR(20) NOT NULL,
             HENVIO_MSGEM_CIPAG             TIMESTAMP NOT NULL,
             COPER_GARNT_REG                CHAR(20) NOT NULL,
             CPROT_SIST_ORIGE               CHAR(50) NOT NULL,
             COPER_EFETU_CIPAG              CHAR(20),
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             CTPO_MANUT_GARNT               DECIMAL(1, 0) NOT NULL,
             CORIGE_OPER_GARNT              DECIMAL(1, 0) NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL,
             CERRO_MSGEM_SIST               CHAR(8) NOT NULL,
             CSIT_RETOR_GARNT               CHAR(1) NOT NULL,
             HRETOR_MSGEM_CIPAG             TIMESTAMP,
             CUSUAR_RESP                    CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_OPER_EFETU_GARNT      *
      ******************************************************************
       01  BVVEB09G.
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      *    *************************************************************
           10 HENVIO-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 COPER-GARNT-REG      PIC X(20).
      *    *************************************************************
           10 CPROT-SIST-ORIGE     PIC X(50).
      *    *************************************************************
           10 COPER-EFETU-CIPAG    PIC X(20).
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-MANUT-GARNT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-OPER-GARNT    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CERRO-MSGEM-SIST     PIC X(8).
      *    *************************************************************
           10 CSIT-RETOR-GARNT     PIC X(1).
      *    *************************************************************
           10 HRETOR-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
