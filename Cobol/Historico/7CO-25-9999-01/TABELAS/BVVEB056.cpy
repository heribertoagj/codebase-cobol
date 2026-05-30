      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOPER_GARNT_AGNDA_CIPAG)                   *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB056))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB056)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOPER_GARNT_AGNDA_CIPAG TABLE
           ( CPROT_SIST_ORIGE               CHAR(50) NOT NULL,
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CCNPJ_TTLAR_RCBVL              DECIMAL(9, 0) NOT NULL,
             CFLIAL_TTLAR_RCBVL             DECIMAL(4, 0) NOT NULL,
             CCTRL_CNPJ_RCBVL               DECIMAL(2, 0) NOT NULL,
             COPER_ENVID_CIPAG              CHAR(20) NOT NULL,
             DINIC_OPER_GARNT               DATE NOT NULL,
             DFIM_OPER_GARNT                DATE NOT NULL,
             VSOLTD_OPER_GARNT              DECIMAL(17, 2) NOT NULL,
             VEFTVC_OPER_GARNT              DECIMAL(17, 2),
             COPER_EFETU_CIPAG              CHAR(20),
             COPER_CANCD_CIPAG              CHAR(20),
             CTPO_MANUT_CIPAG               DECIMAL(1, 0) NOT NULL,
             CSIT_OPER_CIPAG                CHAR(1) NOT NULL,
             IMOTVO_REJEI_CIPAG             CHAR(60),
             DMOVTO                         DATE NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL,
             CCNPJ_TTLAR_RCBVL_ST           CHAR(9) NOT NULL,
             CFLIAL_TTLAR_RCBVL_ST          CHAR(4) NOT NULL,
             CCTRL_CNPJ_RCBVL_ST            DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOPER_GARNT_AGNDA_CIPAG     *
      ******************************************************************
       01  BVVEB056.
      *    *************************************************************
           10 CPROT-SIST-ORIGE     PIC X(50).
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-TTLAR-RCBVL    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-TTLAR-RCBVL   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-RCBVL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      *    *************************************************************
           10 DINIC-OPER-GARNT     PIC X(10).
      *    *************************************************************
           10 DFIM-OPER-GARNT      PIC X(10).
      *    *************************************************************
           10 VSOLTD-OPER-GARNT    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VEFTVC-OPER-GARNT    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 COPER-EFETU-CIPAG    PIC X(20).
      *    *************************************************************
           10 COPER-CANCD-CIPAG    PIC X(20).
      *    *************************************************************
           10 CTPO-MANUT-CIPAG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-OPER-CIPAG      PIC X(1).
      *    *************************************************************
           10 IMOTVO-REJEI-CIPAG   PIC X(60).
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-TTLAR-RCBVL-ST
              PIC X(9).
      *    *************************************************************
           10 CFLIAL-TTLAR-RCBVL-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-RCBVL-ST  PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
