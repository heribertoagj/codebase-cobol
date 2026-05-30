      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ESTBL_CONVN_BACC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB014))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB014)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ESTBL_CONVN_BACC TABLE
           ( CPREFX_ESTBL_BACC              DECIMAL(4, 0) NOT NULL,
             CBCO_VISA                      DECIMAL(3, 0) NOT NULL,
             CESTBL_CONVN_BACC              DECIMAL(6, 0) NOT NULL,
             VLIM_OPER_VDA                  DECIMAL(9, 2) NOT NULL,
             CRECEB_BOLET_HTCRD             DECIMAL(1, 0) NOT NULL,
             CEXCEC_DSBTO_ORPAG             DECIMAL(1, 0) NOT NULL,
             CDEVLC_FIS_ORPAG               DECIMAL(1, 0) NOT NULL,
             CMEIO_COMUN_DEVLC              DECIMAL(1, 0) NOT NULL,
             CIND_MOEDA                     DECIMAL(1, 0) NOT NULL,
             IESTBL_CONVN_BACC              CHAR(35) NOT NULL,
             ELOGDR                         CHAR(40) NOT NULL,
             ENRO_IMOV                      CHAR(5) NOT NULL,
             ECOMPL_LOGDR                   CHAR(10) NOT NULL,
             EBAIRO_LOGDR                   CHAR(20) NOT NULL,
             IMUN                           CHAR(30) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCEP_COMPL                     CHAR(3) NOT NULL,
             PCOMIS_VDA_CATAO               DECIMAL(5, 2) NOT NULL,
             TCRED_ESTBL_BACC               DECIMAL(2, 0) NOT NULL,
             DDIA_CREDT                     DECIMAL(2, 0) NOT NULL,
             CTPO_PCMTO                     DECIMAL(1, 0) NOT NULL,
             DDSCRD                         DATE,
             CTPO_ATVDD_COML                DECIMAL(5, 0) NOT NULL,
             CLOC_ARQ_ORPAG                 DECIMAL(1, 0) NOT NULL,
             CCGC_CPF_CMAST                 DECIMAL(16, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             IFANTS                         CHAR(40) NOT NULL,
             QMAQNA_CATAO_CREDT             DECIMAL(5, 0) NOT NULL,
             DADSAO_VISA                    DATE NOT NULL,
             CCGC_CPF_CMAST_ST              CHAR(15) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ESTBL_CONVN_BACC            *
      ******************************************************************
       01  BACCB014.
      *    *************************************************************
           10 CPREFX-ESTBL-BACC    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-VISA            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CESTBL-CONVN-BACC    PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VLIM-OPER-VDA        PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CRECEB-BOLET-HTCRD   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CEXCEC-DSBTO-ORPAG   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CDEVLC-FIS-ORPAG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CMEIO-COMUN-DEVLC    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIND-MOEDA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IESTBL-CONVN-BACC    PIC X(35).
      *    *************************************************************
           10 ELOGDR               PIC X(40).
      *    *************************************************************
           10 ENRO-IMOV            PIC X(5).
      *    *************************************************************
           10 ECOMPL-LOGDR         PIC X(10).
      *    *************************************************************
           10 EBAIRO-LOGDR         PIC X(20).
      *    *************************************************************
           10 IMUN                 PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC X(3).
      *    *************************************************************
           10 PCOMIS-VDA-CATAO     PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 TCRED-ESTBL-BACC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DDIA-CREDT           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PCMTO           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DDSCRD               PIC X(10).
      *    *************************************************************
           10 CTPO-ATVDD-COML      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CLOC-ARQ-ORPAG       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-CMAST       PIC S9(16)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 IFANTS               PIC X(40).
      *    *************************************************************
           10 QMAQNA-CATAO-CREDT   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DADSAO-VISA          PIC X(10).
      *    *************************************************************
           10 CCGC-CPF-CMAST-ST    PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************
