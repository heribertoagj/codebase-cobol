      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TIMOV_CREDT_RURAL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0B1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0B1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TIMOV_CREDT_RURAL TABLE
           ( CIMOV_CREDT_RURAL              DECIMAL(10, 0) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             CESPEC_IMOV                    DECIMAL(3, 0) NOT NULL,
             CTPO_BEM_IMOV                  DECIMAL(3, 0) NOT NULL,
             CMUN                           DECIMAL(10, 0) NOT NULL,
             CMUN_CARTR_IMOV                DECIMAL(10, 0),
             CIMOV_CREDT_AGDOR              DECIMAL(10, 0),
             CUND_MEDD                      DECIMAL(3, 0) NOT NULL,
             CTPO_BEM_RURAL                 DECIMAL(5, 0),
             CMODLD_DISTR_RURAL             DECIMAL(5, 0) NOT NULL,
             CDOCTO_TITLO_AQSTV             DECIMAL(5, 0),
             IIMOV_CREDT_RURAL              CHAR(60) NOT NULL,
             ELOGDR_IMOV_RURAL              CHAR(70),
             ENRO_IMOV_RURAL                CHAR(7),
             ECOMPL_IMOV_RURAL              CHAR(30),
             EBAIRO_IMOV_RURAL              CHAR(40),
             EDSTRI_IMOV_RURAL              CHAR(60),
             RROTRO_ACSSO_IMOV              VARCHAR(300) NOT NULL,
             CTPO_AVALC_IMOV                CHAR(1) NOT NULL,
             CCPF_CNPJ_AVADR                DECIMAL(9, 0),
             CFLIAL_CNPJ_AVADR              DECIMAL(4, 0),
             CCTRL_CPF_CNPJ                 DECIMAL(2, 0),
             VTERRA_SEM_BENFE               DECIMAL(15, 2),
             VIMOV_CREDT_RURAL              DECIMAL(15, 2),
             DAVALC_IMOV_RURAL              DATE,
             CINSCR_INCRA                   CHAR(15),
             MAREA_IMOV_RURAL               DECIMAL(11, 4),
             CMATR_IMOV_RURAL               CHAR(20),
             CLIVRO_REG_CREDT               CHAR(5),
             CFL_REG_CREDT                  CHAR(5),
             CAVERB_IMOV_RURAL              DECIMAL(5, 0),
             CIMOV_RURAL_IMPNH              CHAR(1) NOT NULL,
             CGLEBA_IMOV                    CHAR(10),
             CLOTE_IMOV_RURAL               CHAR(10),
             DAQUIS_IMOV_RURAL              DATE,
             DREG_CREDT_RURAL               DATE,
             ICARTR_CREDT_RURAL             VARCHAR(320),
             ICOMAR_JURID                   CHAR(60),
             CCARTR_CREDT_RURAL             DECIMAL(5, 0),
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CPSSOA                         DECIMAL(10, 0),
             CPSSOA_JURID                   DECIMAL(10, 0),
             CSEQ_PATRM_IMOV                DECIMAL(5, 0),
             CIMOV_VINC_RURAL               DECIMAL(10, 0),
             CNIRF_IMOV_RURAL               CHAR(9),
             CCCIR                          CHAR(13),
             CREG_AMBTL_RURAL               CHAR(41),
             PPRESV_AMBTL_PPRIE             DECIMAL(5, 2),
             COUTGA_AGUA_IMOV               CHAR(30),
             DVALDD_OUTGA_AGUA              DATE,
             CCPF_CNPJ_AVADR_ST             CHAR(9),
             CFLIAL_CNPJ_AVADR_ST           CHAR(4),
             CCTRL_CPF_CNPJ_ST              DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TIMOV_CREDT_RURAL           *
      ******************************************************************
       01  CRURB0B1.
      *    *************************************************************
           10 CIMOV-CREDT-RURAL    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CESPEC-IMOV          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BEM-IMOV        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMUN                 PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CMUN-CARTR-IMOV      PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CIMOV-CREDT-AGDOR    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BEM-RURAL       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CMODLD-DISTR-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-TITLO-AQSTV   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 IIMOV-CREDT-RURAL    PIC X(60).
      *    *************************************************************
           10 ELOGDR-IMOV-RURAL    PIC X(70).
      *    *************************************************************
           10 ENRO-IMOV-RURAL      PIC X(7).
      *    *************************************************************
           10 ECOMPL-IMOV-RURAL    PIC X(30).
      *    *************************************************************
           10 EBAIRO-IMOV-RURAL    PIC X(40).
      *    *************************************************************
           10 EDSTRI-IMOV-RURAL    PIC X(60).
      *    *************************************************************
           10 RROTRO-ACSSO-IMOV.
              49 RROTRO-ACSSO-IMOV-LEN
                 PIC S9(4) USAGE COMP-5.
              49 RROTRO-ACSSO-IMOV-TEXT
                 PIC X(300).
      *    *************************************************************
           10 CTPO-AVALC-IMOV      PIC X(1).
      *    *************************************************************
           10 CCPF-CNPJ-AVADR      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-AVADR    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-CNPJ       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VTERRA-SEM-BENFE     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VIMOV-CREDT-RURAL    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DAVALC-IMOV-RURAL    PIC X(10).
      *    *************************************************************
           10 CINSCR-INCRA         PIC X(15).
      *    *************************************************************
           10 MAREA-IMOV-RURAL     PIC S9(7)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 CMATR-IMOV-RURAL     PIC X(20).
      *    *************************************************************
           10 CLIVRO-REG-CREDT     PIC X(5).
      *    *************************************************************
           10 CFL-REG-CREDT        PIC X(5).
      *    *************************************************************
           10 CAVERB-IMOV-RURAL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIMOV-RURAL-IMPNH    PIC X(1).
      *    *************************************************************
           10 CGLEBA-IMOV          PIC X(10).
      *    *************************************************************
           10 CLOTE-IMOV-RURAL     PIC X(10).
      *    *************************************************************
           10 DAQUIS-IMOV-RURAL    PIC X(10).
      *    *************************************************************
           10 DREG-CREDT-RURAL     PIC X(10).
      *    *************************************************************
           10 ICARTR-CREDT-RURAL.
              49 ICARTR-CREDT-RURAL-LEN
                 PIC S9(4) USAGE COMP-5.
              49 ICARTR-CREDT-RURAL-TEXT
                 PIC X(320).
      *    *************************************************************
           10 ICOMAR-JURID         PIC X(60).
      *    *************************************************************
           10 CCARTR-CREDT-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CPSSOA               PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-JURID         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-PATRM-IMOV      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIMOV-VINC-RURAL     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CNIRF-IMOV-RURAL     PIC X(9).
      *    *************************************************************
           10 CCCIR                PIC X(13).
      *    *************************************************************
           10 CREG-AMBTL-RURAL     PIC X(41).
      *    *************************************************************
           10 PPRESV-AMBTL-PPRIE   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 COUTGA-AGUA-IMOV     PIC X(30).
      *    *************************************************************
           10 DVALDD-OUTGA-AGUA    PIC X(10).
      *    *************************************************************
           10 CCPF-CNPJ-AVADR-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-AVADR-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-CNPJ-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 58      *
      ******************************************************************
