      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TIMOV_RURAL_EXPDO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB056))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB056)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TIMOV_RURAL_EXPDO TABLE
           ( CCLUB                          DECIMAL(10, 0) NOT NULL,
             CSEQ_IMOV_RURAL                DECIMAL(3, 0) NOT NULL,
             CSEQ_ATULZ_IMOV                DECIMAL(3, 0) NOT NULL,
             CSEQ_PATRM_IMOV                DECIMAL(5, 0),
             RIMOV_RURAL                    CHAR(25) NOT NULL,
             RTPO_EXPLO_IMOV                CHAR(20) NOT NULL,
             PIMOV_CLI                      DECIMAL(5, 2) NOT NULL,
             ELOGDR                         CHAR(40) NOT NULL,
             ENRO_LOGDR                     CHAR(5) NOT NULL,
             ECOMPL_LOGDR                   CHAR(15) NOT NULL,
             EBAIRO_LOGDR                   CHAR(20) NOT NULL,
             CCEP                           DECIMAL(5, 0),
             CCEP_COMPL                     DECIMAL(3, 0),
             MAREA_CULTR_AGRIC              DECIMAL(11, 4) NOT NULL,
             MAREA_PASTA                    DECIMAL(11, 4) NOT NULL,
             MDMAIS_AREA                    DECIMAL(11, 4) NOT NULL,
             RROTRO_ACSSO                   CHAR(150) NOT NULL,
             CORIGE_INFO_IMOV               CHAR(1) NOT NULL,
             CINDCD_MGRAC_REG               CHAR(1) NOT NULL,
             HULT_ATULZ_EFETU               TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             IRGIAO_JUDIC_IMOV              CHAR(40) NOT NULL,
             ICARTR_CONTR_RURAL             VARCHAR(320) NOT NULL,
             CCARTR_IMOV_REG                CHAR(10) NOT NULL,
             CGLEBA_IMOV_HIPOT              CHAR(10) NOT NULL,
             CLOTE_IMOV_HIPOT               CHAR(10) NOT NULL,
             CNIRF_IMOV_RURAL               CHAR(9),
             CCCIR                          CHAR(13),
             CREG_AMBTL_RURAL               CHAR(41),
             PPRESV_AMBTL_PPRIE             DECIMAL(5, 2),
             COUTGA_AGUA_IMOV               CHAR(30),
             DVALDD_OUTGA_AGUA              DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TIMOV_RURAL_EXPDO           *
      ******************************************************************
       01  RUECB056.
      *    *************************************************************
           10 CCLUB                PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-IMOV-RURAL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-ATULZ-IMOV      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-PATRM-IMOV      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RIMOV-RURAL          PIC X(25).
      *    *************************************************************
           10 RTPO-EXPLO-IMOV      PIC X(20).
      *    *************************************************************
           10 PIMOV-CLI            PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 ELOGDR               PIC X(40).
      *    *************************************************************
           10 ENRO-LOGDR           PIC X(5).
      *    *************************************************************
           10 ECOMPL-LOGDR         PIC X(15).
      *    *************************************************************
           10 EBAIRO-LOGDR         PIC X(20).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 MAREA-CULTR-AGRIC    PIC S9(7)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 MAREA-PASTA          PIC S9(7)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 MDMAIS-AREA          PIC S9(7)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 RROTRO-ACSSO         PIC X(150).
      *    *************************************************************
           10 CORIGE-INFO-IMOV     PIC X(1).
      *    *************************************************************
           10 CINDCD-MGRAC-REG     PIC X(1).
      *    *************************************************************
           10 HULT-ATULZ-EFETU     PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 IRGIAO-JUDIC-IMOV    PIC X(40).
      *    *************************************************************
           10 ICARTR-CONTR-RURAL.
              49 ICARTR-CONTR-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 ICARTR-CONTR-RURAL-TEXT
                 PIC X(320).
      *    *************************************************************
           10 CCARTR-IMOV-REG      PIC X(10).
      *    *************************************************************
           10 CGLEBA-IMOV-HIPOT    PIC X(10).
      *    *************************************************************
           10 CLOTE-IMOV-HIPOT     PIC X(10).
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************
