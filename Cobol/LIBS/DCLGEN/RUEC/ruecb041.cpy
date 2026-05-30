      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MATR_CDULA_RURAL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB041))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB041)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MATR_CDULA_RURAL TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCDULA                         DECIMAL(9, 0) NOT NULL,
             CSEQ_MATR_RURAL                DECIMAL(3, 0) NOT NULL,
             CINDCD_AREA_PRINC              DECIMAL(1, 0) NOT NULL,
             CMATR_IMOV_RURAL               CHAR(15) NOT NULL,
             IIMOV_RURAL                    CHAR(25) NOT NULL,
             MAREA_IMOV_RURAL               DECIMAL(9, 2) NOT NULL,
             CSEQ_IMOV_RURAL                DECIMAL(3, 0) NOT NULL,
             CMUN_RURAL                     DECIMAL(9, 0) NOT NULL,
             CID_GLEBA                      CHAR(19),
             MLATTD_PTO_CNTRL               DECIMAL(9, 6),
             MLOGTD_PTO_CNTRL               DECIMAL(9, 6),
             CINDCD_AREA_CIRC               DECIMAL(1, 0),
             CNIRF_IMOV_RURAL               CHAR(9),
             CCCIR                          CHAR(13),
             CREG_AMBTL_RURAL               CHAR(41),
             PPRESV_AMBTL_PPRIE             DECIMAL(5, 2),
             COUTGA_AGUA_IMOV               CHAR(30),
             DVALDD_OUTGA_AGUA              DATE,
             CTPO_CORND_GEOGR               DECIMAL(1, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MATR_CDULA_RURAL            *
      ******************************************************************
       01  RUECB041.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MATR-RURAL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AREA-PRINC    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CMATR-IMOV-RURAL     PIC X(15).
      *    *************************************************************
           10 IIMOV-RURAL          PIC X(25).
      *    *************************************************************
           10 MAREA-IMOV-RURAL     PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-IMOV-RURAL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMUN-RURAL           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CID-GLEBA            PIC X(19).
      *    *************************************************************
           10 MLATTD-PTO-CNTRL     PIC S9(3)V9(6) USAGE COMP-3.
      *    *************************************************************
           10 MLOGTD-PTO-CNTRL     PIC S9(3)V9(6) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AREA-CIRC     PIC S9(1)V USAGE COMP-3.
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
           10 CTPO-CORND-GEOGR     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
