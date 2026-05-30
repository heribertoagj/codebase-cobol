      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_CTA_CATAO)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB042))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB042)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_CTA_CATAO TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CIDTFD_PRODT_CATAO             DECIMAL(5, 0) NOT NULL,
             HRECEB                         TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             VDISPN_ANTEC_ANTER             DECIMAL(15, 2) NOT NULL,
             VDISPN_ANTEC_ATUAL             DECIMAL(15, 2) NOT NULL,
             PSDO_ANTEC_CREDT               DECIMAL(5, 2) NOT NULL,
             VMOVTC_MAX_ANTEC               DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_CTA_CATAO              *
      ******************************************************************
       01  BACCB042.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-CATAO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HRECEB               PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 VDISPN-ANTEC-ANTER   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VDISPN-ANTEC-ATUAL   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PSDO-ANTEC-CREDT     PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMOVTC-MAX-ANTEC     PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
