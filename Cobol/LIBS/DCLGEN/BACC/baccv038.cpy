      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01TRANS_PROCEDA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV038))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV038)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01TRANS_PROCEDA TABLE
           ( CSCTRO_PROCEDA                 DECIMAL(5, 0) NOT NULL,
             DDIA_PROCM                     DECIMAL(2, 0) NOT NULL,
             CLOTE_PROCEDA                  DECIMAL(7, 0) NOT NULL,
             CVDDOR_PROCEDA                 DECIMAL(13, 0) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             CORPAG_PROCEDA                 DECIMAL(7, 0) NOT NULL,
             DCOMPR                         DATE NOT NULL,
             VTRANS_AUTRZ_VISA              DECIMAL(11, 2) NOT NULL,
             CTPO_OBTEN_AUTRZ               CHAR(1) NOT NULL,
             CTPO_FINAN_VISA                DECIMAL(1, 0) NOT NULL,
             CFONTE_AUTRZ_VISA              CHAR(1) NOT NULL,
             CCARAC_AUTRZ_VISA              CHAR(1) NOT NULL,
             CID_AUTRZ_VISA                 DECIMAL(15, 0) NOT NULL,
             CVALDC_AUTRZ_VISA              CHAR(4) NOT NULL,
             CCOMPL_AUTRZ_VISA              CHAR(1) NOT NULL,
             CTERM_PROCEDA                  DECIMAL(8, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01TRANS_PROCEDA            *
      ******************************************************************
       01  BACCV038.
      *    *************************************************************
           10 CSCTRO-PROCEDA       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DDIA-PROCM           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CLOTE-PROCEDA        PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CVDDOR-PROCEDA       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CORPAG-PROCEDA       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DCOMPR               PIC X(10).
      *    *************************************************************
           10 VTRANS-AUTRZ-VISA    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OBTEN-AUTRZ     PIC X(1).
      *    *************************************************************
           10 CTPO-FINAN-VISA      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFONTE-AUTRZ-VISA    PIC X(1).
      *    *************************************************************
           10 CCARAC-AUTRZ-VISA    PIC X(1).
      *    *************************************************************
           10 CID-AUTRZ-VISA       PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 CVALDC-AUTRZ-VISA    PIC X(4).
      *    *************************************************************
           10 CCOMPL-AUTRZ-VISA    PIC X(1).
      *    *************************************************************
           10 CTERM-PROCEDA        PIC S9(8)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
