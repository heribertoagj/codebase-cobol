      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SOLTC_RELAT_ONLINE)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0D9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0D9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SOLTC_RELAT_ONLINE TABLE
           ( CIMPRE_RELAT                   CHAR(8) NOT NULL,
             HSOLTC_IMPRE                   TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             DDIA_INIC_RELAT                DECIMAL(2, 0) NOT NULL,
             DMES_INIC_RELAT                DECIMAL(2, 0) NOT NULL,
             DANO_INIC_RELAT                DECIMAL(4, 0) NOT NULL,
             DDIA_FIM_RELAT                 DECIMAL(2, 0) NOT NULL,
             DMES_FIM_RELAT                 DECIMAL(2, 0) NOT NULL,
             DANO_FIM_RELAT                 DECIMAL(4, 0) NOT NULL,
             VMIN_RELAT                     DECIMAL(11, 2) NOT NULL,
             VMAX_RELAT                     DECIMAL(11, 2) NOT NULL,
             CSIT_SOLTC_RELAT               DECIMAL(1, 0) NOT NULL,
             CINDCD_DEPDC_TOT               DECIMAL(1, 0) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL,
             CIDTFD_FLEXZ_IMPRE             DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SOLTC_RELAT_ONLINE          *
      ******************************************************************
       01  GFCTB0D9.
      *    *************************************************************
           10 CIMPRE-RELAT         PIC X(8).
      *    *************************************************************
           10 HSOLTC-IMPRE         PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DDIA-INIC-RELAT      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-INIC-RELAT      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-INIC-RELAT      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 DDIA-FIM-RELAT       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-FIM-RELAT       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-FIM-RELAT       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 VMIN-RELAT           PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMAX-RELAT           PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSIT-SOLTC-RELAT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-DEPDC-TOT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-FLEXZ-IMPRE   PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
