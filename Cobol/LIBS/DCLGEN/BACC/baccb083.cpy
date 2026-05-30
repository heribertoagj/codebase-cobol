      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_ATVDD_COML_MCA)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB083))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB083)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_ATVDD_COML_MCA TABLE
           ( CTPO_ATVDD_COML                DECIMAL(5, 0) NOT NULL,
             RATVDD_COML                    CHAR(40) NOT NULL,
             CTRATO_DSBTO_ORPAG             DECIMAL(1, 0) NOT NULL,
             VLIM_OPER_LOC                  DECIMAL(9, 2) NOT NULL,
             VLIM_OPER_INTL                 DECIMAL(9, 2) NOT NULL,
             PCOMIS_ICBIO                   DECIMAL(5, 2) NOT NULL,
             CTPO_ATVDD_TE                  CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_ATVDD_COML_MCA          *
      ******************************************************************
       01  BACCB083.
      *    *************************************************************
           10 CTPO-ATVDD-COML      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RATVDD-COML          PIC X(40).
      *    *************************************************************
           10 CTRATO-DSBTO-ORPAG   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VLIM-OPER-LOC        PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VLIM-OPER-INTL       PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PCOMIS-ICBIO         PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ATVDD-TE        PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
