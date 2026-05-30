      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTPO_MIDIA_SERVC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0G9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0G9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTPO_MIDIA_SERVC TABLE
           ( CTPO_MIDIA                     DECIMAL(5, 0) NOT NULL,
             RTPO_MIDIA                     CHAR(30) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CTERM_INCL_REG                 CHAR(8) NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL,
             CTERM_EXCL                     CHAR(8) NOT NULL,
             CINDCD_EXCL_REG                DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTPO_MIDIA_SERVC            *
      ******************************************************************
       01  GFCTB0G9.
      *    *************************************************************
           10 CTPO-MIDIA           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-MIDIA           PIC X(30).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CTERM-INCL-REG       PIC X(8).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CTERM-EXCL           PIC X(8).
      *    *************************************************************
           10 CINDCD-EXCL-REG      PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
