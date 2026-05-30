*     ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCOMPS_MIDIA_PERIF)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0G8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0G8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCOMPS_MIDIA_PERIF TABLE
           ( CTPO_MIDIA                     DECIMAL(5, 0) NOT NULL,
             CTPO_PERIF                     DECIMAL(3, 0) NOT NULL,
             DINIC_VGCIA_PERIF              DATE NOT NULL,
             DFIM_VGCIA_PERIF               DATE NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CTERM_INCL_REG                 CHAR(8) NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL,
             CTERM_EXCL                     CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCOMPS_MIDIA_PERIF          *
      ******************************************************************
       01  GFCTB0G8.
      *    *************************************************************
           10 CTPO-MIDIA           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PERIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-PERIF    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-PERIF     PIC X(10).
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
