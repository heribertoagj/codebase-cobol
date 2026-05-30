***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TASSOC_AGPTO_CTBIL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0I8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0I8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TASSOC_AGPTO_CTBIL TABLE
           ( CAGPTO_CTBIL                   DECIMAL(5, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_ASSOC              DATE NOT NULL,
             DFIM_VGCIA_ASSOC               DATE NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TASSOC_AGPTO_CTBIL          *
      ******************************************************************
       01  GFCTB0I8.
      *    *************************************************************
           10 CAGPTO-CTBIL         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-ASSOC    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-ASSOC     PIC X(10).
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
