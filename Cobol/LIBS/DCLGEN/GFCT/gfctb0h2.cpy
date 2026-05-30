      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TASSOC_CJTO_NATUZ)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0H2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0H2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TASSOC_CJTO_NATUZ TABLE
           ( CCJTO_SERVC                    DECIMAL(3, 0) NOT NULL,
             CNATUZ_SERVC                   DECIMAL(3, 0) NOT NULL,
             DINIC_VGCIA_ASSOC              DATE NOT NULL,
             DFIM_VGCIA_ASSOC               DATE NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TASSOC_CJTO_NATUZ           *
      ******************************************************************
       01  GFCTB0H2.
      *    *************************************************************
           10 CCJTO-SERVC          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CNATUZ-SERVC         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-ASSOC    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-ASSOC     PIC X(10).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
