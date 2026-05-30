***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIERQ_ALCAD_PASSO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB082))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB082)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIERQ_ALCAD_PASSO TABLE
           ( CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_ALCAD_PASSO              DATE NOT NULL,
             DFIM_ALCAD_PASSO               DATE NOT NULL,
             QDIA_ANLSE_DEFER               DECIMAL(2, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIERQ_ALCAD_PASSO           *
      ******************************************************************
       01  GFCTB082.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-ALCAD-PASSO    PIC X(10).
      *    *************************************************************
           10 DFIM-ALCAD-PASSO     PIC X(10).
      *    *************************************************************
           10 QDIA-ANLSE-DEFER     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
