***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIERQ_AGPTO_PPRIO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB080))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB080)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIERQ_AGPTO_PPRIO TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_ORD_AGPTO                DATE NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CNVEL_HIERQ_AGPTO              DECIMAL(3, 0) NOT NULL,
             DFIM_ORD_AGPTO                 DATE NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIERQ_AGPTO_PPRIO           *
      ******************************************************************
       01  GFCTB080.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-ORD-AGPTO      PIC X(10).
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CNVEL-HIERQ-AGPTO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-ORD-AGPTO       PIC X(10).
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
