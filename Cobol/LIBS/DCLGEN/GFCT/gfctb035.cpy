***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CUSTO_TARIF_PRINC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB035))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB035)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CUSTO_TARIF_PRINC TABLE
           ( CTPO_CUSTO_SERVC               DECIMAL(2, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_CUSTO              DATE NOT NULL,
             VCUSTO_SERVC_TARIF             DECIMAL(11, 6) NOT NULL,
             DFIM_VGCIA_CUSTO               DATE NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CUSTO_TARIF_PRINC           *
      ******************************************************************
       01  GFCTB035.
      *    *************************************************************
           10 CTPO-CUSTO-SERVC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-CUSTO    PIC X(10).
      *    *************************************************************
           10 VCUSTO-SERVC-TARIF   PIC S9(5)V9(6) USAGE COMP-3.
      *    *************************************************************
           10 DFIM-VGCIA-CUSTO     PIC X(10).
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
