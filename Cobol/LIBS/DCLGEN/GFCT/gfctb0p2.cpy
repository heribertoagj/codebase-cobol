***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TALCAD_LCTO_DEPTO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0P2))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0P2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TALCAD_LCTO_DEPTO TABLE
           ( NALCAD_LCTO                    DECIMAL(10, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0),
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             DINIC_ALCAD_PDRAO              DATE NOT NULL,
             DFIM_ALCAD_PDRAO               DATE NOT NULL,
             VALCAD_DEPDC                   DECIMAL(11, 2) NOT NULL,
             RJUSTF_INCL_AUTRZ              VARCHAR(256),
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0),
             HMANUT_REG                     TIMESTAMP,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TALCAD_LCTO_DEPTO           *
      ******************************************************************
       01  GFCTB0P2.
      *    *************************************************************
           10 NALCAD-LCTO          PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-ALCAD-PDRAO    PIC X(10).
      *    *************************************************************
           10 DFIM-ALCAD-PDRAO     PIC X(10).
      *    *************************************************************
           10 VALCAD-DEPDC         PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-INCL-AUTRZ.
              49 RJUSTF-INCL-AUTRZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-INCL-AUTRZ-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
