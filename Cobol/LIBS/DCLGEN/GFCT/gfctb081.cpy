***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIERQ_ALCAD_EXCEC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB081))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB081)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIERQ_ALCAD_EXCEC TABLE
           ( CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CIDTFD_DEPDC                   CHAR(2) NOT NULL,
             DINIC_ALCAD_EXCEC              DATE NOT NULL,
             DFIM_ALCAD_EXCEC               DATE NOT NULL,
             CEMPR_SUPE                     DECIMAL(5, 0) NOT NULL,
             CDEPDC_SUPE                    DECIMAL(5, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CSIT_HIERQ_ALCAD               DECIMAL(1, 0) NOT NULL,
             CUSUAR_DEFER_HIERQ             DECIMAL(9, 0) NOT NULL,
             HDEFER_HIERQ_ALCAD             TIMESTAMP NOT NULL,
             RJUSTF_PAREC_HIERQ             VARCHAR(256) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIERQ_ALCAD_EXCEC           *
      ******************************************************************
       01  GFCTB081.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-DEPDC         PIC X(2).
      *    *************************************************************
           10 DINIC-ALCAD-EXCEC    PIC X(10).
      *    *************************************************************
           10 DFIM-ALCAD-EXCEC     PIC X(10).
      *    *************************************************************
           10 CEMPR-SUPE           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-SUPE          PIC S9(5)V USAGE COMP-3.
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
      *    *************************************************************
           10 CSIT-HIERQ-ALCAD     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-DEFER-HIERQ   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HDEFER-HIERQ-ALCAD   PIC X(26).
      *    *************************************************************
           10 RJUSTF-PAREC-HIERQ.
              49 RJUSTF-PAREC-HIERQ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-PAREC-HIERQ-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
