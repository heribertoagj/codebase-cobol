      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TALCAD_LCTO_PDRAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0H1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0H1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TALCAD_LCTO_PDRAO TABLE
           ( CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CIDTFD_DEPDC                   CHAR(2) NOT NULL,
             DINIC_ALCAD_PDRAO              DATE NOT NULL,
             DFIM_ALCAD_PDRAO               DATE NOT NULL,
             VALCAD_DEPDC                   DECIMAL(11, 2) NOT NULL,
             PMAX_ALCAD_TARIF               DECIMAL(7, 4) NOT NULL,
             QMAX_FRANQ_ALCAD               DECIMAL(3, 0) NOT NULL,
             QMAX_RTCAO_ALCAD               DECIMAL(2, 0) NOT NULL,
             CIND_PRMSS_AGNDA               CHAR(1) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CSIT_LCTO_ALCAD                DECIMAL(1, 0) NOT NULL,
             CUSUAR_DEFER_ALCAD             DECIMAL(9, 0) NOT NULL,
             HDEFER_LCTO_ALCAD              TIMESTAMP NOT NULL,
             RJUSTF_PAREC_ALCAD             VARCHAR(256) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TALCAD_LCTO_PDRAO           *
      ******************************************************************
       01  GFCTB0H1.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-DEPDC         PIC X(2).
      *    *************************************************************
           10 DINIC-ALCAD-PDRAO    PIC X(10).
      *    *************************************************************
           10 DFIM-ALCAD-PDRAO     PIC X(10).
      *    *************************************************************
           10 VALCAD-DEPDC         PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PMAX-ALCAD-TARIF     PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 QMAX-FRANQ-ALCAD     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-RTCAO-ALCAD     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIND-PRMSS-AGNDA     PIC X(1).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CSIT-LCTO-ALCAD      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-DEFER-ALCAD   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HDEFER-LCTO-ALCAD    PIC X(26).
      *    *************************************************************
           10 RJUSTF-PAREC-ALCAD.
              49 RJUSTF-PAREC-ALCAD-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-PAREC-ALCAD-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
