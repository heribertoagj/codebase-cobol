      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPARM_CREDT_RURAL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB038))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB038)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPARM_CREDT_RURAL TABLE
           ( CPSSOA_JURID_CONGL             DECIMAL(10, 0) NOT NULL,
             CPARM_CREDT_RURAL              DECIMAL(5, 0) NOT NULL,
             CGRP_CREDT_RURAL               DECIMAL(2, 0) NOT NULL,
             CUND_MEDD                      DECIMAL(3, 0),
             CPERDC                         DECIMAL(2, 0),
             IPARM_RURAL                    CHAR(40) NOT NULL,
             CINDCD_CONTD_RURAL             DECIMAL(1, 0) NOT NULL,
             IREDZD_PARM_CREDT              CHAR(8) NOT NULL,
             DINIC_VGCIA_PARM               DATE NOT NULL,
             DFNAL_VGCIA_PARM               DATE NOT NULL,
             TFIXO_PARM_RURAL               DECIMAL(5, 0),
             VFIXO_CONTD_MONET              DECIMAL(15, 2),
             PFIXO_PARM_RURAL               DECIMAL(5, 2),
             RPARM_CREDT_RURAL              VARCHAR(256),
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPARM_CREDT_RURAL           *
      ******************************************************************
       01  CRURB038.
      *    *************************************************************
           10 CPSSOA-JURID-CONGL   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CPARM-CREDT-RURAL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CREDT-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPERDC               PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IPARM-RURAL          PIC X(40).
      *    *************************************************************
           10 CINDCD-CONTD-RURAL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IREDZD-PARM-CREDT    PIC X(8).
      *    *************************************************************
           10 DINIC-VGCIA-PARM     PIC X(10).
      *    *************************************************************
           10 DFNAL-VGCIA-PARM     PIC X(10).
      *    *************************************************************
           10 TFIXO-PARM-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VFIXO-CONTD-MONET    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PFIXO-PARM-RURAL     PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 RPARM-CREDT-RURAL.
              49 RPARM-CREDT-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 RPARM-CREDT-RURAL-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
