      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCORND_IMOV_BENEF)                         *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0V4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0V4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCORND_IMOV_BENEF TABLE
           ( CORCAM_CONTR_RURAL             DECIMAL(10, 0) NOT NULL,
             CIMOV_CREDT_RURAL              DECIMAL(10, 0) NOT NULL,
             NSEQ_GLEBA_IMOV                DECIMAL(5, 0) NOT NULL,
             NCORND_IMOV_BENEF              DECIMAL(5, 0) NOT NULL,
             MLATTD_CORND_IMOV              DECIMAL(13, 11) NOT NULL,
             MLOGTD_CORND_IMOV              DECIMAL(14, 11) NOT NULL,
             MALTTD_CORND_IMOV              DECIMAL(6, 2) NOT NULL,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CTPO_CORND_GEOGR               DECIMAL(1, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCORND_IMOV_BENEF           *
      ******************************************************************
       01  CRURB0V4.
      *    *************************************************************
           10 CORCAM-CONTR-RURAL   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CIMOV-CREDT-RURAL    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-GLEBA-IMOV      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NCORND-IMOV-BENEF    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 MLATTD-CORND-IMOV    PIC S9(2)V9(11) USAGE COMP-3.
      *    *************************************************************
           10 MLOGTD-CORND-IMOV    PIC S9(3)V9(11) USAGE COMP-3.
      *    *************************************************************
           10 MALTTD-CORND-IMOV    PIC S9(4)V9(2) USAGE COMP-3.
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
      *    *************************************************************
           10 CTPO-CORND-GEOGR     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
