      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSOLO_IMOV_BENEF)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0VJ))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0VJ)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSOLO_IMOV_BENEF TABLE
           ( CORCAM_CONTR_RURAL             DECIMAL(10, 0) NOT NULL,
             CIMOV_CREDT_RURAL              DECIMAL(10, 0) NOT NULL,
             CTPO_SOLO_AGCLT                DECIMAL(2, 0) NOT NULL,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CCICLO_CTVDA                   DECIMAL(2, 0),
             CMANUT_SOLO                    DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSOLO_IMOV_BENEF            *
      ******************************************************************
       01  CRURB0VJ.
      *    *************************************************************
           10 CORCAM-CONTR-RURAL   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CIMOV-CREDT-RURAL    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-SOLO-AGCLT      PIC S9(2)V USAGE COMP-3.
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
           10 CCICLO-CTVDA         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CMANUT-SOLO          PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
