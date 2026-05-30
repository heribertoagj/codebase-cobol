      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCORND_GEOGR_CONTR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB09N))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB09N)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCORND_GEOGR_CONTR TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             CSEQ_MATR_RURAL                DECIMAL(3, 0) NOT NULL,
             NSEQ_GLEBA_IMOV                DECIMAL(5, 0) NOT NULL,
             NCORND_GEOGR_IMOV              DECIMAL(5, 0) NOT NULL,
             MLATTD_IMOV_RURAL              DECIMAL(14, 11) NOT NULL,
             MLOGTD_IMOV_RURAL              DECIMAL(14, 11) NOT NULL,
             MALTTD_IMOV_RURAL              DECIMAL(7, 2) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL                          TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CTPO_CORND_GEOGR               DECIMAL(1, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCORND_GEOGR_CONTR          *
      ******************************************************************
       01  RUECB09N.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MATR-RURAL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-GLEBA-IMOV      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NCORND-GEOGR-IMOV    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 MLATTD-IMOV-RURAL    PIC S9(3)V9(11) USAGE COMP-3.
      *    *************************************************************
           10 MLOGTD-IMOV-RURAL    PIC S9(3)V9(11) USAGE COMP-3.
      *    *************************************************************
           10 MALTTD-IMOV-RURAL    PIC S9(5)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CTPO-CORND-GEOGR     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
