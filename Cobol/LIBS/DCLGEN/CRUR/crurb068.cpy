      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TEMPTO_RENDA_AGROP)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB068))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB068)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TEMPTO_RENDA_AGROP TABLE
           ( CPSSOA                         DECIMAL(10, 0) NOT NULL,
             CFICHA_CAD_RURAL               DECIMAL(3, 0) NOT NULL,
             CEMPTO_RURAL                   DECIMAL(6, 0) NOT NULL,
             NEMPTO_RENDA_AGROP             DECIMAL(5, 0) NOT NULL,
             CUND_MEDD                      DECIMAL(3, 0) NOT NULL,
             QEMPTO_RENDA_AGROP             DECIMAL(9, 2) NOT NULL,
             VEMPTO_RENDA_AGROP             DECIMAL(15, 2) NOT NULL,
             PRBATE_EMPTO_RENDA             DECIMAL(9, 6),
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CVARDE_RURAL_BACEN             DECIMAL(5, 0),
             CCESTA_SAFRA_BACEN             DECIMAL(5, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TEMPTO_RENDA_AGROP          *
      ******************************************************************
       01  CRURB068.
      *    *************************************************************
           10 CPSSOA               PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CFICHA-CAD-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPTO-RURAL         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 NEMPTO-RENDA-AGROP   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QEMPTO-RENDA-AGROP   PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VEMPTO-RENDA-AGROP   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PRBATE-EMPTO-RENDA   PIC S9(3)V9(6) USAGE COMP-3.
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
           10 CVARDE-RURAL-BACEN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCESTA-SAFRA-BACEN   PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
