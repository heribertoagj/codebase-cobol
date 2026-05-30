      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONTR_RURAL_RENEG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUFIB045))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUFIB045)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONTR_RURAL_RENEG TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             CREFT_RECOR                    CHAR(11) NOT NULL,
             VRENEG_CONTR_RURAL             DECIMAL(17, 2) NOT NULL,
             CBASE_LEGAL_RENEG              CHAR(5) NOT NULL,
             VSDO_DVDOR_PRINC               DECIMAL(17, 2) NOT NULL,
             VTOT_ENCAR                     DECIMAL(17, 2) NOT NULL,
             VTOT_JURO_MORA                 DECIMAL(17, 2) NOT NULL,
             VTOT_MULTA                     DECIMAL(17, 2) NOT NULL,
             CINDCD_TPO_RENEG               CHAR(1),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL                          TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONTR_RURAL_RENEG          *
      ******************************************************************
       01  RUFIB045.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CREFT-RECOR          PIC X(11).
      *    *************************************************************
           10 VRENEG-CONTR-RURAL   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CBASE-LEGAL-RENEG    PIC X(5).
      *    *************************************************************
           10 VSDO-DVDOR-PRINC     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-ENCAR           PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-JURO-MORA       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-MULTA           PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TPO-RENEG     PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
