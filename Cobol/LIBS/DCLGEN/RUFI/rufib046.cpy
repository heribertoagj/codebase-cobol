      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPCELA_RURAL_RENEG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUFIB046))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUFIB046)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPCELA_RURAL_RENEG TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             CREFT_RECOR                    CHAR(11) NOT NULL,
             CTPO_EVNTO_RURAL               DECIMAL(4, 0) NOT NULL,
             DEVNTO                         DATE NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             NPCELA_CONTR                   DECIMAL(3, 0) NOT NULL,
             DVCTO                          DATE NOT NULL,
             VSDO_DVDOR_PRINC               DECIMAL(17, 2) NOT NULL,
             VTOT_ENCAR                     DECIMAL(17, 2) NOT NULL,
             VTOT_JURO_MORA                 DECIMAL(17, 2) NOT NULL,
             VTOT_MULTA                     DECIMAL(17, 2) NOT NULL,
             CINDCD_PCEL_RENEG              CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPCELA_RURAL_RENEG          *
      ******************************************************************
       01  RUFIB046.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CREFT-RECOR          PIC X(11).
      *    *************************************************************
           10 CTPO-EVNTO-RURAL     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 DEVNTO               PIC X(10).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-CONTR         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO                PIC X(10).
      *    *************************************************************
           10 VSDO-DVDOR-PRINC     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-ENCAR           PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-JURO-MORA       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-MULTA           PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PCEL-RENEG    PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
