      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_MOVTO_CTBIL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RUMOB002))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUMOB002)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_MOVTO_CTBIL TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             DVCTO                          DATE NOT NULL,
             CHIST_MOVTO_CTBIL              TIMESTAMP NOT NULL,
             CTPO_EVNTO_RURAL               DECIMAL(4, 0) NOT NULL,
             VPCELA                         DECIMAL(15, 2) NOT NULL,
             VJURO                          DECIMAL(15, 2) NOT NULL,
             VCORRC_ATULZ_MONET             DECIMAL(15, 2) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0),
             DCTBIL_ENCAR_MORA              DATE,
             CINDCD_RETRD_PCELA             DECIMAL(1, 0) NOT NULL,
             VJURO_REMUN_ATRSO              DECIMAL(15, 2) NOT NULL,
             VJURO_MORA_PCELA               DECIMAL(15, 2) NOT NULL,
             VMULTA_CONTR_ATRSO             DECIMAL(15, 2) NOT NULL,
             VJURO_CREDT_ATRSO              DECIMAL(15, 2) NOT NULL,
             VJURO_MORA_CREDT               DECIMAL(15, 2) NOT NULL,
             VMULTA_CREDT_ATRSO             DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_MOVTO_CTBIL            *
      ******************************************************************
       01  RUMOB002.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO                PIC X(10).
      *    *************************************************************
           10 CHIST-MOVTO-CTBIL    PIC X(26).
      *    *************************************************************
           10 CTPO-EVNTO-RURAL     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 VPCELA               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO                PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCORRC-ATULZ-MONET   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DCTBIL-ENCAR-MORA    PIC X(10).
      *    *************************************************************
           10 CINDCD-RETRD-PCELA   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VJURO-REMUN-ATRSO    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO-MORA-PCELA     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMULTA-CONTR-ATRSO   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO-CREDT-ATRSO    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO-MORA-CREDT     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMULTA-CREDT-ATRSO   PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
