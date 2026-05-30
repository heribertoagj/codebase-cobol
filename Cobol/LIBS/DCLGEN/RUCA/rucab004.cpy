      ******************************************************************
      * DCLGEN TABLE(DB2PRD.RUCA_CAD_DEP)                              *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB004))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB004)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.RUCA_CAD_DEP TABLE
           ( B004_CH_CONTR                  DECIMAL(9, 0) NOT NULL,
             B004_CH_TIPO                   DECIMAL(3, 0) NOT NULL,
             B004_CH_NUMERO                 DECIMAL(7, 0) NOT NULL,
             B004_ESPECIE                   CHAR(1) NOT NULL,
             B004_CONTROLE                  CHAR(8) NOT NULL,
             B004_DTEVENTO                  DATE,
             B004_CODLANC                   DECIMAL(3, 0) NOT NULL,
             B004_DTCONTAB                  DATE,
             B004_DTACERT                   DATE,
             B004_IDENTDC                   CHAR(1) NOT NULL,
             B004_DEBCRED                   CHAR(1) NOT NULL,
             B004_VLPRIORI                  DECIMAL(15, 2) NOT NULL,
             B004_VLPRICOR                  DECIMAL(15, 2) NOT NULL,
             B004_VLMUTORI                  DECIMAL(15, 2) NOT NULL,
             B004_VLMUTCOR                  DECIMAL(15, 2) NOT NULL,
             B004_VLPROORI                  DECIMAL(15, 2) NOT NULL,
             B004_VLPROCOR                  DECIMAL(15, 2) NOT NULL,
             B004_VLPRMUORI                 DECIMAL(15, 2) NOT NULL,
             B004_VLPRMUCOR                 DECIMAL(15, 2) NOT NULL,
             B004_VLAUXIL                   DECIMAL(15, 2) NOT NULL,
             B004_JURORIG                   DECIMAL(15, 2) NOT NULL,
             B004_JURCORRI                  DECIMAL(15, 2) NOT NULL,
             B004_CORRORIG                  DECIMAL(15, 2) NOT NULL,
             B004_CORRCORRI                 DECIMAL(15, 2) NOT NULL,
             B004_PROAORIG                  DECIMAL(15, 2) NOT NULL,
             B004_PROACORR                  DECIMAL(15, 2) NOT NULL,
             B004_ASSIORIG                  DECIMAL(15, 2) NOT NULL,
             B004_ASSICORR                  DECIMAL(15, 2) NOT NULL,
             B004_LAUDORIG                  DECIMAL(15, 2) NOT NULL,
             B004_LAUDCORR                  DECIMAL(15, 2) NOT NULL,
             B004_MEDIORIG                  DECIMAL(15, 2) NOT NULL,
             B004_MEDICORR                  DECIMAL(15, 2) NOT NULL,
             B004_REMUORIG                  DECIMAL(15, 2) NOT NULL,
             B004_REMUCORR                  DECIMAL(15, 2) NOT NULL,
             B004_IOFORIG                   DECIMAL(15, 2) NOT NULL,
             B004_IOFCORR                   DECIMAL(15, 2) NOT NULL,
             B004_AGDEBCRED                 DECIMAL(5, 0) NOT NULL,
             B004_RZDEBCRED                 DECIMAL(5, 0) NOT NULL,
             B004_CTDEBCRED                 DECIMAL(7, 0) NOT NULL,
             B004_DTMORA                    DECIMAL(9, 0) NOT NULL,
             B004_LANCMORA                  DECIMAL(3, 0) NOT NULL,
             B004_DBCRMORA                  CHAR(1) NOT NULL,
             B004_NUMMORA                   DECIMAL(7, 0) NOT NULL,
             B004_CODCRS                    CHAR(4) NOT NULL,
             B004_DTACERTO                  DATE,
             B004_CODACERT                  DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.RUCA_CAD_DEP                *
      ******************************************************************
       01  RUCAB004.
      *    *************************************************************
           10 B004-CH-CONTR        PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 B004-CH-TIPO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B004-CH-NUMERO       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 B004-ESPECIE         PIC X(1).
      *    *************************************************************
           10 B004-CONTROLE        PIC X(8).
      *    *************************************************************
           10 B004-DTEVENTO        PIC X(10).
      *    *************************************************************
           10 B004-CODLANC         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B004-DTCONTAB        PIC X(10).
      *    *************************************************************
           10 B004-DTACERT         PIC X(10).
      *    *************************************************************
           10 B004-IDENTDC         PIC X(1).
      *    *************************************************************
           10 B004-DEBCRED         PIC X(1).
      *    *************************************************************
           10 B004-VLPRIORI        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-VLPRICOR        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-VLMUTORI        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-VLMUTCOR        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-VLPROORI        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-VLPROCOR        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-VLPRMUORI       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-VLPRMUCOR       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-VLAUXIL         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-JURORIG         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-JURCORRI        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-CORRORIG        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-CORRCORRI       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-PROAORIG        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-PROACORR        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-ASSIORIG        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-ASSICORR        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-LAUDORIG        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-LAUDCORR        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-MEDIORIG        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-MEDICORR        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-REMUORIG        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-REMUCORR        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-IOFORIG         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-IOFCORR         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B004-AGDEBCRED       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 B004-RZDEBCRED       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 B004-CTDEBCRED       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 B004-DTMORA          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 B004-LANCMORA        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B004-DBCRMORA        PIC X(1).
      *    *************************************************************
           10 B004-NUMMORA         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 B004-CODCRS          PIC X(4).
      *    *************************************************************
           10 B004-DTACERTO        PIC X(10).
      *    *************************************************************
           10 B004-CODACERT        PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 46      *
      ******************************************************************
