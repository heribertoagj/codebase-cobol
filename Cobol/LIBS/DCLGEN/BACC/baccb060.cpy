      ******************************************************************
      * DCLGEN TABLE(DB2PRD.RSUMO_TRANS_CATAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB060))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB060)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.RSUMO_TRANS_CATAO TABLE
           ( DMES_ANO_TRANS                 DECIMAL(6, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CFORMA_PGTO_CATAO              CHAR(1) NOT NULL,
             CIND_MOEDA                     DECIMAL(1, 0) NOT NULL,
             CORIGE_SOLTC_CATAO             DECIMAL(9, 0) NOT NULL,
             QTRANS_ANUDD_CATAO             DECIMAL(8, 0) NOT NULL,
             QCOMPR_PRZ_EMISR               DECIMAL(8, 0) NOT NULL,
             QCOMPR_PRZ_LOJST               DECIMAL(8, 0) NOT NULL,
             QTRANS_COMPR_VISTA             DECIMAL(8, 0) NOT NULL,
             QTRANS_SAQUE_CATAO             DECIMAL(8, 0) NOT NULL,
             VTRANS_ANUDD_CATAO             DECIMAL(13, 2) NOT NULL,
             VCOMPR_PRZ_EMISR               DECIMAL(13, 2) NOT NULL,
             VCOMPR_PRZ_LOJST               DECIMAL(13, 2) NOT NULL,
             VTRANS_COMPR_VISTA             DECIMAL(13, 2) NOT NULL,
             VTRANS_SAQUE_CATAO             DECIMAL(13, 2) NOT NULL,
             VSAQUE_PCELA_TRANS             DECIMAL(13, 2) NOT NULL,
             QSAQUE_PCELA_TRANS             DECIMAL(8, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.RSUMO_TRANS_CATAO           *
      ******************************************************************
       01  BACCB060.
      *    *************************************************************
           10 DMES-ANO-TRANS       PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-PGTO-CATAO    PIC X(1).
      *    *************************************************************
           10 CIND-MOEDA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-SOLTC-CATAO   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QTRANS-ANUDD-CATAO   PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 QCOMPR-PRZ-EMISR     PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 QCOMPR-PRZ-LOJST     PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 QTRANS-COMPR-VISTA   PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 QTRANS-SAQUE-CATAO   PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 VTRANS-ANUDD-CATAO   PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCOMPR-PRZ-EMISR     PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCOMPR-PRZ-LOJST     PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTRANS-COMPR-VISTA   PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTRANS-SAQUE-CATAO   PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSAQUE-PCELA-TRANS   PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QSAQUE-PCELA-TRANS   PIC S9(8)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
