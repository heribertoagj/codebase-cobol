      ******************************************************************
      * DCLGEN TABLE(DB2PRD.QTD_CATAO_MES)                             *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB093))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB093)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.QTD_CATAO_MES TABLE
           ( DMES_ANO_TOT_CATAO             DECIMAL(6, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CFORMA_PGTO_CATAO              CHAR(1) NOT NULL,
             CTTLAR_DEPDT_CATAO             DECIMAL(1, 0) NOT NULL,
             CORIGE_SOLTC_CATAO             DECIMAL(9, 0) NOT NULL,
             CTPO_QTD_CATAO                 CHAR(3) NOT NULL,
             QCATAO_CREDT                   DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.QTD_CATAO_MES               *
      ******************************************************************
       01  BACCB093.
      *    *************************************************************
           10 DMES-ANO-TOT-CATAO   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-PGTO-CATAO    PIC X(1).
      *    *************************************************************
           10 CTTLAR-DEPDT-CATAO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-SOLTC-CATAO   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-QTD-CATAO       PIC X(3).
      *    *************************************************************
           10 QCATAO-CREDT         PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
