      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TABUL_CATAO_ATIVO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB059))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB059)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TABUL_CATAO_ATIVO TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CTTLAR_DEPDT_CATAO             DECIMAL(1, 0) NOT NULL,
             DABERT_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             DVCTO_CATAO_CREDT              DECIMAL(6, 0) NOT NULL,
             DPROCM                         DATE NOT NULL,
             QCATAO_ATIVO_SMARI             DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TABUL_CATAO_ATIVO           *
      ******************************************************************
       01  BACCB059.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CTTLAR-DEPDT-CATAO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DABERT-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-CATAO-CREDT    PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DPROCM               PIC X(10).
      *    *************************************************************
           10 QCATAO-ATIVO-SMARI   PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
