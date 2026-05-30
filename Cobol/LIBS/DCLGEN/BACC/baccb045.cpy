      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOT_CATAO_DIA)                             *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB045))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB045)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOT_CATAO_DIA TABLE
           ( DMES_ANO_TOT_CATAO             DECIMAL(6, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CFORMA_PGTO_CATAO              CHAR(1) NOT NULL,
             CTTLAR_DEPDT_CATAO             DECIMAL(1, 0) NOT NULL,
             CORIGE_SOLTC_CATAO             DECIMAL(9, 0) NOT NULL,
             QCATAO_CREDT_CANCD             DECIMAL(7, 0) NOT NULL,
             QCATAO_CREDT_RENOV             DECIMAL(7, 0) NOT NULL,
             QCATAO_CREDT_EMTDO             DECIMAL(7, 0) NOT NULL,
             QCATAO_CREDT_REEMS             DECIMAL(7, 0) NOT NULL,
             QCATAO_CREDT_MOVTC             DECIMAL(7, 0) NOT NULL,
             QCATAO_CREDT_ATIVO             DECIMAL(7, 0) NOT NULL,
             QCATAO_CREDT_CCAMP             DECIMAL(7, 0) NOT NULL,
             QCATAO_CREDT_ECAMP             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_DESIN             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_BLOQ              DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_ADMTV             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_TEMPR             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_FURTO             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_EXTRV             DECIMAL(7, 0) NOT NULL,
             QCANCD_TEMPR_INADP             DECIMAL(7, 0) NOT NULL,
             QCANCD_PRE_APROV               DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_FRAUD             DECIMAL(7, 0) NOT NULL,
             QCATAO_BLOQ_TEMPR              DECIMAL(7, 0) NOT NULL,
             QCATAO_BLOQ_PRECA              DECIMAL(7, 0) NOT NULL,
             QCATAO_DESIN_DEFTV             DECIMAL(7, 0) NOT NULL,
             QCATAO_DESIN_PRVIO             DECIMAL(7, 0) NOT NULL,
             QCATAO_RENEG_DIVDA             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_VENCD             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_TROCA             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_TRNSF             DECIMAL(7, 0) NOT NULL,
             QCATAO_ATIVO_BRANC             DECIMAL(7, 0) NOT NULL,
             QCATAO_CANCD_RENOV             DECIMAL(7, 0) NOT NULL,
             QCATAO_ATIVO_RENOV             DECIMAL(7, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOT_CATAO_DIA               *
      ******************************************************************
       01  BACCB045.
      *    *************************************************************
           10 DMES-ANO-TOT-CATAO   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-PGTO-CATAO    PIC X(1).
      *    *************************************************************
           10 CTTLAR-DEPDT-CATAO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-SOLTC-CATAO   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CREDT-CANCD   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CREDT-RENOV   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CREDT-EMTDO   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CREDT-REEMS   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CREDT-MOVTC   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CREDT-ATIVO   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CREDT-CCAMP   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CREDT-ECAMP   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-DESIN   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-BLOQ    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-ADMTV   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-TEMPR   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-FURTO   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-EXTRV   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCANCD-TEMPR-INADP   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCANCD-PRE-APROV     PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-FRAUD   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-BLOQ-TEMPR    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-BLOQ-PRECA    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-DESIN-DEFTV   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-DESIN-PRVIO   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-RENEG-DIVDA   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-VENCD   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-TROCA   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-TRNSF   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-ATIVO-BRANC   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-CANCD-RENOV   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QCATAO-ATIVO-RENOV   PIC S9(7)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 34      *
      ******************************************************************
