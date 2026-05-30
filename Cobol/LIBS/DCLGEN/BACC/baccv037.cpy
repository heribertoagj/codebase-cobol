      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01SOLTC_ALT_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV037))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV037)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01SOLTC_ALT_CATAO TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT_INSTC             CHAR(19) NOT NULL,
             CDADO_CATAO_CREDT              DECIMAL(3, 0) NOT NULL,
             HINCL                          TIMESTAMP NOT NULL,
             CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             WDADO_CATAO_CREDT              CHAR(30) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CFUNC_BDSCO_CANCT              DECIMAL(9, 0),
             DCANCT_SOLTC                   DATE,
             CCOBR_TARIF                    CHAR(1) NOT NULL,
             VTARIF_CLI                     DECIMAL(13, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01SOLTC_ALT_CATAO          *
      ******************************************************************
       01  BACCV037.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT-INSTC   PIC X(19).
      *    *************************************************************
           10 CDADO-CATAO-CREDT    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 WDADO-CATAO-CREDT    PIC X(30).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO-CANCT    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DCANCT-SOLTC         PIC X(10).
      *    *************************************************************
           10 CCOBR-TARIF          PIC X(1).
      *    *************************************************************
           10 VTARIF-CLI           PIC S9(11)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
