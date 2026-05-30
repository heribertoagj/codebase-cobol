      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01SIT_CATAO_CREDT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV035))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV035)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01SIT_CATAO_CREDT TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             TINADP                         DECIMAL(3, 0) NOT NULL,
             CIND_COBR_VENCD                DECIMAL(1, 0) NOT NULL,
             VACUM_CATAO                    DECIMAL(16, 2) NOT NULL,
             VACUM_DOLAR_CATAO              DECIMAL(16, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01SIT_CATAO_CREDT          *
      ******************************************************************
       01  BACCV035.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 TINADP               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIND-COBR-VENCD      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VACUM-CATAO          PIC S9(14)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VACUM-DOLAR-CATAO    PIC S9(14)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
