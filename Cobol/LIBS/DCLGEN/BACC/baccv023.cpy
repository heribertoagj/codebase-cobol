      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CATAO_CREDT_CAN)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV023))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV023)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CATAO_CREDT_CAN TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             DCANCT                         DATE NOT NULL,
             DVALDD_CANCT_CATAO             DATE NOT NULL,
             CSUFXO_CATAO_CREDT             DECIMAL(3, 0) NOT NULL,
             CBOLET_VISA_CANCT              DECIMAL(5, 0) NOT NULL,
             CULT_BOLET_VISA                DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CATAO_CREDT_CAN          *
      ******************************************************************
       01  BACCV023.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 DCANCT               PIC X(10).
      *    *************************************************************
           10 DVALDD-CANCT-CATAO   PIC X(10).
      *    *************************************************************
           10 CSUFXO-CATAO-CREDT   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBOLET-VISA-CANCT    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CULT-BOLET-VISA      PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
