      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01FONET_ESTBL_BAC)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV032))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV032)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01FONET_ESTBL_BAC TABLE
           ( CFONET                         CHAR(6) NOT NULL,
             CPREFX_ESTBL_BACC              DECIMAL(4, 0) NOT NULL,
             CBCO_VISA                      DECIMAL(3, 0) NOT NULL,
             CESTBL_CONVN_BACC              DECIMAL(6, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01FONET_ESTBL_BAC          *
      ******************************************************************
       01  BACCV032.
      *    *************************************************************
           10 CFONET               PIC X(6).
      *    *************************************************************
           10 CPREFX-ESTBL-BACC    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-VISA            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CESTBL-CONVN-BACC    PIC S9(6)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
