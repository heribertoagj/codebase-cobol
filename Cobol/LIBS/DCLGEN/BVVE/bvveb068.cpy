      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TESTRT_INFO_CATAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB068))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB068)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TESTRT_INFO_CATAO TABLE
           ( NESTRT_INFO_BACEN              DECIMAL(3, 0) NOT NULL,
             CESTRT_INFO_BACEN              CHAR(100) NOT NULL,
             IESTRT_INFO_BACEN              CHAR(100) NOT NULL,
             CIDTFD_TPO_ESTRT               DECIMAL(1, 0) NOT NULL,
             DDSATV_ESTRT_INFO              DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TESTRT_INFO_CATAO           *
      ******************************************************************
       01  BVVEB068.
      *    *************************************************************
           10 NESTRT-INFO-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CESTRT-INFO-BACEN    PIC X(100).
      *    *************************************************************
           10 IESTRT-INFO-BACEN    PIC X(100).
      *    *************************************************************
           10 CIDTFD-TPO-ESTRT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DDSATV-ESTRT-INFO    PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
