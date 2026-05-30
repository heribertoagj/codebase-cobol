      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOPER_RENEG_CIPAG)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09C))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09C)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOPER_RENEG_CIPAG TABLE
           ( NOPER_RENEG_CIPAG              DECIMAL(9, 0) NOT NULL,
             COPER_ENVID_CIPAG              CHAR(20) NOT NULL,
             COPER_RENEG_CIPAG              CHAR(20) NOT NULL,
             COPER_GARNT_REG                CHAR(20) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOPER_RENEG_CIPAG           *
      ******************************************************************
       01  BVVEB09C.
      *    *************************************************************
           10 NOPER-RENEG-CIPAG    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      *    *************************************************************
           10 COPER-RENEG-CIPAG    PIC X(20).
      *    *************************************************************
           10 COPER-GARNT-REG      PIC X(20).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
