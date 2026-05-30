      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_EXCEC_HTCRD)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB047))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB047)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_EXCEC_HTCRD TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             HINCL                          TIMESTAMP NOT NULL,
             CCANCT_CATAO_CREDT             CHAR(2) NOT NULL,
             DCANCT                         DECIMAL(8, 0) NOT NULL,
             CRGIAO_BOLET                   CHAR(1) NOT NULL,
             CATULZ_HIST_EXCEC              DECIMAL(1, 0) NOT NULL,
             CTPO_ATULZ_HTCRD               DECIMAL(1, 0) NOT NULL,
             CRGIAO_HIST_EXCEC              CHAR(9) NOT NULL,
             DRETRD_HIST_EXCEC              DECIMAL(8, 0) NOT NULL,
             CACAO_VISA                     CHAR(2) NOT NULL,
             COPERD_CICS                    CHAR(3) NOT NULL,
             DTRNSM                         DATE NOT NULL,
             HTRNSM                         TIME NOT NULL
           ) END-EXEC.
      ******************************************************************
      *  DB2PRD.HIST_EXCEC_HTCRD                                       *
      ******************************************************************
       01  BACCB047.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CCANCT-CATAO-CREDT   PIC X(2).
      *    *************************************************************
           10 DCANCT               PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-BOLET         PIC X(1).
      *    *************************************************************
           10 CATULZ-HIST-EXCEC    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ATULZ-HTCRD     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-HIST-EXCEC    PIC X(9).
      *    *************************************************************
           10 DRETRD-HIST-EXCEC    PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CACAO-VISA           PIC X(2).
      *    *************************************************************
           10 COPERD-CICS          PIC X(3).
      *    *************************************************************
           10 DTRNSM               PIC X(10).
      *    *************************************************************
           10 HTRNSM               PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
