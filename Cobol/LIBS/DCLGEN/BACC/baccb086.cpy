      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRANS_LIBRD_VERIF)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB086))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB086)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRANS_LIBRD_VERIF TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             DCOMPR                         DATE NOT NULL,
             CSEQ                           DECIMAL(9, 0) NOT NULL,
             DMOVTO                         DATE NOT NULL,
             IESTBL_CATAO_CREDT             CHAR(35) NOT NULL,
             CAUTRZ_VDA_CATAO               CHAR(6) NOT NULL,
             VCOMPR_CATAO_CREDT             DECIMAL(13, 2) NOT NULL,
             VMAX_LIM_CATAO                 DECIMAL(11, 2) NOT NULL,
             CCANCT_CATAO_CREDT             CHAR(2) NOT NULL,
             CMOEDA_ORIGE_TRANS             CHAR(3) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRANS_LIBRD_VERIF           *
      ******************************************************************
       01  BACCB086.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 DCOMPR               PIC X(10).
      *    *************************************************************
           10 CSEQ                 PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 IESTBL-CATAO-CREDT   PIC X(35).
      *    *************************************************************
           10 CAUTRZ-VDA-CATAO     PIC X(6).
      *    *************************************************************
           10 VCOMPR-CATAO-CREDT   PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMAX-LIM-CATAO       PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCANCT-CATAO-CREDT   PIC X(2).
      *    *************************************************************
           10 CMOEDA-ORIGE-TRANS   PIC X(3).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
