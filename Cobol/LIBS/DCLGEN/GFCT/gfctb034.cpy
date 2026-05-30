***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CTRL_PCOTE_MOVTO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB034))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB034)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CTRL_PCOTE_MOVTO TABLE
           ( CBCO_DSTNO_MOVTO               DECIMAL(3, 0) NOT NULL,
             CAG_DSTNO_MOVTO                DECIMAL(5, 0) NOT NULL,
             CCTA_DSTNO_MOVTO               DECIMAL(13, 0) NOT NULL,
             CTPO_COMPO_PCOTE               DECIMAL(1, 0) NOT NULL,
             CCOMPO_PCOTE                   DECIMAL(5, 0) NOT NULL,
             DINIC_CTRL_PCOTE               DATE NOT NULL,
             CTARIF_PCOTE                   DECIMAL(5, 0) NOT NULL,
             QSERVC_UTLZD_PCOTE             DECIMAL(7, 0) NOT NULL,
             DFIM_CTRL_PCOTE                DATE NOT NULL,
             QMOVTO_COMPO_PCOTE             DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CTRL_PCOTE_MOVTO            *
      ******************************************************************
       01  GFCTB034.
      *    *************************************************************
           10 CBCO-DSTNO-MOVTO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-DSTNO-MOVTO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-DSTNO-MOVTO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-COMPO-PCOTE     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPO-PCOTE         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-CTRL-PCOTE     PIC X(10).
      *    *************************************************************
           10 CTARIF-PCOTE         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QSERVC-UTLZD-PCOTE   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-CTRL-PCOTE      PIC X(10).
      *    *************************************************************
           10 QMOVTO-COMPO-PCOTE   PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
