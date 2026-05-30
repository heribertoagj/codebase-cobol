***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.BONIF_PCOTE_QTD)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB019))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB019)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.BONIF_PCOTE_QTD TABLE
           ( CTARIF_PCOTE                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_COMPS              DATE NOT NULL,
             CSEQ_COMPO_PCOTE               DECIMAL(3, 0) NOT NULL,
             CTPO_BONIF                     DECIMAL(3, 0) NOT NULL,
             CCRITR_BONIF                   DECIMAL(3, 0) NOT NULL,
             DINIC_REGRA_BONIF              DATE NOT NULL,
             CSEQ_BONIF_PCOTE               DECIMAL(4, 0) NOT NULL,
             QMIN_FAIXA_BONIF               DECIMAL(5, 0) NOT NULL,
             QMAX_FAIXA_BONIF               DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.BONIF_PCOTE_QTD             *
      ******************************************************************
       01  GFCTB019.
      *    *************************************************************
           10 CTARIF-PCOTE         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-COMPS    PIC X(10).
      *    *************************************************************
           10 CSEQ-COMPO-PCOTE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BONIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCRITR-BONIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-REGRA-BONIF    PIC X(10).
      *    *************************************************************
           10 CSEQ-BONIF-PCOTE     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 QMIN-FAIXA-BONIF     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-FAIXA-BONIF     PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
