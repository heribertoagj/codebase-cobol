***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.BONIF_SERVC_HORA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB021))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB021)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.BONIF_SERVC_HORA TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CTPO_BONIF                     DECIMAL(3, 0) NOT NULL,
             CCRITR_BONIF                   DECIMAL(3, 0) NOT NULL,
             DINIC_REGRA_BONIF              DATE NOT NULL,
             CSEQ_BONIF_SERVC               DECIMAL(3, 0) NOT NULL,
             HINIC_FAIXA_BONIF              TIME NOT NULL,
             HFIM_FAIXA_BONIF               TIME NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.BONIF_SERVC_HORA            *
      ******************************************************************
       01  GFCTB021.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BONIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCRITR-BONIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-REGRA-BONIF    PIC X(10).
      *    *************************************************************
           10 CSEQ-BONIF-SERVC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HINIC-FAIXA-BONIF    PIC X(8).
      *    *************************************************************
           10 HFIM-FAIXA-BONIF     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
