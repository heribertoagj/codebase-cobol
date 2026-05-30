***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.BONIF_SERVC_VLR)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB024))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB024)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.BONIF_SERVC_VLR TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CTPO_BONIF                     DECIMAL(3, 0) NOT NULL,
             CCRITR_BONIF                   DECIMAL(3, 0) NOT NULL,
             DINIC_REGRA_BONIF              DATE NOT NULL,
             CSEQ_BONIF_SERVC               DECIMAL(3, 0) NOT NULL,
             VMIN_FAIXA_BONIF               DECIMAL(11, 2) NOT NULL,
             VMAX_FAIXA_BONIF               DECIMAL(11, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.BONIF_SERVC_VLR             *
      ******************************************************************
       01  GFCTB024.
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
           10 VMIN-FAIXA-BONIF     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMAX-FAIXA-BONIF     PIC S9(9)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
