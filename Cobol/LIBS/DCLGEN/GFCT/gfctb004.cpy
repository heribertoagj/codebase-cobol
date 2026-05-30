***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ADSAO_GRP_MUN)                             *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB004))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB004)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ADSAO_GRP_MUN TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             DINIC_ADSAO_COMP               DATE NOT NULL,
             CMUN_IBGE                      DECIMAL(7, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ADSAO_GRP_MUN               *
      ******************************************************************
       01  GFCTB004.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-ADSAO-COMP     PIC X(10).
      *    *************************************************************
           10 CMUN-IBGE            PIC S9(7)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
