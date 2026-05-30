***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ADSAO_GRP_EMPR)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB003))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB003)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ADSAO_GRP_EMPR TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             DINIC_ADSAO_COMP               DATE NOT NULL,
             CAG_EMPR                       DECIMAL(5, 0) NOT NULL,
             CRZ_CTA_EMPR                   DECIMAL(5, 0) NOT NULL,
             CNRO_CTA_DEB                   DECIMAL(13, 0) NOT NULL,
             CLCTO_EMPR                     DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ADSAO_GRP_EMPR              *
      ******************************************************************
       01  GFCTB003.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-ADSAO-COMP     PIC X(10).
      *    *************************************************************
           10 CAG-EMPR             PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CRZ-CTA-EMPR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTA-DEB         PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-EMPR           PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
