***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ADSAO_GRP_SGMTO)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB007))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB007)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ADSAO_GRP_SGMTO TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             DINIC_ADSAO_COMP               DATE NOT NULL,
             CSGMTO_GSTAO_TARIF             DECIMAL(3, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ADSAO_GRP_SGMTO             *
      ******************************************************************
       01  GFCTB007.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-ADSAO-COMP     PIC X(10).
      *    *************************************************************
           10 CSGMTO-GSTAO-TARIF   PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
