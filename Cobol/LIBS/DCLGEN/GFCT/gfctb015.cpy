***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.AGNDA_GRP_SGMTO)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB015))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB015)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.AGNDA_GRP_SGMTO TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_FLEXZ_CTA                 DECIMAL(2, 0) NOT NULL,
             CSGMTO_CLI                     DECIMAL(3, 0) NOT NULL,
             CTPO_PSSOA                     CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.AGNDA_GRP_SGMTO             *
      ******************************************************************
       01  GFCTB015.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-FLEXZ-CTA       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PSSOA           PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
