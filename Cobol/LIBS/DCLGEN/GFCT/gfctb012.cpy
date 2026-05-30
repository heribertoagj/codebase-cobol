***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.AGNDA_GRP_EMPR)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB012))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB012)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.AGNDA_GRP_EMPR TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_FLEXZ_CTA                 DECIMAL(2, 0) NOT NULL,
             CAG_EMPR                       DECIMAL(5, 0) NOT NULL,
             CRZ_CTA_EMPR                   DECIMAL(5, 0) NOT NULL,
             CNRO_CTA_DEB                   DECIMAL(13, 0) NOT NULL,
             CLCTO_CTA_DEB                  DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.AGNDA_GRP_EMPR              *
      ******************************************************************
       01  GFCTB012.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-FLEXZ-CTA       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-EMPR             PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CRZ-CTA-EMPR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTA-DEB         PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-CTA-DEB        PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
