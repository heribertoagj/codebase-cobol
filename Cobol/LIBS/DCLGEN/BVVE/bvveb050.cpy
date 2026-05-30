      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCLI_CTA_DOMCL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB050))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB050)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCLI_CTA_DOMCL TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             CBCO_VINC                      DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA_VINC                 DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_VINC                DECIMAL(13, 0) NOT NULL,
             CUSUAR_MANUT                   CHAR(7) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCLI_CTA_DOMCL              *
      ******************************************************************
       01  BVVEB050.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-VINC            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA-VINC       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-VINC      PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
