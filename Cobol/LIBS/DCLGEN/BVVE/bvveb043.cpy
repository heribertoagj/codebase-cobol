      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TVLR_COMPM_OPER_CREDT)                     *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB043))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB043)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TVLR_COMPM_OPER_CREDT TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             VMOVTO_DEB_CATAO               DECIMAL(15, 2),
             VMOVTO_GARNT_CATAO             DECIMAL(15, 2),
             VGARNT_RCBVL_MARCD             DECIMAL(15, 2),
             VAGNDA_FINCR_BANDE             DECIMAL(15, 2),
             HATULZ                         TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TVLR_COMPM_OPER_CREDT       *
      ******************************************************************
       01  BVVEB043.
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
           10 VMOVTO-DEB-CATAO     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMOVTO-GARNT-CATAO   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VGARNT-RCBVL-MARCD   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VAGNDA-FINCR-BANDE   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 HATULZ               PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
