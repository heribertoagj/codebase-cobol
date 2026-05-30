***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TUTILZ_COMPO_SERVC)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0J9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0J9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TUTILZ_COMPO_SERVC TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             DCOMPT_FCHTO_TARIF             DECIMAL(6, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CTPO_COMPO_PCOTE               DECIMAL(1, 0) NOT NULL,
             CCOMPO_PCOTE                   DECIMAL(5, 0) NOT NULL,
             CTPO_COMPO_SERVC               DECIMAL(1, 0) NOT NULL,
             CCOMPO_GRP_SERVC               DECIMAL(5, 0) NOT NULL,
             QREST_UTILZ_TARIF              DECIMAL(3, 0) NOT NULL,
             QUTILZ_FNAL_COMPO              DECIMAL(7, 0) NOT NULL,
             QUTILZ_EXCED_COMPO             DECIMAL(7, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TUTILZ_COMPO_SERVC          *
      ******************************************************************
       01  GFCTB0J9.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DCOMPT-FCHTO-TARIF   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-COMPO-PCOTE     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPO-PCOTE         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-COMPO-SERVC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPO-GRP-SERVC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QREST-UTILZ-TARIF    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QUTILZ-FNAL-COMPO    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QUTILZ-EXCED-COMPO   PIC S9(7)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
