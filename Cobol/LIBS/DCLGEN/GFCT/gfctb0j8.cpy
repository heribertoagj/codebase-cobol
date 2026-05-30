***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TUTILZ_COMPO_PCOTE)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0J8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0J8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TUTILZ_COMPO_PCOTE TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             DCOMPT_FCHTO_TARIF             DECIMAL(6, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CTPO_COMPO_PCOTE               DECIMAL(1, 0) NOT NULL,
             CCOMPO_PCOTE                   DECIMAL(5, 0) NOT NULL,
             CSIT_PER_UTILZ                 CHAR(1) NOT NULL,
             QUTILZ_CONTT_COMPO             DECIMAL(7, 0) NOT NULL,
             QUTILZ_EXCED_COMPO             DECIMAL(7, 0) NOT NULL,
             VUNTAR_EXCED_CONTT             DECIMAL(12, 2) NOT NULL,
             VINIC_EXCED_COMPO              DECIMAL(12, 2) NOT NULL,
             VFNAL_EXCED_COMPO              DECIMAL(12, 2) NOT NULL,
             QUTILZ_COMPO_TARIF             DECIMAL(7, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TUTILZ_COMPO_PCOTE          *
      ******************************************************************
       01  GFCTB0J8.
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
           10 CSIT-PER-UTILZ       PIC X(1).
      *    *************************************************************
           10 QUTILZ-CONTT-COMPO   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 QUTILZ-EXCED-COMPO   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 VUNTAR-EXCED-CONTT   PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VINIC-EXCED-COMPO    PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VFNAL-EXCED-COMPO    PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QUTILZ-COMPO-TARIF   PIC S9(7)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
