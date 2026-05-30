***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMSCRA_EXTRT_PCOTE)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0K2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0K2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMSCRA_EXTRT_PCOTE TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DCOMPT_FCHTO_TARIF             DECIMAL(6, 0) NOT NULL,
             NLIN_EXTRT_TARIF               DECIMAL(3, 0) NOT NULL,
             CTPO_COMPO_PCOTE               DECIMAL(1, 0) NOT NULL,
             CCOMPO_PCOTE                   DECIMAL(5, 0) NOT NULL,
             CINDCD_NOME_GRP                CHAR(1) NOT NULL,
             CINDCD_GRP_EXTRT               CHAR(1) NOT NULL,
             HGERAC_MSCRA_EXTRT             TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMSCRA_EXTRT_PCOTE          *
      ******************************************************************
       01  GFCTB0K2.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DCOMPT-FCHTO-TARIF   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 NLIN-EXTRT-TARIF     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-COMPO-PCOTE     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPO-PCOTE         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-NOME-GRP      PIC X(1).
      *    *************************************************************
           10 CINDCD-GRP-EXTRT     PIC X(1).
      *    *************************************************************
           10 HGERAC-MSCRA-EXTRT   PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
