***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCOMPS_PCOTE_PERSO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0O2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0O2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCOMPS_PCOTE_PERSO TABLE
           ( CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CTARIF_PCOTE                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_COMPS              DATE NOT NULL,
             CSEQ_COMPO_PCOTE               DECIMAL(3, 0) NOT NULL,
             QFRANQ_COMPO_PERSO             DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCOMPS_PCOTE_PERSO          *
      ******************************************************************
       01  GFCTB0O2.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CTARIF-PCOTE         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-COMPS    PIC X(10).
      *    *************************************************************
           10 CSEQ-COMPO-PCOTE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QFRANQ-COMPO-PERSO   PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
