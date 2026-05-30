      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMOVTO_TARIF_DIA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0H6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0H6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMOVTO_TARIF_DIA TABLE
           ( CAG_DSTNO_MOVTO                DECIMAL(5, 0) NOT NULL,
             CCTA_DSTNO_MOVTO               DECIMAL(13, 0) NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             DOCOR_EVNTO                    DATE NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             QEVNTO_AGRUP                   DECIMAL(6, 0) NOT NULL,
             VTARIF_CADTR_SIST              DECIMAL(11, 2) NOT NULL,
             VTARIF_MOVTO_LIQ               DECIMAL(12, 2) NOT NULL,
             VTARIF_MOVTO_DEB               DECIMAL(12, 2) NOT NULL,
             VTARIF_RECBR_CLI               DECIMAL(12, 2) NOT NULL,
             VTARIF_ESTRN_CLI               DECIMAL(12, 2) NOT NULL,
             VTARIF_PERDA_CLI               DECIMAL(10, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMOVTO_TARIF_DIA            *
      ******************************************************************
       01  GFCTB0H6.
      *    *************************************************************
           10 CAG-DSTNO-MOVTO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-DSTNO-MOVTO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 DOCOR-EVNTO          PIC X(10).
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QEVNTO-AGRUP         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-CADTR-SIST    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-MOVTO-LIQ     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-MOVTO-DEB     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-RECBR-CLI     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-ESTRN-CLI     PIC S9(10)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-PERDA-CLI     PIC S9(8)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
