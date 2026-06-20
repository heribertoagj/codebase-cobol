      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MOTVO_DEVLC_CHEQ)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CHSFB026))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CHSFB026)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MOTVO_DEVLC_CHEQ TABLE
           ( CTPO_DEVLC_CHEQ                DECIMAL(3, 0) NOT NULL,
             CINDCD_TARIF_CCF               DECIMAL(5, 0),
             CINDCD_TARIF_DEVOL             DECIMAL(5, 0),
             CINDCD_TX_CCF                  DECIMAL(5, 0),
             CINDCD_TX_DEVOL                DECIMAL(5, 0),
             CIDTFD_MOTVO_SFUND             CHAR(1) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_MOTVO              DATE NOT NULL,
             DFNAL_VGCIA_MOTVO              DATE,
             DULT_ATULZ_MOTVO               DATE NOT NULL,
             RCOPLT_MOTVO_DEVLC             CHAR(180) NOT NULL,
             RRSUMO_MOTVO_DEVLC             CHAR(20) NOT NULL,
             CTPO_DEVLC_TRFMD               DECIMAL(3, 0) NOT NULL,
             DINCL_CONVS_MOTVO              DATE,
             CINDCD_REAP_CHEQ               CHAR(1),
             CCRITC_MOTVO_DEVLC             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MOTVO_DEVLC_CHEQ            *
      ******************************************************************
       01  CHSFB026.
      *    *************************************************************
           10 CTPO-DEVLC-CHEQ      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TARIF-CCF     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TARIF-DEVOL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TX-CCF        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TX-DEVOL      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-MOTVO-SFUND   PIC X(1).
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-MOTVO    PIC X(10).
      *    *************************************************************
           10 DFNAL-VGCIA-MOTVO    PIC X(10).
      *    *************************************************************
           10 DULT-ATULZ-MOTVO     PIC X(10).
      *    *************************************************************
           10 RCOPLT-MOTVO-DEVLC   PIC X(180).
      *    *************************************************************
           10 RRSUMO-MOTVO-DEVLC   PIC X(20).
      *    *************************************************************
           10 CTPO-DEVLC-TRFMD     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINCL-CONVS-MOTVO    PIC X(10).
      *    *************************************************************
           10 CINDCD-REAP-CHEQ     PIC X(1).
      *    *************************************************************
           10 CCRITC-MOTVO-DEVLC   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
