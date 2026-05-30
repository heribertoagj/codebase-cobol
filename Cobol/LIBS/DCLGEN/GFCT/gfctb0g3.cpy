      ******************************************************************
      * DCLGEN TABLE(DB2PRD.VLR_COMPO_TARIF)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0G3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0G3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.VLR_COMPO_TARIF TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_TARIF              DATE NOT NULL,
             CSEQ_COMPO_TARIF               DECIMAL(2, 0) NOT NULL,
             CMOEDA_BACEN                   DECIMAL(5, 0) NOT NULL,
             RCOMPO_TARIF                   VARCHAR(255) NOT NULL,
             VCOMPO_TARIF                   DECIMAL(11, 2) NOT NULL,
             PSERVC_PTADO                   DECIMAL(7, 4) NOT NULL,
             VMAX_COMPO_TARIF               DECIMAL(11, 2) NOT NULL,
             VMIN_COMPO_TARIF               DECIMAL(11, 2) NOT NULL,
             CMOMEN_CONVS_MOEDA             DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.VLR_COMPO_TARIF             *
      ******************************************************************
       01  GFCTB0G3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-TARIF    PIC X(10).
      *    *************************************************************
           10 CSEQ-COMPO-TARIF     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CMOEDA-BACEN         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RCOMPO-TARIF.
              49 RCOMPO-TARIF-LEN  PIC S9(4) USAGE COMP.
              49 RCOMPO-TARIF-TEXT  PIC X(255).
      *    *************************************************************
           10 VCOMPO-TARIF         PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PSERVC-PTADO         PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 VMAX-COMPO-TARIF     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMIN-COMPO-TARIF     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CMOMEN-CONVS-MOEDA   PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
