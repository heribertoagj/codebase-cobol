      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDETLH_ARQ_DVDOR_TARIF)                    *
      *        LIBRARY(AD.DB2.DCLGEN(DBTOB0DE))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DBTOB0DE)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDETLH_ARQ_DVDOR_TARIF TABLE
           ( NCTRL_ARQ_DVDOR                DECIMAL(5, 0) NOT NULL,
             NDETLH_ARQ_DVDOR               DECIMAL(5, 0) NOT NULL,
             CCTA_COSIF                     DECIMAL(8, 0) NOT NULL,
             CCNPJ_PGDOR_FNAL               DECIMAL(9, 0) NOT NULL,
             CFLIAL_PGDOR_FNAL              DECIMAL(4, 0) NOT NULL,
             CCTRL_PGDOR_FNAL               DECIMAL(2, 0) NOT NULL,
             VSDO_TARIF_ANTER               DECIMAL(15, 2) NOT NULL,
             VAMOTZ_PG_DVDOR                DECIMAL(15, 2) NOT NULL,
             VLIBRC_TARIF_RECBR             DECIMAL(15, 2) NOT NULL,
             VJURO_INC_RECBR                DECIMAL(15, 2) NOT NULL,
             VAJUST_TARIF_RECBR             DECIMAL(15, 2) NOT NULL,
             VSDO_TARIF_ATUAL               DECIMAL(15, 2) NOT NULL,
             RCOMPL_TARIF_DVDOR             CHAR(50) NOT NULL,
             CSERVC_FEBRABAN                DECIMAL(5, 0),
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             RMOTVO_MANUT                   CHAR(40)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDETLH_ARQ_DVDOR_TARIF      *
      ******************************************************************
       01  DBTOB0DE.
      *    *************************************************************
           10 NCTRL-ARQ-DVDOR      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NDETLH-ARQ-DVDOR     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-COSIF           PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-PGDOR-FNAL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-PGDOR-FNAL    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-PGDOR-FNAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VSDO-TARIF-ANTER     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VAMOTZ-PG-DVDOR      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VLIBRC-TARIF-RECBR   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO-INC-RECBR      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VAJUST-TARIF-RECBR   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-TARIF-ATUAL     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 RCOMPL-TARIF-DVDOR   PIC X(50).
      *    *************************************************************
           10 CSERVC-FEBRABAN      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 RMOTVO-MANUT         PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
