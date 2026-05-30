      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSIST_PROD_RURAL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB086))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB086)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSIST_PROD_RURAL TABLE
           ( NEMPTO_RURAL                   DECIMAL(9, 0) NOT NULL,
             NITEM_SIST_PROD                DECIMAL(5, 0) NOT NULL,
             CTPO_AGROP_RURAL               DECIMAL(5, 0) NOT NULL,
             CTPO_INTGC_RURAL               DECIMAL(5, 0) NOT NULL,
             CINDCD_UND_PRDTV               DECIMAL(1, 0) NOT NULL,
             CTPO_IGCAO_RURAL               DECIMAL(5, 0) NOT NULL,
             CTPO_CTIVO_RURAL               DECIMAL(5, 0) NOT NULL,
             CCICLO_PROD_RURAL              DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA                    DATE,
             DFIM_VGCIA                     DATE,
             DULT_ATULZ                     DATE,
             CUSUAR_RESP                    CHAR(9),
             CCLIMT_RURAL_BACEN             DECIMAL(2, 0) NOT NULL,
             COUTRO_TRATO_SOLO              DECIMAL(5, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSIST_PROD_RURAL            *
      ******************************************************************
       01  RURCB086.
      *    *************************************************************
           10 NEMPTO-RURAL         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NITEM-SIST-PROD      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-AGROP-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-INTGC-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-UND-PRDTV     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-IGCAO-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CTIVO-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCICLO-PROD-RURAL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA           PIC X(10).
      *    *************************************************************
           10 DULT-ATULZ           PIC X(10).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      *    *************************************************************
           10 CCLIMT-RURAL-BACEN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 COUTRO-TRATO-SOLO    PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
