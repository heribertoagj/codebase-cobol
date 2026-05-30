      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSIST_PROD_BACEN)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0T2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0T2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSIST_PROD_BACEN TABLE
           ( CEMPTO_RURAL_BACEN             DECIMAL(9, 0) NOT NULL,
             CITEM_PROD_BACEN               DECIMAL(5, 0) NOT NULL,
             CAGROP_RURAL_BACEN             DECIMAL(5, 0) NOT NULL,
             CINTGC_RURAL_BACEN             DECIMAL(5, 0) NOT NULL,
             CIGCAO_RURAL_BACEN             DECIMAL(5, 0) NOT NULL,
             CCULTR_RURAL_BACEN             DECIMAL(5, 0) NOT NULL,
             CCICLO_PROD_BACEN              DECIMAL(5, 0) NOT NULL,
             CTPO_CULTR_BACEN               DECIMAL(2, 0) NOT NULL,
             DINIC_VGCIA_REG                DATE NOT NULL,
             DFIM_VGCIA_REG                 DATE,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CCLIMT_RURAL_BACEN             DECIMAL(2, 0) NOT NULL,
             COUTRO_TRATO_SOLO              DECIMAL(5, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSIST_PROD_BACEN            *
      ******************************************************************
       01  CRURB0T2.
      *    *************************************************************
           10 CEMPTO-RURAL-BACEN   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CITEM-PROD-BACEN     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGROP-RURAL-BACEN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINTGC-RURAL-BACEN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIGCAO-RURAL-BACEN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCULTR-RURAL-BACEN   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCICLO-PROD-BACEN    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CULTR-BACEN     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-REG      PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-REG       PIC X(10).
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CCLIMT-RURAL-BACEN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 COUTRO-TRATO-SOLO    PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
