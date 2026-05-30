      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMODLD_RURAL)                              *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0B9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0B9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMODLD_RURAL TABLE
           ( CMODLD_RURAL                   DECIMAL(5, 0) NOT NULL,
             CATVDD_RURAL                   DECIMAL(5, 0) NOT NULL,
             CFNALD_RURAL                   DECIMAL(4, 0) NOT NULL,
             CSUB_FNALD                     DECIMAL(3, 0),
             CEMPTO_RURAL                   DECIMAL(6, 0) NOT NULL,
             CUND_MEDD                      DECIMAL(3, 0) NOT NULL,
             CMODLD_LIQDC_RURAL             DECIMAL(5, 0),
             CCTCIA_PROD_PROVV              CHAR(1),
             CUND_MEDD_PROVV                DECIMAL(3, 0) NOT NULL,
             CCTCIA_ITEM_FINAN              DECIMAL(1, 0) NOT NULL,
             CUND_CTCIA_FINAN               DECIMAL(3, 0) NOT NULL,
             CCTCIA_QTD_INTGD               CHAR(1) NOT NULL,
             CMODLD_RURAL_CONAB             DECIMAL(4, 0) NOT NULL,
             CMODLD_RURAL_BACEN             DECIMAL(8, 0) NOT NULL,
             CBACEN_ZONA_AGRIC              DECIMAL(8, 0) NOT NULL,
             CEXIGE_IMOV_BENEF              CHAR(1) NOT NULL,
             CVIST_PRVIO_OBRIG              CHAR(1) NOT NULL,
             CLIQDC_CONTR_ANTER             CHAR(1) NOT NULL,
             IMODLD_RURAL                   CHAR(160) NOT NULL,
             RMODLD_RURAL                   VARCHAR(256),
             PINDIC_CALC_EGF                DECIMAL(6, 4),
             DINIC_VGCIA_MODLD              DATE NOT NULL,
             DFIM_VGCIA_MODLD               DATE,
             CMODLD_SMULA_RURAL             DECIMAL(1, 0) NOT NULL,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMODLD_RURAL                *
      ******************************************************************
       01  CRURB0B9.
      *    *************************************************************
           10 CMODLD-RURAL         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CATVDD-RURAL         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFNALD-RURAL         PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CSUB-FNALD           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPTO-RURAL         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMODLD-LIQDC-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTCIA-PROD-PROVV    PIC X(1).
      *    *************************************************************
           10 CUND-MEDD-PROVV      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTCIA-ITEM-FINAN    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-CTCIA-FINAN     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTCIA-QTD-INTGD     PIC X(1).
      *    *************************************************************
           10 CMODLD-RURAL-CONAB   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CMODLD-RURAL-BACEN   PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CBACEN-ZONA-AGRIC    PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CEXIGE-IMOV-BENEF    PIC X(1).
      *    *************************************************************
           10 CVIST-PRVIO-OBRIG    PIC X(1).
      *    *************************************************************
           10 CLIQDC-CONTR-ANTER   PIC X(1).
      *    *************************************************************
           10 IMODLD-RURAL         PIC X(160).
      *    *************************************************************
           10 RMODLD-RURAL.
              49 RMODLD-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 RMODLD-RURAL-TEXT
                 PIC X(256).
      *    *************************************************************
           10 PINDIC-CALC-EGF      PIC S9(2)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-MODLD    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-MODLD     PIC X(10).
      *    *************************************************************
           10 CMODLD-SMULA-RURAL   PIC S9(1)V USAGE COMP-3.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 29      *
      ******************************************************************
