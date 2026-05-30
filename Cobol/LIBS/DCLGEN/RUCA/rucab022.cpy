      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOG_ENCAR_CONTR)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB022))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB022)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOG_ENCAR_CONTR TABLE
           ( CCONTR_CREDT_RURAL             DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             PJURO_DEB_BLETO                DECIMAL(5, 2) NOT NULL,
             PJURO_DEB_CTA                  DECIMAL(5, 2) NOT NULL,
             CFORMA_LIQDC                   DECIMAL(2, 0) NOT NULL,
             CINDCD_PRMSS_DEB               CHAR(1) NOT NULL,
             CINDCD_PRMSS_LIM               CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             DINIC_VGCIA                    DATE NOT NULL,
             DFIM_VGCIA                     DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOG_ENCAR_CONTR            *
      ******************************************************************
       01  RUCAB022.
      *    *************************************************************
           10 CCONTR-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 PJURO-DEB-BLETO      PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PJURO-DEB-CTA        PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-LIQDC         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRMSS-DEB     PIC X(1).
      *    *************************************************************
           10 CINDCD-PRMSS-LIM     PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA           PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
