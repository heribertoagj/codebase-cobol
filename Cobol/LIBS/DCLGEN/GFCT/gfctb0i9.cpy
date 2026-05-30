***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRMSS_FLEXZ_CTA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0I9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0I9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRMSS_FLEXZ_CTA TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_PRMSS_FLEXZ              DATE NOT NULL,
             DFIM_PRMSS_FLEXZ               DATE NOT NULL,
             CINDCD_PRMSS_AGNDA             DECIMAL(1, 0) NOT NULL,
             CINDCD_PRMSS_VLR               DECIMAL(1, 0) NOT NULL,
             CINDCD_PRMSS_PERC              DECIMAL(1, 0) NOT NULL,
             CINDCD_PRMSS_FRANQ             DECIMAL(1, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_ENCRR_VGCIA              DECIMAL(9, 0) NOT NULL,
             HVGCIA_REGRA                   TIMESTAMP NOT NULL,
             CINDCD_PRMSS_PERDC             DECIMAL(1, 0) NOT NULL,
             CINDCD_FLEXZ_LOTE              CHAR(1) NOT NULL,
             CINDCD_VOLUM_MES               CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRMSS_FLEXZ_CTA            *
      ******************************************************************
       01  GFCTB0I9.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-PRMSS-FLEXZ    PIC X(10).
      *    *************************************************************
           10 DFIM-PRMSS-FLEXZ     PIC X(10).
      *    *************************************************************
           10 CINDCD-PRMSS-AGNDA   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRMSS-VLR     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRMSS-PERC    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRMSS-FRANQ   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-ENCRR-VGCIA    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HVGCIA-REGRA         PIC X(26).
      *    *************************************************************
           10 CINDCD-PRMSS-PERDC   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-FLEXZ-LOTE    PIC X(1).
      *    *************************************************************
           10 CINDCD-VOLUM-MES     PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
