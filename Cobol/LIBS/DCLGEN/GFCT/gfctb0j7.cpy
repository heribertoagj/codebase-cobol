***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRMSS_FLEXZ_AGPTO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0J7))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0J7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRMSS_FLEXZ_AGPTO TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_PRMSS_FLEXZ              DATE NOT NULL,
             DFIM_PRMSS_FLEXZ               DATE NOT NULL,
             CINDCD_PRMSS_AGNDA             DECIMAL(1, 0) NOT NULL,
             CINDCD_PRMSS_VLR               DECIMAL(1, 0) NOT NULL,
             CINDCD_PRMSS_PERC              DECIMAL(1, 0) NOT NULL,
             CINDCD_PRMSS_FRANQ             DECIMAL(1, 0) NOT NULL,
             CINDCD_PRMSS_PERDC             DECIMAL(1, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_ENCRR_VGCIA              DECIMAL(9, 0),
             HVGCIA_REGRA                   TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRMSS_FLEXZ_AGPTO          *
      ******************************************************************
       01  GFCTB0J7.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
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
           10 CINDCD-PRMSS-PERDC   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-ENCRR-VGCIA    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HVGCIA-REGRA         PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
