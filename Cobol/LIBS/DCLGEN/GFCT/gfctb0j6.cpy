***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRMSS_ESTRN_TARIF)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0J6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0J6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRMSS_ESTRN_TARIF TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_PRMSS_ESTRN              DATE NOT NULL,
             DFIM_PRMSS_ESTRN               DATE NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_ENCRR_VGCIA              DECIMAL(9, 0),
             HVGCIA_REGRA                   TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRMSS_ESTRN_TARIF          *
      ******************************************************************
       01  GFCTB0J6.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-PRMSS-ESTRN    PIC X(10).
      *    *************************************************************
           10 DFIM-PRMSS-ESTRN     PIC X(10).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-ENCRR-VGCIA    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HVGCIA-REGRA         PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
