***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPARM_EMIS_EXTRT)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0K8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0K8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPARM_EMIS_EXTRT TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSGMTO_GSTAO_TARIF             DECIMAL(3, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CIDTFD_SIT_PARM                DECIMAL(1, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL,
             QTARIF_EMIS_EXTRT              DECIMAL(3, 0) NOT NULL,
             VRENTB_EMIS_EXTRT              DECIMAL(11, 2) NOT NULL,
             VRENDA_EMIS_EXTRT              DECIMAL(11, 2) NOT NULL,
             CINDCD_CLI_GERC                CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPARM_EMIS_EXTRT            *
      ******************************************************************
       01  GFCTB0K8.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGMTO-GSTAO-TARIF   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CIDTFD-SIT-PARM      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 QTARIF-EMIS-EXTRT    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VRENTB-EMIS-EXTRT    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VRENDA-EMIS-EXTRT    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CLI-GERC      PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
