*     ******************************************************************
      * DCLGEN TABLE(DB2PRD.TASSOC_SGMTO_CLI)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0H3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0H3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TASSOC_SGMTO_CLI TABLE
           ( CSGMTO_GSTAO_TARIF             DECIMAL(3, 0) NOT NULL,
             DINIC_VGCIA_ASSOC              DATE NOT NULL,
             CSGMTO_INIC_CLI                DECIMAL(3, 0) NOT NULL,
             DFIM_VGCIA_ASSOC               DATE NOT NULL,
             CINDCD_INTVL_ESPEC             DECIMAL(1, 0) NOT NULL,
             CSGMTO_FNAL_CLI                DECIMAL(3, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TASSOC_SGMTO_CLI            *
      ******************************************************************
       01  GFCTB0H3.
      *    *************************************************************
           10 CSGMTO-GSTAO-TARIF   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-ASSOC    PIC X(10).
      *    *************************************************************
           10 CSGMTO-INIC-CLI      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-VGCIA-ASSOC     PIC X(10).
      *    *************************************************************
           10 CINDCD-INTVL-ESPEC   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSGMTO-FNAL-CLI      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************