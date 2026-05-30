      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTPO_SGMTO_GSTAO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0H5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0H5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTPO_SGMTO_GSTAO TABLE
           ( CSGMTO_GSTAO_TARIF             DECIMAL(3, 0) NOT NULL,
             CTPO_PSSOA                     CHAR(1) NOT NULL,
             ISGMTO_GSTAO_TARIF             CHAR(30) NOT NULL,
             CDEPDC_GSTOR_SGMTO             DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_SGMTO              DATE NOT NULL,
             DFIM_VGCIA_SGMTO               DATE NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTPO_SGMTO_GSTAO            *
      ******************************************************************
       01  GFCTB0H5.
      *    *************************************************************
           10 CSGMTO-GSTAO-TARIF   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PSSOA           PIC X(1).
      *    *************************************************************
           10 ISGMTO-GSTAO-TARIF   PIC X(30).
      *    *************************************************************
           10 CDEPDC-GSTOR-SGMTO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-SGMTO    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-SGMTO     PIC X(10).
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
