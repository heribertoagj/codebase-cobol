***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAUTRZ_ENVIO_SGMTO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0L7))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0L7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAUTRZ_ENVIO_SGMTO TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CINDCD_AUTRZ_EMIS              DECIMAL(1, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CSGMTO_INIC_CLI                DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAUTRZ_ENVIO_SGMTO          *
      ******************************************************************
       01  GFCTB0L7.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AUTRZ-EMIS    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CSGMTO-INIC-CLI      PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
