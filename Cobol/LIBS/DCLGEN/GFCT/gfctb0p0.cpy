***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPOSIC_SDO_APLIC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0P0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0P0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPOSIC_SDO_APLIC TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             DANO_MES_SDO                   DECIMAL(6, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             VSDO_MED_FUNDO                 DECIMAL(17, 2) NOT NULL,
             VSDO_MED_POUP                  DECIMAL(17, 2) NOT NULL,
             VSDO_MED_CERTF                 DECIMAL(17, 2) NOT NULL,
             VSDO_MED_PREVD                 DECIMAL(17, 2) NOT NULL,
             VSDO_APLIC_ACOES               DECIMAL(17, 2) NOT NULL,
             VSDO_APLIC_TSORO               DECIMAL(17, 2) NOT NULL,
             VSDO_APLIC_DBTRS               DECIMAL(17, 2) NOT NULL,
             VSDO_APLIC_DERIV               DECIMAL(17, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPOSIC_SDO_APLIC            *
      ******************************************************************
       01  GFCTB0P0.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-MES-SDO         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VSDO-MED-FUNDO       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-MED-POUP        PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-MED-CERTF       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-MED-PREVD       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-APLIC-ACOES     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-APLIC-TSORO     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-APLIC-DBTRS     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-APLIC-DERIV     PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
