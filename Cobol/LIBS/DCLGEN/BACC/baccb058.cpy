      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_MES_RENTB)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB058))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB058)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_MES_RENTB TABLE
           ( DANO_MES_RENTB                 DECIMAL(6, 0) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CFORMA_PGTO_CATAO              CHAR(1) NOT NULL,
             CIND_ORIGE_TRANS               CHAR(1) NOT NULL,
             VCOMPR_VISTA_MES               DECIMAL(17, 2) NOT NULL,
             VCOMPR_PRZ_MES                 DECIMAL(17, 2) NOT NULL,
             VENCAR_SAQUE_CATAO             DECIMAL(17, 2) NOT NULL,
             VSDO_ROTVO_FINCD               DECIMAL(17, 2) NOT NULL,
             VSDO_PCELD_FINCD               DECIMAL(17, 2) NOT NULL,
             VPRINC_SAQUE_MES               DECIMAL(17, 2) NOT NULL,
             VANUDD_MES                     DECIMAL(17, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_MES_RENTB              *
      ******************************************************************
       01  BACCB058.
      *    *************************************************************
           10 DANO-MES-RENTB       PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-PGTO-CATAO    PIC X(1).
      *    *************************************************************
           10 CIND-ORIGE-TRANS     PIC X(1).
      *    *************************************************************
           10 VCOMPR-VISTA-MES     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCOMPR-PRZ-MES       PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VENCAR-SAQUE-CATAO   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-ROTVO-FINCD     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-PCELD-FINCD     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPRINC-SAQUE-MES     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VANUDD-MES           PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
