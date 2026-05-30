      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SDO_EXTR_FAT_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB064))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB064)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SDO_EXTR_FAT_CATAO TABLE
           ( CCATAO_CREDT                   DECIMAL(17, 0) NOT NULL,
             ISIT_CICLO                     CHAR(1) NOT NULL,
             IMOEDA                         DECIMAL(3, 0) NOT NULL,
             DDIA_DEB                       DECIMAL(2, 0) NOT NULL,
             VTOT_CATAO_CREDT               DECIMAL(15, 2) NOT NULL,
             VPGTO_MINIMO                   DECIMAL(15, 2) NOT NULL,
             CCUST_KEY                      CHAR(19) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SDO_EXTR_FAT_CATAO          *
      ******************************************************************
       01  BACCB064.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(17)V USAGE COMP-3.
      *    *************************************************************
           10 ISIT-CICLO           PIC X(1).
      *    *************************************************************
           10 IMOEDA               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DDIA-DEB             PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VTOT-CATAO-CREDT     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPGTO-MINIMO         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCUST-KEY            PIC X(19).
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
