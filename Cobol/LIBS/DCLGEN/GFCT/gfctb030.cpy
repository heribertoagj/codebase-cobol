***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CRITR_BONIF_SERVC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB030))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB030)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CRITR_BONIF_SERVC TABLE
           ( CTPO_BONIF                     DECIMAL(3, 0) NOT NULL,
             CCRITR_BONIF                   DECIMAL(3, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CINDCD_TARIF_INDVD             CHAR(1) NOT NULL,
             CINDCD_TARIF_COMBD             CHAR(1) NOT NULL,
             CINDCD_TARIF_PCOTE             CHAR(1) NOT NULL,
             CINDCD_TPO_FAIXA               CHAR(1) NOT NULL,
             CINDCD_DESC_PERC               CHAR(1) NOT NULL,
             CINDCD_DESC_VLR                CHAR(1) NOT NULL,
             CINDCD_DESC_QTD                CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CRITR_BONIF_SERVC           *
      ******************************************************************
       01  GFCTB030.
      *    *************************************************************
           10 CTPO-BONIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCRITR-BONIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CINDCD-TARIF-INDVD   PIC X(1).
      *    *************************************************************
           10 CINDCD-TARIF-COMBD   PIC X(1).
      *    *************************************************************
           10 CINDCD-TARIF-PCOTE   PIC X(1).
      *    *************************************************************
           10 CINDCD-TPO-FAIXA     PIC X(1).
      *    *************************************************************
           10 CINDCD-DESC-PERC     PIC X(1).
      *    *************************************************************
           10 CINDCD-DESC-VLR      PIC X(1).
      *    *************************************************************
           10 CINDCD-DESC-QTD      PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
