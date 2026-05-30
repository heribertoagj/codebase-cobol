      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SOLTC_ENVIO_CAD)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB090))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB090)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SOLTC_ENVIO_CAD TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCUSTM_KEY_SOLTD               DECIMAL(17, 0) NOT NULL,
             CTPO_ENVIO                     DECIMAL(1, 0) NOT NULL,
             DSOLTC_ENVIO                   DATE NOT NULL,
             CSTTUS_REG                     CHAR(1) NOT NULL,
             TPERDC_ENVIO                   CHAR(1),
             CTPO_SAIDA                     DECIMAL(2, 0) NOT NULL,
             CINDCD_INFO_CATAO              CHAR(1) NOT NULL,
             CEMPR_TEB                      DECIMAL(8, 0),
             DPROCM_SOLTC                   DATE,
             CUSUAR_INCL                    CHAR(8) NOT NULL,
             DULT_ALT                       DATE,
             CULT_ALT_CATAO                 CHAR(8)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SOLTC_ENVIO_CAD             *
      ******************************************************************
       01  BACCB090.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCUSTM-KEY-SOLTD     PIC S9(17)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ENVIO           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DSOLTC-ENVIO         PIC X(10).
      *    *************************************************************
           10 CSTTUS-REG           PIC X(1).
      *    *************************************************************
           10 TPERDC-ENVIO         PIC X(1).
      *    *************************************************************
           10 CTPO-SAIDA           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-INFO-CATAO    PIC X(1).
      *    *************************************************************
           10 CEMPR-TEB            PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 DPROCM-SOLTC         PIC X(10).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(8).
      *    *************************************************************
           10 DULT-ALT             PIC X(10).
      *    *************************************************************
           10 CULT-ALT-CATAO       PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
