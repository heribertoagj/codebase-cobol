      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMANUT_MOTVO_ENCRR_BCO)                    *
      *        LIBRARY(AD.DB2.DCLGEN(CTECB025))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CTECB025)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMANUT_MOTVO_ENCRR_BCO TABLE
           ( HINCL_REG                      TIMESTAMP NOT NULL,
             CMOTVO_ENCRR_CTA               DECIMAL(3, 0) NOT NULL,
             CINDCD_TPO_MANUT               CHAR(1) NOT NULL,
             RMOTVO_ENCRR_CTA               CHAR(70) NOT NULL,
             CTPO_ENCRR_CTA                 DECIMAL(2, 0),
             CTPO_CONTR_NEGOC               DECIMAL(3, 0),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CEMIS_CARTA_ENCRR              CHAR(1),
             RMSGEM_MOTVO_ENCRR             VARCHAR(256),
             CINDCD_ENCRR                   CHAR(1),
             QDIA_AGNDA_PERM                DECIMAL(3, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMANUT_MOTVO_ENCRR_BCO      *
      ******************************************************************
       01  CTECB025.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CMOTVO-ENCRR-CTA     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TPO-MANUT     PIC X(1).
      *    *************************************************************
           10 RMOTVO-ENCRR-CTA     PIC X(70).
      *    *************************************************************
           10 CTPO-ENCRR-CTA       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CEMIS-CARTA-ENCRR    PIC X(1).
      *    *************************************************************
           10 RMSGEM-MOTVO-ENCRR.
              49 RMSGEM-MOTVO-ENCRR-LEN
                 PIC S9(4) USAGE COMP-5.
              49 RMSGEM-MOTVO-ENCRR-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CINDCD-ENCRR         PIC X(1).
      *    *************************************************************
           10 QDIA-AGNDA-PERM      PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
