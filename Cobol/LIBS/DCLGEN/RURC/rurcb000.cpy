      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ORIGEM_RECURSO)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ORIGEM_RECURSO TABLE
           ( CORIGE_REC                     DECIMAL(3, 0) NOT NULL,
             IORIGE_REC                     CHAR(40) NOT NULL,
             CDISPN_ORIGE_REC               CHAR(1) NOT NULL,
             DDIA_BASE_LIBRC                DECIMAL(2, 0),
             DDIA_BASE_PGTO                 DECIMAL(2, 0),
             CORIGE_REC_DESC                CHAR(1),
             CCART                          CHAR(3),
             VALCAD_AGRON                   DECIMAL(17, 2) NOT NULL,
             VALCAD_AG                      DECIMAL(17, 2) NOT NULL,
             CINDCD_REC_OBRIG               DECIMAL(1, 0) NOT NULL,
             QCONTR_ORIGE_REC               DECIMAL(5, 0) NOT NULL,
             CINDCD_PSIVO                   CHAR(1) NOT NULL,
             CORIGE_VINC                    DECIMAL(3, 0) NOT NULL,
             DINIC_VGCIA                    DATE,
             CINDCD_ENVIO_BACEN             CHAR(1) NOT NULL,
             CPLANO_REFIN_RURAL             DECIMAL(3, 0) NOT NULL,
             CPROG_RURAL                    CHAR(4) NOT NULL,
             CORIGE_REC_BACEN               DECIMAL(4, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ORIGEM_RECURSO              *
      ******************************************************************
       01  RURCB000.
      *    *************************************************************
           10 CORIGE-REC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IORIGE-REC           PIC X(40).
      *    *************************************************************
           10 CDISPN-ORIGE-REC     PIC X(1).
      *    *************************************************************
           10 DDIA-BASE-LIBRC      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DDIA-BASE-PGTO       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC-DESC      PIC X(1).
      *    *************************************************************
           10 CCART                PIC X(3).
      *    *************************************************************
           10 VALCAD-AGRON         PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VALCAD-AG            PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-REC-OBRIG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QCONTR-ORIGE-REC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PSIVO         PIC X(1).
      *    *************************************************************
           10 CORIGE-VINC          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 CINDCD-ENVIO-BACEN   PIC X(1).
      *    *************************************************************
           10 CPLANO-REFIN-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPROG-RURAL          PIC X(4).
      *    *************************************************************
           10 CORIGE-REC-BACEN     PIC S9(4)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
