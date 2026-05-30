      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SEQ_TAXA_RURAL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB051))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB051)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SEQ_TAXA_RURAL TABLE
           ( CSEQ_TX_RURAL                  DECIMAL(5, 0) NOT NULL,
             CMAX_FNALD_RURAL               DECIMAL(3, 0) NOT NULL,
             CMIN_FNALD_RURAL               DECIMAL(3, 0) NOT NULL,
             CMAX_ORIGE_REC                 DECIMAL(3, 0) NOT NULL,
             CMIN_ORIGE_REC                 DECIMAL(3, 0) NOT NULL,
             CMAX_PRODT_RURAL               DECIMAL(5, 0) NOT NULL,
             CMIN_PRODT_RURAL               DECIMAL(5, 0) NOT NULL,
             CMAX_TPO_PROTR                 DECIMAL(3, 0) NOT NULL,
             CMIN_TPO_PROTR                 DECIMAL(3, 0) NOT NULL,
             CFORMA_RURAL_CM                DECIMAL(3, 0),
             PUTILZ_INDCD_DESC              DECIMAL(5, 2),
             CFORMA_RURAL_JURO              DECIMAL(3, 0) NOT NULL,
             PJURO                          DECIMAL(5, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SEQ_TAXA_RURAL              *
      ******************************************************************
       01  RURCB051.
      *    *************************************************************
           10 CSEQ-TX-RURAL        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CMAX-FNALD-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMIN-FNALD-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMAX-ORIGE-REC       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMIN-ORIGE-REC       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMAX-PRODT-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CMIN-PRODT-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CMAX-TPO-PROTR       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMIN-TPO-PROTR       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-RURAL-CM      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 PUTILZ-INDCD-DESC    PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-RURAL-JURO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 PJURO                PIC S9(3)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
