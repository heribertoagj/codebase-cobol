      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TROTA_MSGRA_CATAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB097))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB097)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TROTA_MSGRA_CATAO TABLE
           ( NROTA_MSGRA                    DECIMAL(9, 0) NOT NULL,
             CMSGRA_SIST                    DECIMAL(3, 0),
             CDOMNO_MSGRA_SIST              CHAR(5),
             NESTRT_INFO_BACEN              DECIMAL(3, 0),
             CSIST_EXTER                    CHAR(3),
             CEVNTO_MSGEM                   CHAR(4),
             CFLUXO_MSGEM                   CHAR(2),
             CIDTFD_FLUXO_MSGEM             DECIMAL(1, 0),
             CINDCD_TPO_TRATO               DECIMAL(1, 0) NOT NULL,
             IARQ_ROTA_MSGRA                CHAR(10)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TROTA_MSGRA_CATAO           *
      ******************************************************************
       01  BVVEB097.
      *    *************************************************************
           10 NROTA-MSGRA          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CMSGRA-SIST          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDOMNO-MSGRA-SIST    PIC X(5).
      *    *************************************************************
           10 NESTRT-INFO-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSIST-EXTER          PIC X(3).
      *    *************************************************************
           10 CEVNTO-MSGEM         PIC X(4).
      *    *************************************************************
           10 CFLUXO-MSGEM         PIC X(2).
      *    *************************************************************
           10 CIDTFD-FLUXO-MSGEM   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-TPO-TRATO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IARQ-ROTA-MSGRA      PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
