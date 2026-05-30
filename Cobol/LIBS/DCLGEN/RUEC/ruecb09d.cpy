      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPCELA_ENVIO_RECOR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB09D))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB09D)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPCELA_ENVIO_RECOR TABLE
           ( CCONTR_CREDT_RURAL             DECIMAL(9, 0) NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             NPCELA_CONTR                   DECIMAL(3, 0) NOT NULL,
             DVCTO_PCELA_CONTR              DATE,
             VPCELA_RURAL                   DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPCELA_ENVIO_RECOR          *
      ******************************************************************
       01  RUECB09D.
      *    *************************************************************
           10 CCONTR-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-CONTR         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-PCELA-CONTR    PIC X(10).
      *    *************************************************************
           10 VPCELA-RURAL         PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
