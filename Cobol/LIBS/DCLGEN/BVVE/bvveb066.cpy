      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TERRO_MSGEM_CATAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB066))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB066)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TERRO_MSGEM_CATAO TABLE
           ( CERRO_MSGEM_SIST               CHAR(8) NOT NULL,
             HATIVO_ERRO_MSGEM              TIMESTAMP NOT NULL,
             HDSATV_ERRO_MSGEM              TIMESTAMP,
             RERRO_MSGEM_SIST               CHAR(80) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TERRO_MSGEM_CATAO           *
      ******************************************************************
       01  BVVEB066.
      *    *************************************************************
           10 CERRO-MSGEM-SIST     PIC X(8).
      *    *************************************************************
           10 HATIVO-ERRO-MSGEM    PIC X(26).
      *    *************************************************************
           10 HDSATV-ERRO-MSGEM    PIC X(26).
      *    *************************************************************
           10 RERRO-MSGEM-SIST     PIC X(80).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
