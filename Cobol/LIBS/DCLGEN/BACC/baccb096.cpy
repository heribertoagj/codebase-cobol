      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TFRIO_CARGA_AGNDA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB096))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB096)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TFRIO_CARGA_AGNDA TABLE
           ( DCARGA_AGNDA_FINCR             DATE NOT NULL,
             CPSSOA_JURID                   DECIMAL(10, 0) NOT NULL,
             NSEQ_UND_ORGNZ                 DECIMAL(8, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CIDTFD_PRODT_CATAO             DECIMAL(5, 0) NOT NULL,
             DMOVTO                         DATE NOT NULL,
             CIDTFD_INDCD                   DECIMAL(5, 0) NOT NULL,
             VMOVTO_CREDT_CATAO             DECIMAL(15, 2) NOT NULL,
             VMOVTO_DEB_CATAO               DECIMAL(15, 2) NOT NULL,
             VBLOQ_GARNT_CONTR              DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TFRIO_CARGA_AGNDA           *
      ******************************************************************
       01  BACCB096.
      *    *************************************************************
           10 DCARGA-AGNDA-FINCR   PIC X(10).
      *    *************************************************************
           10 CPSSOA-JURID         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-UND-ORGNZ       PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-CATAO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 CIDTFD-INDCD         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VMOVTO-CREDT-CATAO   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMOVTO-DEB-CATAO     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VBLOQ-GARNT-CONTR    PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
