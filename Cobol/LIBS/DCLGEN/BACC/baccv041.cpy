      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01MOVTO_CTA_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV041))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV041)                                     *
      *        APOST                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01MOVTO_CTA_CATAO TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CIDTFD_PRODT_CATAO             DECIMAL(5, 0) NOT NULL,
             CIDTFD_INDCD                   DECIMAL(5, 0) NOT NULL,
             DMOVTO                         DATE NOT NULL,
             VMOVTO_CREDT_CATAO             DECIMAL(15, 2) NOT NULL,
             VMOVTO_DEB_CATAO               DECIMAL(15, 2) NOT NULL,
             VBLOQ_GARNT_CONTR              DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01MOVTO_CTA_CATAO          *
      ******************************************************************
       01  BACCV041.
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
           10 CIDTFD-PRODT-CATAO   PIC S9(5)V USAGE COMP-3.
           10 CIDTFD-INDCD         PIC S9(5)V USAGE COMP-3.
           10 DMOVTO               PIC X(10).
           10 VMOVTO-CREDT-CATAO   PIC S9(13)V9(2) USAGE COMP-3.
           10 VMOVTO-DEB-CATAO     PIC S9(13)V9(2) USAGE COMP-3.
           10 VBLOQ-GARNT-CONTR    PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
