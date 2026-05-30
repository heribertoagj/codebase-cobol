      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPSSOA_ENVIO_RECOR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB09E))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB09E)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPSSOA_ENVIO_RECOR TABLE
           ( CCONTR_CREDT_RURAL             DECIMAL(9, 0) NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             NPSSOA_CONTR                   DECIMAL(3, 0) NOT NULL,
             CTPO_RESP_PSSOA                CHAR(1),
             CTPO_PSSOA                     DECIMAL(1, 0),
             CCNPJ_CPF                      DECIMAL(9, 0),
             CFLIAL_CNPJ                    DECIMAL(5, 0),
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0),
             CCONTR                         DECIMAL(9, 0),
             CSEQ_MATR_RURAL                DECIMAL(3, 0),
             CCNPJ_CPF_ST                   CHAR(9),
             CFLIAL_CNPJ_ST                 CHAR(5),
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPSSOA_ENVIO_RECOR          *
      ******************************************************************
       01  RUECB09E.
      *    *************************************************************
           10 CCONTR-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NPSSOA-CONTR         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-RESP-PSSOA      PIC X(1).
      *    *************************************************************
           10 CTPO-PSSOA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MATR-RURAL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(5).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
