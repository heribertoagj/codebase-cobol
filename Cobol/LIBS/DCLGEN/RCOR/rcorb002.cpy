      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TWORKF_MSGEM_CREDT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RCORB002))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RCORB002)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TWORKF_MSGEM_CREDT TABLE
           ( CMSGEM_PROCS_CREDT             CHAR(9) NOT NULL,
             CCTRL_MSGEM_CREDT              DECIMAL(13, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             NMSGEM_PROCS_CREDT             DECIMAL(3, 0) NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0) NOT NULL,
             CIDTFD_ENVIO_MSGEM             CHAR(1) NOT NULL,
             WMSGEM_PROCS_CREDT             VARCHAR(32500) NOT NULL,
             CTPO_ERRO_MSGEM                DECIMAL(1, 0),
             RERRO_MSGEM_CREDT              CHAR(80)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TWORKF_MSGEM_CREDT          *
      ******************************************************************
       01  RCORB002.
      *    *************************************************************
           10 CMSGEM-PROCS-CREDT   PIC X(9).
      *    *************************************************************
           10 CCTRL-MSGEM-CREDT    PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 NMSGEM-PROCS-CREDT   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-ENVIO-MSGEM   PIC X(1).
      *    *************************************************************
           10 WMSGEM-PROCS-CREDT.
              49 WMSGEM-PROCS-CREDT-LEN
                 PIC S9(4) USAGE COMP.
              49 WMSGEM-PROCS-CREDT-TEXT
                 PIC X(32500).
      *    *************************************************************
           10 CTPO-ERRO-MSGEM      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RERRO-MSGEM-CREDT    PIC X(80).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
