      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPDIDO_CONTS_OPER)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09F))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09F)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPDIDO_CONTS_OPER TABLE
           ( DINCL_CONTS_OPER               DATE NOT NULL,
             NPDIDO_CONTS_OPER              DECIMAL(9, 0) NOT NULL,
             CCRDRA_ESTBL_COML              DECIMAL(4, 0) NOT NULL,
             COPER_EFETU_CIPAG              CHAR(20),
             CID_INCL_AUTRZ                 CHAR(20),
             CIDTFD_CTRL_RQUIS              CHAR(20) NOT NULL,
             CIDTFD_MOTVO_CONTS             CHAR(1) NOT NULL,
             RMOTVO_CONTS_OPER              CHAR(200),
             COPER_CONTS_CIPAG              CHAR(20),
             CSIT_RETOR_CONTS               CHAR(1),
             HENVIO_MSGEM_CIPAG             TIMESTAMP NOT NULL,
             HRETOR_MSGEM_CIPAG             TIMESTAMP,
             CUSUAR_RESP                    CHAR(9) NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPDIDO_CONTS_OPER           *
      ******************************************************************
       01  BVVEB09F.
      *    *************************************************************
           10 DINCL-CONTS-OPER     PIC X(10).
      *    *************************************************************
           10 NPDIDO-CONTS-OPER    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-EFETU-CIPAG    PIC X(20).
      *    *************************************************************
           10 CID-INCL-AUTRZ       PIC X(20).
      *    *************************************************************
           10 CIDTFD-CTRL-RQUIS    PIC X(20).
      *    *************************************************************
           10 CIDTFD-MOTVO-CONTS   PIC X(1).
      *    *************************************************************
           10 RMOTVO-CONTS-OPER    PIC X(200).
      *    *************************************************************
           10 COPER-CONTS-CIPAG    PIC X(20).
      *    *************************************************************
           10 CSIT-RETOR-CONTS     PIC X(1).
      *    *************************************************************
           10 HENVIO-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 HRETOR-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
