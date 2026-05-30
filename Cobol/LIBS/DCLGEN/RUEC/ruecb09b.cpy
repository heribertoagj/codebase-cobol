      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THIST_ENVIO_RECOR)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB09B))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB09B)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THIST_ENVIO_RECOR TABLE
           ( CCONTR_CREDT_RURAL             DECIMAL(9, 0) NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CMSGEM_RECOR                   CHAR(9),
             HINCL                          TIMESTAMP,
             CCTRL_RQUIS_FINCR              CHAR(20),
             CUNIC_MSGEM                    CHAR(23),
             CREFT_RECOR                    CHAR(11),
             CSIT_MSGEM                     DECIMAL(1, 0),
             CUSUAR_RESP                    CHAR(9)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THIST_ENVIO_RECOR           *
      ******************************************************************
       01  RUECB09B.
      *    *************************************************************
           10 CCONTR-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CMSGEM-RECOR         PIC X(9).
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CCTRL-RQUIS-FINCR    PIC X(20).
      *    *************************************************************
           10 CUNIC-MSGEM          PIC X(23).
      *    *************************************************************
           10 CREFT-RECOR          PIC X(11).
      *    *************************************************************
           10 CSIT-MSGEM           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
