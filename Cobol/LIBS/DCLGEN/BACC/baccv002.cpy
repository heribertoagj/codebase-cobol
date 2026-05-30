      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01AVISO_PTDOR)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV002))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV002)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01AVISO_PTDOR TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CSEQ_MSGEM_PTDOR               CHAR(2) NOT NULL,
             DVALDD_INIC_MSGEM              DATE NOT NULL,
             DVALDD_FNAL_MSGEM              DATE NOT NULL,
             RMSGEM_ENVIO_PTDOR             CHAR(90) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01AVISO_PTDOR              *
      ******************************************************************
       01  BACCV002.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MSGEM-PTDOR     PIC X(2).
      *    *************************************************************
           10 DVALDD-INIC-MSGEM    PIC X(10).
      *    *************************************************************
           10 DVALDD-FNAL-MSGEM    PIC X(10).
      *    *************************************************************
           10 RMSGEM-ENVIO-PTDOR   PIC X(90).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
