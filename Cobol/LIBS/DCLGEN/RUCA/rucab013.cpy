      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TENVIO_RETOR_MANUT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB013))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB013)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TENVIO_RETOR_MANUT TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             NREG_MSGEM_RECOR               DECIMAL(5, 0) NOT NULL,
             CTPO_CONTD_MSGEM               DECIMAL(1, 0) NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0) NOT NULL,
             WCONTD_MSGEM_RURAL             VARCHAR(32500) NOT NULL,
             HMOVTO_MSGEM_RECOR             TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TENVIO_RETOR_MANUT          *
      ******************************************************************
       01  RUCAB013.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NREG-MSGEM-RECOR     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTD-MSGEM     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 WCONTD-MSGEM-RURAL.
              49 WCONTD-MSGEM-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 WCONTD-MSGEM-RURAL-TEXT
                 PIC X(32500).
      *    *************************************************************
           10 HMOVTO-MSGEM-RECOR   PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
