      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_ENVIO_MANUT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB012))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB012)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_ENVIO_MANUT TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             CCTRL_RQUIS_FINCR              CHAR(20),
             CUNIC_MSGEM                    CHAR(23),
             CREFT_RECOR                    CHAR(11),
             HINCL                          TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_ENVIO_MANUT           *
      ******************************************************************
       01  RUCAB012.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-RQUIS-FINCR    PIC X(20).
      *    *************************************************************
           10 CUNIC-MSGEM          PIC X(23).
      *    *************************************************************
           10 CREFT-RECOR          PIC X(11).
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
