      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01INCON_CDULA)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RUECV019))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECV019)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01INCON_CDULA TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCDULA                         DECIMAL(9, 0) NOT NULL,
             CRSUMO_MSGEM_INCON             CHAR(5) NOT NULL,
             RMSGEM_INCON                   CHAR(40) NOT NULL,
             CINDCD_RESP_APROV              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01INCON_CDULA              *
      ******************************************************************
       01  RUECV019.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CRSUMO-MSGEM-INCON   PIC X(5).
      *    *************************************************************
           10 RMSGEM-INCON         PIC X(40).
      *    *************************************************************
           10 CINDCD-RESP-APROV    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
