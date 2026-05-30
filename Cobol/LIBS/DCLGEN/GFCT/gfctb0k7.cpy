***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THOMOL_EXTRT_TARIF)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0K7))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0K7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THOMOL_EXTRT_TARIF TABLE
           ( DANO_REFT_EXTRT                DECIMAL(4, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CINDCD_ANO_COPLT               CHAR(1) NOT NULL,
             CIDTFD_HOMOL_PRVIO             DECIMAL(1, 0) NOT NULL,
             CFUNC_HOMOL_PRVIO              DECIMAL(9, 0) NOT NULL,
             HHOMOL_PRVIO_EXTRT             TIMESTAMP NOT NULL,
             CIDTFD_HOMOL_EMIS              DECIMAL(1, 0) NOT NULL,
             CFUNC_HOMOL_EMIS               DECIMAL(9, 0) NOT NULL,
             HHOMOL_EMIS_EXTRT              TIMESTAMP NOT NULL,
             RJUSTF_HOMOL_PRVIO             VARCHAR(256) NOT NULL,
             RJUSTF_HOMOL_EMIS              VARCHAR(256) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THOMOL_EXTRT_TARIF          *
      ******************************************************************
       01  GFCTB0K7.
      *    *************************************************************
           10 DANO-REFT-EXTRT      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CINDCD-ANO-COPLT     PIC X(1).
      *    *************************************************************
           10 CIDTFD-HOMOL-PRVIO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-HOMOL-PRVIO    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HHOMOL-PRVIO-EXTRT   PIC X(26).
      *    *************************************************************
           10 CIDTFD-HOMOL-EMIS    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-HOMOL-EMIS     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HHOMOL-EMIS-EXTRT    PIC X(26).
      *    *************************************************************
           10 RJUSTF-HOMOL-PRVIO.
              49 RJUSTF-HOMOL-PRVIO-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-HOMOL-PRVIO-TEXT
                 PIC X(256).
      *    *************************************************************
           10 RJUSTF-HOMOL-EMIS.
              49 RJUSTF-HOMOL-EMIS-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-HOMOL-EMIS-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
