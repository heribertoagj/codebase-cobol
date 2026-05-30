***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TVALDC_PRVIO_EXTRT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0K9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0K9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TVALDC_PRVIO_EXTRT TABLE
           ( CIDTFD_FUNCL_PRVIO             DECIMAL(1, 0) NOT NULL,
             DANO_REFT_EXTRT                DECIMAL(4, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CINDCD_SOLTC_TOT               CHAR(1) NOT NULL,
             DMES_REFT_EXTRT                DECIMAL(2, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             CSIT_PROCM_SOLTC               DECIMAL(1, 0) NOT NULL,
             HEXCUC_SOLTC_EXTRT             TIMESTAMP NOT NULL,
             QEXTRT_GERDR_PRVIO             DECIMAL(11, 0) NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TVALDC_PRVIO_EXTRT          *
      ******************************************************************
       01  GFCTB0K9.
      *    *************************************************************
           10 CIDTFD-FUNCL-PRVIO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-REFT-EXTRT      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CINDCD-SOLTC-TOT     PIC X(1).
      *    *************************************************************
           10 DMES-REFT-EXTRT      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PROCM-SOLTC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCUC-SOLTC-EXTRT   PIC X(26).
      *    *************************************************************
           10 QEXTRT-GERDR-PRVIO   PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
