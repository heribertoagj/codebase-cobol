***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSOLTC_RPROC_EXTRT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSOLTC_RPROC_EXTRT TABLE
           ( CROTNA_RPROC_EXTRT             CHAR(4) NOT NULL,
             DANO_MES_RPROC                 DECIMAL(6, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CSIT_PROCM_SOLTC               DECIMAL(1, 0) NOT NULL,
             HEFTVC_PROCM_SOLTC             TIMESTAMP NOT NULL,
             QREG_RPROC_EXTRT               DECIMAL(11, 0) NOT NULL,
             QREG_REJTD_EXTRT               DECIMAL(11, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL,
             RJUSTF_SOLTC_RPROC             VARCHAR(256) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSOLTC_RPROC_EXTRT          *
      ******************************************************************
       01  GFCTB0N5.
      *    *************************************************************
           10 CROTNA-RPROC-EXTRT   PIC X(4).
      *    *************************************************************
           10 DANO-MES-RPROC       PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CSIT-PROCM-SOLTC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HEFTVC-PROCM-SOLTC   PIC X(26).
      *    *************************************************************
           10 QREG-RPROC-EXTRT     PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 QREG-REJTD-EXTRT     PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 RJUSTF-SOLTC-RPROC.
              49 RJUSTF-SOLTC-RPROC-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-SOLTC-RPROC-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
