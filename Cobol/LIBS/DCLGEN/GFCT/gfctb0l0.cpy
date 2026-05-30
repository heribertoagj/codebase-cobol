***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCANCT_ADSAO_AGPTO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0L0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0L0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCANCT_ADSAO_AGPTO TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_SOLTC_CANCT              DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CSIT_SOLTC_CANCT               DECIMAL(2, 0) NOT NULL,
             QPRVIO_CANCT_ADSAO             DECIMAL(11, 0) NOT NULL,
             QADSAO_CANCD                   DECIMAL(11, 0) NOT NULL,
             HEXCUC_CALC_PRVIO              TIMESTAMP NOT NULL,
             HEXCUC_PROCM                   TIMESTAMP NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             RJUSTF_SOLTC_CANCT             VARCHAR(256) NOT NULL,
             RJUSTF_PAREC_CANCT             VARCHAR(256) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCANCT_ADSAO_AGPTO          *
      ******************************************************************
       01  GFCTB0L0.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-SOLTC-CANCT    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-SOLTC-CANCT     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QPRVIO-CANCT-ADSAO   PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 QADSAO-CANCD         PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCUC-CALC-PRVIO    PIC X(26).
      *    *************************************************************
           10 HEXCUC-PROCM         PIC X(26).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 RJUSTF-SOLTC-CANCT.
              49 RJUSTF-SOLTC-CANCT-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-SOLTC-CANCT-TEXT
                 PIC X(256).
      *    *************************************************************
           10 RJUSTF-PAREC-CANCT.
              49 RJUSTF-PAREC-CANCT-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-PAREC-CANCT-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
