***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAUTRZ_ENVIO_EXTRT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0K3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0K3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAUTRZ_ENVIO_EXTRT TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CINDCD_AUTRZ_EMIS              DECIMAL(1, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             RJUSTF_INCL_AUTRZ              VARCHAR(256) NOT NULL,
             RJUSTF_EXCL_AUTRZ              VARCHAR(256) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAUTRZ_ENVIO_EXTRT          *
      ******************************************************************
       01  GFCTB0K3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AUTRZ-EMIS    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-INCL-AUTRZ.
              49 RJUSTF-INCL-AUTRZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-INCL-AUTRZ-TEXT
                 PIC X(256).
      *    *************************************************************
           10 RJUSTF-EXCL-AUTRZ.
              49 RJUSTF-EXCL-AUTRZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-EXCL-AUTRZ-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
