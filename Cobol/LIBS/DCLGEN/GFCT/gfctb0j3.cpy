***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDEPTO_ACSSO_AUTRZ)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0J3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0J3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDEPTO_ACSSO_AUTRZ TABLE
           ( CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC_AUTRZ_ACSSO             DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_AUTRZ              DATE NOT NULL,
             DFIM_VGCIA_AUTRZ               DATE NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDEPTO_ACSSO_AUTRZ          *
      ******************************************************************
       01  GFCTB0J3.
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-AUTRZ-ACSSO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-AUTRZ    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-AUTRZ     PIC X(10).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
