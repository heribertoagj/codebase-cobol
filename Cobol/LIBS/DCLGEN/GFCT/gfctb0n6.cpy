***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOPER_PERM_ADSAO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOPER_PERM_ADSAO TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             COPER_FONE_MOVEL               DECIMAL(9, 0) NOT NULL,
             DINIC_VGCIA_REG                DATE NOT NULL,
             DFIM_VGCIA_REG                 DATE NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOPER_PERM_ADSAO            *
      ******************************************************************
       01  GFCTB0N6.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-FONE-MOVEL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-REG      PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-REG       PIC X(10).
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
