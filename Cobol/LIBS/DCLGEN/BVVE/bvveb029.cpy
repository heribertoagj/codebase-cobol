      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOG_OPER_DESC)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB029))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB029)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOG_OPER_DESC TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CIDTFD_PRODT_CATAO             DECIMAL(5, 0) NOT NULL,
             CCTRO_CUSTO                    CHAR(4) NOT NULL,
             DMOVTO                         DATE NOT NULL,
             NOPER_DESC_COML                DECIMAL(13, 0) NOT NULL,
             CTPO_OPER                      CHAR(1) NOT NULL,
             HATULZ                         TIMESTAMP NOT NULL,
             VMOVTO_DEB_CATAO               DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOG_OPER_DESC              *
      ******************************************************************
       01  BVVEB029.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-CATAO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRO-CUSTO          PIC X(4).
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 NOPER-DESC-COML      PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OPER            PIC X(1).
      *    *************************************************************
           10 HATULZ               PIC X(26).
      *    *************************************************************
           10 VMOVTO-DEB-CATAO     PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
