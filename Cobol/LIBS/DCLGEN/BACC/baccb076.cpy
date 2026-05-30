      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SERVC_TARIF)                               *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB076))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB076)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SERVC_TARIF TABLE
           ( CSERVC                         DECIMAL(5, 0) NOT NULL,
             RSERVC                         CHAR(25) NOT NULL,
             VTARIF_SERVC                   DECIMAL(13, 2) NOT NULL,
             VMIN_SERVC                     DECIMAL(13, 2) NOT NULL,
             VMAX_SERVC                     DECIMAL(13, 2) NOT NULL,
             QDIA_CONS                      DECIMAL(3, 0) NOT NULL,
             CCART_TARIF                    DECIMAL(3, 0) NOT NULL,
             CLCTO_TARIF                    DECIMAL(3, 0) NOT NULL,
             CEXCL                          CHAR(1) NOT NULL,
             HINCL_TARIF                    TIMESTAMP NOT NULL,
             HATULZ_TARIF                   TIMESTAMP NOT NULL,
             HEXCL_TARIF                    TIMESTAMP NOT NULL,
             CFUNC_BDSCO_INCL               CHAR(7) NOT NULL,
             CFUNC_BDSCO_ATULZ              CHAR(7) NOT NULL,
             CFUNC_BDSCO_EXCL               CHAR(7) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             CTERM_ATULZ                    CHAR(8) NOT NULL,
             CTERM_EXCL                     CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SERVC_TARIF                 *
      ******************************************************************
       01  BACCB076.
      *    *************************************************************
           10 CSERVC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RSERVC               PIC X(25).
      *    *************************************************************
           10 VTARIF-SERVC         PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMIN-SERVC           PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMAX-SERVC           PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QDIA-CONS            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCART-TARIF          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-TARIF          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CEXCL                PIC X(1).
      *    *************************************************************
           10 HINCL-TARIF          PIC X(26).
      *    *************************************************************
           10 HATULZ-TARIF         PIC X(26).
      *    *************************************************************
           10 HEXCL-TARIF          PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO-INCL     PIC X(7).
      *    *************************************************************
           10 CFUNC-BDSCO-ATULZ    PIC X(7).
      *    *************************************************************
           10 CFUNC-BDSCO-EXCL     PIC X(7).
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 CTERM-ATULZ          PIC X(8).
      *    *************************************************************
           10 CTERM-EXCL           PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
