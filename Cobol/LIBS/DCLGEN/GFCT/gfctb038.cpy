***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.DEB_UNIC_CONSL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB038))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB038)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.DEB_UNIC_CONSL TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HINCL_DEB_UNIC                 TIMESTAMP NOT NULL,
             HEXCL_DEB_UNIC                 TIMESTAMP NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CTERM_INCL_DEB                 CHAR(8) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CTERM_EXCL_DEB                 CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.DEB_UNIC_CONSL              *
      ******************************************************************
       01  GFCTB038.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-DEB-UNIC       PIC X(26).
      *    *************************************************************
           10 HEXCL-DEB-UNIC       PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL-DEB       PIC X(8).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-EXCL-DEB       PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
