***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CTA_CTLZA_DEB)                             *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB031))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB031)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CTA_CTLZA_DEB TABLE
           ( CAG_EVNTO                      DECIMAL(5, 0) NOT NULL,
             CCTA_EVNTO                     DECIMAL(13, 0) NOT NULL,
             HINCL_CTLZA                    TIMESTAMP NOT NULL,
             HEXCL_CTLZA                    TIMESTAMP NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CAG_CTLZA_DEB                  DECIMAL(5, 0) NOT NULL,
             CCTA_CTLZA_DEB                 DECIMAL(13, 0) NOT NULL,
             CTERM_INCL_CTLZA               CHAR(8) NOT NULL,
             CTERM_EXCL_CTLZA               CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CTA_CTLZA_DEB               *
      ******************************************************************
       01  GFCTB031.
      *    *************************************************************
           10 CAG-EVNTO            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-EVNTO           PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-CTLZA          PIC X(26).
      *    *************************************************************
           10 HEXCL-CTLZA          PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-CTLZA-DEB        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CTLZA-DEB       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-INCL-CTLZA     PIC X(8).
      *    *************************************************************
           10 CTERM-EXCL-CTLZA     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
