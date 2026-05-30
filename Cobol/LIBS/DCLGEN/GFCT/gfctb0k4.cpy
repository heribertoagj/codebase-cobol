***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_EXCED_EXTRT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0K4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0K4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_EXCED_EXTRT TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             DANO_REFT_EXTRT                DECIMAL(4, 0) NOT NULL,
             CIDTFD_ORIGE_SOLTC             DECIMAL(1, 0) NOT NULL,
             DMES_REFT_EXTRT                DECIMAL(2, 0) NOT NULL,
             CINDCD_EXTRT_SOLTD             DECIMAL(1, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             QCONS_EXTRT_SOLTD              DECIMAL(5, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_EXCED_EXTRT           *
      ******************************************************************
       01  GFCTB0K4.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-REFT-EXTRT      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-ORIGE-SOLTC   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DMES-REFT-EXTRT      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-EXTRT-SOLTD   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 QCONS-EXTRT-SOLTD    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
