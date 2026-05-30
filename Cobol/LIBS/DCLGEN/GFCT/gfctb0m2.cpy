***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TADSAO_INDVD_CLULR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0M2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0M2)                                     *
      *        APOST                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TADSAO_INDVD_CLULR TABLE
           ( CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             NSEQ_REG_CLULR                 DECIMAL(5, 0) NOT NULL,
             COPER_FONE_MOVEL               DECIMAL(9, 0) NOT NULL,
             CDDD_FONE_MOVEL                DECIMAL(3, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             NLIN_TFONI                     DECIMAL(11, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TADSAO_INDVD_CLULR          *
      ******************************************************************
       01  GFCTB0M2.
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
           10 HINCL-REG            PIC X(26).
           10 NSEQ-REG-CLULR       PIC S9(5)V USAGE COMP-3.
           10 COPER-FONE-MOVEL     PIC S9(9)V USAGE COMP-3.
           10 CDDD-FONE-MOVEL      PIC S9(3)V USAGE COMP-3.
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
           10 HMANUT-REG           PIC X(26).
           10 NLIN-TFONI           PIC S9(11)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
