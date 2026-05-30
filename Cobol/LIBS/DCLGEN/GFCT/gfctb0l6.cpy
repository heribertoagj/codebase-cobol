      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAUTRZ_ENVIO_POSTO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0L6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0L6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAUTRZ_ENVIO_POSTO TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CINDCD_AUTRZ_EMIS              DECIMAL(1, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAUTRZ_ENVIO_POSTO          *
      ******************************************************************
       01  GFCTB0L6.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AUTRZ-EMIS    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
