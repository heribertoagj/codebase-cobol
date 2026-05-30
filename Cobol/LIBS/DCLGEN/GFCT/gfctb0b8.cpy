      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PDIDO_FLEXZ_PAB)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0B8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0B8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PDIDO_FLEXZ_PAB TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HIDTFD_FLEXZ_AGPTO             TIMESTAMP NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PDIDO_FLEXZ_PAB             *
-     ******************************************************************
       01  GFCTB0B8.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-FLEXZ-AGPTO   PIC X(26).
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
