      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SUSP_GRP_UF)                               *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0E6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0E6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SUSP_GRP_UF TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             DINIC_SUSP_COBR                DATE NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SUSP_GRP_UF                 *
      ******************************************************************
       01  GFCTB0E6.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-SUSP-COBR      PIC X(10).
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
