      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MOVIMENTACAO_EDBS)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB001))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB001)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MOVIMENTACAO_EDBS TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             IVDDOR_VISA                    CHAR(40) NOT NULL,
             DMOVTO                         DATE NOT NULL,
             VDISPN_ANTEC_CREDT             DECIMAL(15, 2) NOT NULL,
             CAG_CTA                        DECIMAL(12, 0) NOT NULL,
             VRECBD_ANTEC_VDDOR             DECIMAL(15, 2) NOT NULL,
             VDISPN_ANTEC_VDDOR             DECIMAL(15, 2) NOT NULL,
             CORIGE_ANTEC_VDDOR             CHAR(4) NOT NULL,
             DRECEB_VISA                    DATE NOT NULL,
             CRAMO_ATVDD                    DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MOVIMENTACAO_EDBS           *
      ******************************************************************
       01  BVVEB001.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 IVDDOR-VISA          PIC X(40).
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 VDISPN-ANTEC-CREDT   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CAG-CTA              PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 VRECBD-ANTEC-VDDOR   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VDISPN-ANTEC-VDDOR   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-ANTEC-VDDOR   PIC X(4).
      *    *************************************************************
           10 DRECEB-VISA          PIC X(10).
      *    *************************************************************
           10 CRAMO-ATVDD          PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
