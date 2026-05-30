      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCRDRA_PGTO_PRFIL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB041))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB041)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCRDRA_PGTO_PRFIL TABLE
           ( CCRDRA_ESTBL_COML              DECIMAL(4, 0) NOT NULL,
             NPRFIL_CRDRA_PGTO              DECIMAL(1, 0) NOT NULL,
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA_CLI                  DECIMAL(5, 0) NOT NULL,
             NCTA_BCRIA                     DECIMAL(12, 0) NOT NULL,
             CTPO_CONTR_CTA                 DECIMAL(3, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP,
             CUSUAR_INCL                    CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CUSUAR_MANUT                   CHAR(7)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCRDRA_PGTO_PRFIL           *
      ******************************************************************
       01  BVVEB041.
      *    *************************************************************
           10 CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NPRFIL-CRDRA-PGTO    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA-CLI        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NCTA-BCRIA           PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-CTA       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
