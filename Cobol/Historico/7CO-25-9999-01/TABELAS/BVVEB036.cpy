      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPARTC_TROCA_CERTF)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB036))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB036)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPARTC_TROCA_CERTF TABLE
           ( CISPB_PARTC_MSGEM              DECIMAL(9, 0) NOT NULL,
             IPARTC_TROCA_MSGEM             CHAR(40) NOT NULL,
             CISPB_PARTC_MSGEM_ST           CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPARTC_TROCA_CERTF          *
      ******************************************************************
       01  BVVEB036.
      *    *************************************************************
           10 CISPB-PARTC-MSGEM    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 IPARTC-TROCA-MSGEM   PIC X(40).
      *    *************************************************************
           10 CISPB-PARTC-MSGEM-ST
              PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
