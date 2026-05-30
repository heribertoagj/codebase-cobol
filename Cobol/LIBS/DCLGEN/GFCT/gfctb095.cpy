***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MSGEM_PDRAO_SIST)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB095))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB095)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MSGEM_PDRAO_SIST TABLE
           ( CSIST_PRINC                    CHAR(4) NOT NULL,
             CTPO_PROCM_MSGEM               CHAR(1) NOT NULL,
             CSEQ_IDTFD_MSGEM               DECIMAL(4, 0) NOT NULL,
             RMSGEM_USUAR                   CHAR(70) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MSGEM_PDRAO_SIST            *
      ******************************************************************
       01  GFCTB095.
      *    *************************************************************
           10 CSIST-PRINC          PIC X(4).
      *    *************************************************************
           10 CTPO-PROCM-MSGEM     PIC X(1).
      *    *************************************************************
           10 CSEQ-IDTFD-MSGEM     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 RMSGEM-USUAR         PIC X(70).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
