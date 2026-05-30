      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONTD_MSGRA_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB062))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB062)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONTD_MSGRA_CATAO TABLE
           ( CMNTRC_MSGRA                   CHAR(23) NOT NULL,
             NCONTD_MSGRA                   DECIMAL(2, 0) NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0),
             CINDCD_FORMT_MSGEM             DECIMAL(1, 0),
             RCONTD_MSGEM_TRFGO             CHAR(68),
             WCONTD_MSGEM_TRFGO             VARCHAR(32000)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONTD_MSGRA_CATAO          *
      ******************************************************************
       01  BVVEB062.
      *    *************************************************************
           10 CMNTRC-MSGRA         PIC X(23).
      *    *************************************************************
           10 NCONTD-MSGRA         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-FORMT-MSGEM   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RCONTD-MSGEM-TRFGO   PIC X(68).
      *    *************************************************************
           10 WCONTD-MSGEM-TRFGO.
              49 WCONTD-MSGEM-TRFGO-LEN
                 PIC S9(4) USAGE COMP.
              49 WCONTD-MSGEM-TRFGO-TEXT
                 PIC X(32000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
