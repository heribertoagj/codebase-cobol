      ******************************************************************
      * DCLGEN TABLE(DB2PRD.VLR_PARM_AFLIA)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB016))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB016)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.VLR_PARM_AFLIA TABLE
           ( CPARM_AFLIA_ESTBL              DECIMAL(3, 0) NOT NULL,
             CSEQ_VLR_PARM                  DECIMAL(2, 0) NOT NULL,
             WCONTD_PARM                    VARCHAR(255)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.VLR_PARM_AFLIA              *
      ******************************************************************
       01  BVVEB016.
      *    *************************************************************
           10 CPARM-AFLIA-ESTBL    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-VLR-PARM        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 WCONTD-PARM.
              49 WCONTD-PARM-LEN   PIC S9(4) USAGE COMP.
              49 WCONTD-PARM-TEXT  PIC X(255).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
