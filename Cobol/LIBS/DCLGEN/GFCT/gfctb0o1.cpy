***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTPO_ISENC_COMPO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0O1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0O1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTPO_ISENC_COMPO TABLE
           ( CTPO_ISENC                     DECIMAL(3, 0) NOT NULL,
             RTPO_ISENC                     CHAR(50) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTPO_ISENC_COMPO            *
      ******************************************************************
       01  GFCTB0O1.
      *    *************************************************************
           10 CTPO-ISENC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-ISENC           PIC X(50).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
