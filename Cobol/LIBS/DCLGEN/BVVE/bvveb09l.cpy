      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TNEGOC_INSTC_AGNDA_RECPC)                  *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09L))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09L)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TNEGOC_INSTC_AGNDA_RECPC TABLE
           ( DMOVTO                         DATE NOT NULL,
             NIMAGE_AGNDA_RECPC             DECIMAL(9, 0) NOT NULL,
             NNEGOC_INSTC_AGNDA             DECIMAL(9, 0) NOT NULL,
             NPRIOR_NEGOC_RCBVL             DECIMAL(9, 0) NOT NULL,
             VUND_RCBVL_UTLZD               DECIMAL(17, 2) NOT NULL,
             VUND_RCBVL_FUTUR               DECIMAL(17, 2) NOT NULL,
             DVCTO_OPER_CIPAG               DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TNEGOC_INSTC_AGNDA_RECPC    *
      ******************************************************************
       01  BVVEB09L.
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 NIMAGE-AGNDA-RECPC   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NNEGOC-INSTC-AGNDA   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NPRIOR-NEGOC-RCBVL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 VUND-RCBVL-UTLZD     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VUND-RCBVL-FUTUR     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-OPER-CIPAG     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
