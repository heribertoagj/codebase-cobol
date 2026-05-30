      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ESTBL_CONVN_VISA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB069))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB069)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ESTBL_CONVN_VISA TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             IESTBL_CONVN_BACC              CHAR(35) NOT NULL
           ) END-EXEC.
      ******************************************************************
      *  DB2PRD.ESTBL_CONVN_VISA                                       *
      ******************************************************************
       01  BACCB069.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 IESTBL-CONVN-BACC    PIC X(35).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
