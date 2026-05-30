***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TREST_VIP_AG)                              *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0J1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0J1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TREST_VIP_AG TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_VIP                DATE NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HINCL_REST_VIP                 TIMESTAMP NOT NULL,
             CFUNC_INCL_REST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REST_VIP                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_REST                DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TREST_VIP_AG                *
      ******************************************************************
       01  GFCTB0J1.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-VIP      PIC X(10).
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REST-VIP       PIC X(26).
      *    *************************************************************
           10 CFUNC-INCL-REST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REST-VIP       PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-REST      PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
