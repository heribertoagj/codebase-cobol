      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TVRSAO_ESTRT_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB00B))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB00B)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TVRSAO_ESTRT_CATAO TABLE
           ( NESTRT_INFO_BACEN              DECIMAL(3, 0) NOT NULL,
             NVRSAO_ESTRT_INFO              DECIMAL(3, 0) NOT NULL,
             CVRSAO_ESTRT_INFO              CHAR(10) NOT NULL,
             DDSATV_VRSAO_ESTRT             DATE,
             DATIVO_VRSAO_ESTRT             DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TVRSAO_ESTRT_CATAO          *
      ******************************************************************
       01  BVVEB00B.
      *    *************************************************************
           10 NESTRT-INFO-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NVRSAO-ESTRT-INFO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CVRSAO-ESTRT-INFO    PIC X(10).
      *    *************************************************************
           10 DDSATV-VRSAO-ESTRT   PIC X(10).
      *    *************************************************************
           10 DATIVO-VRSAO-ESTRT   PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
