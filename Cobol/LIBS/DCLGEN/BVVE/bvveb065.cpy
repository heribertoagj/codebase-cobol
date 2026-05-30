      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDETLH_VRSAO_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB065))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB065)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDETLH_VRSAO_CATAO TABLE
           ( NESTRT_INFO_BACEN              DECIMAL(3, 0) NOT NULL,
             NVRSAO_ESTRT_INFO              DECIMAL(3, 0) NOT NULL,
             CSEQ_DETLH_VRSAO               DECIMAL(1, 0) NOT NULL,
             WDETLH_VRSAO_ESTRT             VARCHAR(4000)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDETLH_VRSAO_CATAO          *
      ******************************************************************
       01  BVVEB065.
      *    *************************************************************
           10 NESTRT-INFO-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NVRSAO-ESTRT-INFO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-DETLH-VRSAO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 WDETLH-VRSAO-ESTRT.
              49 WDETLH-VRSAO-ESTRT-LEN
                 PIC S9(4) USAGE COMP.
              49 WDETLH-VRSAO-ESTRT-TEXT
                 PIC X(4000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
