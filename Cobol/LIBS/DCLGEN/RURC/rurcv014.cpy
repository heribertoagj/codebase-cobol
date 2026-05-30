      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01MUNICIPIO_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RURCV014))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCV014)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01MUNICIPIO_RURAL TABLE
           ( CMUN_RURAL                     DECIMAL(9, 0) NOT NULL,
             IMUN                           CHAR(30) NOT NULL,
             CAREA_ESPCL_AGRON              DECIMAL(3, 0) NOT NULL,
             CCTRO_FSCAL_RURAL              DECIMAL(3, 0) NOT NULL,
             CMCRO_RGIAO                    DECIMAL(3, 0) NOT NULL,
             CRGIAO_RURAL                   DECIMAL(1, 0) NOT NULL,
             CPROJ_RURAL                    DECIMAL(3, 0) NOT NULL,
             CUF                            DECIMAL(3, 0) NOT NULL,
             CAREA_EMERG                    DECIMAL(1, 0) NOT NULL,
             CRGIAO_RURAL_SCRTZ             DECIMAL(3, 0) NOT NULL,
             CINDCD_REST_RURAL              CHAR(1) NOT NULL,
             DINIC_VGCIA_REST               DATE NOT NULL,
             CMUN_REFT_BACEN                DECIMAL(6, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01MUNICIPIO_RURAL          *
      ******************************************************************
       01  RURCV014.
      *    *************************************************************
           10 CMUN-RURAL           PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 IMUN                 PIC X(30).
      *    *************************************************************
           10 CAREA-ESPCL-AGRON    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRO-FSCAL-RURAL    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMCRO-RGIAO          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-RURAL         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPROJ-RURAL          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CUF                  PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAREA-EMERG          PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-RURAL-SCRTZ   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-REST-RURAL    PIC X(1).
      *    *************************************************************
           10 DINIC-VGCIA-REST     PIC X(10).
      *    *************************************************************
           10 CMUN-REFT-BACEN      PIC S9(6)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
