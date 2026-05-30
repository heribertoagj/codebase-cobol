      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THIST_PEND_DIGIT_DOCTO)                    *
      *        LIBRARY(AD.DB2.DCLGEN(CTECB024))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CTECB024)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THIST_PEND_DIGIT_DOCTO TABLE         
           ( HINCL_REG_HIST                 TIMESTAMP NOT NULL,         
             NSOLTC_CTA_BCRIA               DECIMAL(17, 0) NOT NULL,    
             CDOCTO                         DECIMAL(5, 0),              
             CMOTVO_PEND_DIGIT              DECIMAL(2, 0),              
             CCPF_CNPJ_CLI                  CHAR(9) NOT NULL,           
             CCTRL_CPF_CNPJ_CLI             DECIMAL(2, 0) NOT NULL,     
             CFLIAL_CNPJ_CLI                CHAR(4),                    
             QPAG_DOCTO_DIGIT               DECIMAL(2, 0) NOT NULL,     
             CIDTFD_DIGIT_DOCTO             CHAR(1) NOT NULL,           
             QDOCTO_SEQ                     DECIMAL(1, 0) NOT NULL,     
             IARQ_IMAGE                     CHAR(100),                  
             CINDCD_CONCL_LOTE              DECIMAL(2, 0) NOT NULL,     
             CSIT_DIGIT_ENCRR               DECIMAL(1, 0) NOT NULL,     
             IDOCTO_DIGIT                   CHAR(60) NOT NULL,          
             CDOCTO_RETOR                   CHAR(20) NOT NULL,          
             ICLI_CTA_BCRIA                 CHAR(60) NOT NULL,          
             CSIT_ARMAZ_ARQ                 DECIMAL(4, 0) NOT NULL,     
             CTPO_CTA_BCRIA                 DECIMAL(1, 0) NOT NULL,     
             CTPO_PSSOA_ORIGE               DECIMAL(1, 0) NOT NULL,     
             CUSUAR_INCL                    CHAR(9) NOT NULL,           
             CUSUAR_MANUT                   CHAR(9),                    
             HINCL_REG                      TIMESTAMP NOT NULL,         
             HMANUT_REG                     TIMESTAMP                   
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THIST_PEND_DIGIT_DOCTO      *
      ******************************************************************
       01  CTECB024.                                                    
      *    *************************************************************
           10 HINCL-REG-HIST       PIC X(26).                           
      *    *************************************************************
           10 NSOLTC-CTA-BCRIA     PIC S9(17)V USAGE COMP-3.            
      *    *************************************************************
           10 CDOCTO               PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CMOTVO-PEND-DIGIT    PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CCPF-CNPJ-CLI        PIC X(9).                            
      *    *************************************************************
           10 CCTRL-CPF-CNPJ-CLI   PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CFLIAL-CNPJ-CLI      PIC X(4).                            
      *    *************************************************************
           10 QPAG-DOCTO-DIGIT     PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CIDTFD-DIGIT-DOCTO   PIC X(1).                            
      *    *************************************************************
           10 QDOCTO-SEQ           PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 IARQ-IMAGE           PIC X(100).                          
      *    *************************************************************
           10 CINDCD-CONCL-LOTE    PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CSIT-DIGIT-ENCRR     PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 IDOCTO-DIGIT         PIC X(60).                           
      *    *************************************************************
           10 CDOCTO-RETOR         PIC X(20).                           
      *    *************************************************************
           10 ICLI-CTA-BCRIA       PIC X(60).                           
      *    *************************************************************
           10 CSIT-ARMAZ-ARQ       PIC S9(4)V USAGE COMP-3.             
      *    *************************************************************
           10 CTPO-CTA-BCRIA       PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CTPO-PSSOA-ORIGE     PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).                            
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).                            
      *    *************************************************************
           10 HINCL-REG            PIC X(26).                           
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 23      *
      ******************************************************************
