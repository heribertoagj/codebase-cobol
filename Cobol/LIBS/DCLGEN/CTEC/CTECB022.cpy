      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDSTNO_EVTAL_SDO_CREDR_TED)                *
      *        LIBRARY(AD.DB2.DCLGEN(CTECB022))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CTECB022)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDSTNO_EVTAL_SDO_CREDR_TED TABLE     
           ( NSOLTC_CTA_BCRIA               DECIMAL(17, 0) NOT NULL,    
             CEMPR                          DECIMAL(5, 0),              
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0),              
             CSGRP_CTBIL                    DECIMAL(3, 0),              
             CBCO_DSTNO                     DECIMAL(3, 0) NOT NULL,     
             CAG_DSTNO                      DECIMAL(5, 0) NOT NULL,     
             CCTA_BCRIA_DSTNO               DECIMAL(20, 0) NOT NULL,    
             QTTLAR_ORIGE                   DECIMAL(2, 0) NOT NULL,     
             CCPF_CNPJ_ORIGE                CHAR(9) NOT NULL,           
             CCTRL_ORIGE                    DECIMAL(2, 0) NOT NULL,
             CFLIAL_CNPJ_ORIGE              CHAR(4),               
             ITTLAR_CTA_ORIGE               CHAR(60) NOT NULL,     
             CCPF_ORIGE_SECUN               CHAR(9),               
             CCTRL_ORIGE_SECUN              DECIMAL(2, 0),         
             CFLIAL_ORIGE_SECUN             CHAR(4),               
             ITTLAR_SECUN_ORIGE             CHAR(60),              
             QTTLAR_DSTNO                   DECIMAL(2, 0) NOT NULL,
             CCPF_CNPJ_DSTNO                CHAR(9) NOT NULL,      
             CCTRL_DSTNO                    DECIMAL(2, 0) NOT NULL,
             CFLIAL_CNPJ_DSTNO              CHAR(4),               
             ITTLAR_CTA_DSTNO               CHAR(60) NOT NULL,     
             CCPF_DSTNO_SECUN               CHAR(9),               
             CCTRL_DSTNO_SECUN              DECIMAL(2, 0),         
             CFLIAL_DSTNO_SECUN             CHAR(4),               
             ITTLAR_SECUN_DSTNO             CHAR(60),              
             CFORMA_RLIZC_TRNSF             DECIMAL(1, 0) NOT NULL,
             CINDCD_TTLAR                   DECIMAL(1, 0) NOT NULL,
             CTPO_CTA_DSTNO                 CHAR(2) NOT NULL,      
             CTPO_REGRA_TRNSF               DECIMAL(1, 0) NOT NULL,
             VLCTO_TRNSF_DSTNO              DECIMAL(17, 2)              
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDSTNO_EVTAL_SDO_CREDR_TED  *
      ******************************************************************
       01  CTECB022.                                                    
      *    *************************************************************
           10 NSOLTC-CTA-BCRIA     PIC S9(17)V USAGE COMP-3.            
      *    *************************************************************
           10 CEMPR                PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CBCO-DSTNO           PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CAG-DSTNO            PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCTA-BCRIA-DSTNO     PIC S9(20)V USAGE COMP-3.            
      *    *************************************************************
           10 QTTLAR-ORIGE         PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CCPF-CNPJ-ORIGE      PIC X(9).                            
      *    *************************************************************
           10 CCTRL-ORIGE          PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CFLIAL-CNPJ-ORIGE    PIC X(4).                            
      *    *************************************************************
           10 ITTLAR-CTA-ORIGE     PIC X(60).                           
      *    *************************************************************
           10 CCPF-ORIGE-SECUN     PIC X(9).                            
      *    *************************************************************
           10 CCTRL-ORIGE-SECUN    PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CFLIAL-ORIGE-SECUN   PIC X(4).                            
      *    *************************************************************
           10 ITTLAR-SECUN-ORIGE   PIC X(60).                           
      *    *************************************************************
           10 QTTLAR-DSTNO         PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CCPF-CNPJ-DSTNO      PIC X(9).                            
      *    *************************************************************
           10 CCTRL-DSTNO          PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CFLIAL-CNPJ-DSTNO    PIC X(4).                            
      *    *************************************************************
           10 ITTLAR-CTA-DSTNO     PIC X(60).                           
      *    *************************************************************
           10 CCPF-DSTNO-SECUN     PIC X(9).                            
      *    *************************************************************
           10 CCTRL-DSTNO-SECUN    PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CFLIAL-DSTNO-SECUN   PIC X(4).                            
      *    *************************************************************
           10 ITTLAR-SECUN-DSTNO   PIC X(60).                           
      *    *************************************************************
           10 CFORMA-RLIZC-TRNSF   PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CINDCD-TTLAR         PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CTPO-CTA-DSTNO       PIC X(2).                            
      *    *************************************************************
           10 CTPO-REGRA-TRNSF     PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 VLCTO-TRNSF-DSTNO    PIC S9(15)V9(2) USAGE COMP-3.        
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
