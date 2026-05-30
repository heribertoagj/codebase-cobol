      ******************************************************************        
      * DCLGEN TABLE(DB2PRD.TCTRL_REFT_DEB)                            *        
      *        LIBRARY(AD.DB2.DCLGEN(DBTOB023))                        *        
      *        ACTION(REPLACE)                                         *        
      *        LANGUAGE(COBOL)                                         *        
      *        STRUCTURE(DBTOB023)                                     *        
      *        APOST                                                   *        
      *        LABEL(YES)                                              *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE DB2PRD.TCTRL_REFT_DEB TABLE                         
           ( CTPO_CTRL_DEB                  DECIMAL(5, 0) NOT NULL,             
             CCTRL_REFT_DEB                 DECIMAL(5, 0) NOT NULL,             
             RCTRL_REFT_DEB                 CHAR(40),                           
             DINIC_VGCIA_CTRL               DATE,                               
             DFIM_VGCIA_CTRL                DATE,                               
             RTPO_CLASF_CTRL                CHAR(30),                           
             ROBS_CLASF_CTRL                CHAR(250),                          
             CATRIB_DIC                     INTEGER,                            
             CCPO_DIC                       INTEGER,                            
             CESTRT_DIC                     INTEGER,                            
             NVRSAO_ESTRT_DIC               INTEGER,                            
             HMANUT_REG                     TIMESTAMP,                          
             CUSUAR_MANUT                   CHAR(9),                            
             CINDCD_SIT_ATIVO               CHAR(1) NOT NULL                    
           ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_REFT_DEB              *        
      ******************************************************************        
       01  DBTOB023.                                                            
      *    *************************************************************        
           10 CTPO-CTRL-DEB        PIC S9(5)V USAGE COMP-3.                     
      *    *************************************************************        
           10 CCTRL-REFT-DEB       PIC S9(5)V USAGE COMP-3.                     
      *    *************************************************************        
           10 RCTRL-REFT-DEB       PIC X(40).                                   
      *    *************************************************************        
           10 DINIC-VGCIA-CTRL     PIC X(10).                                   
      *    *************************************************************        
           10 DFIM-VGCIA-CTRL      PIC X(10).                                   
      *    *************************************************************        
           10 RTPO-CLASF-CTRL      PIC X(30).                                   
      *    *************************************************************        
           10 ROBS-CLASF-CTRL      PIC X(250).                                  
      *    *************************************************************        
           10 CATRIB-DIC           PIC S9(9) USAGE COMP.                        
      *    *************************************************************        
           10 CCPO-DIC             PIC S9(9) USAGE COMP.                        
      *    *************************************************************        
           10 CESTRT-DIC           PIC S9(9) USAGE COMP.                        
      *    *************************************************************        
           10 NVRSAO-ESTRT-DIC     PIC S9(9) USAGE COMP.                        
      *    *************************************************************        
           10 HMANUT-REG           PIC X(26).                                   
      *    *************************************************************        
           10 CUSUAR-MANUT         PIC X(9).                                    
      *    *************************************************************        
           10 CINDCD-SIT-ATIVO     PIC X(1).                                    
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *        
      ******************************************************************        
