//SLIG0370 JOB 'SLIG,4335,PR11','D336356',MSGCLASS=Z,SCHENV=BATCH   
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                              
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                         
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                         
//STEP1    EXEC PGM=SLIG6092,                                       
//       PARM='0101'                                                
//*                                                                 
//* ***    FORMATA CNPF/CPF CLIENTE A PARTIR DO                     
//* ***    BANCO / AGENCIA / CONTA.                                 
//*                                                                 
//ENTRA001 DD DSN=MX.SLIG.PENDCOT5(0),                              
//       DISP=SHR                                                   
//SAIDA001 DD DSN=MX.SLIG.PENDCOT6(+1),                             
//       DISP=(,CATLG,DELETE),                                      
//       UNIT=(DISCO,50),                                           
//       SPACE=(TRK,(018000,5940),RLSE),                            
//       DCB=(MX.A,LRECL=0182,RECFM=FB),                            
//       DATACLAS=PRODX37                                           
//SAIDA002 DD DSN=MX.SLIG.PENCOT6A(+1),                             
//       DISP=(,CATLG,DELETE),                            
//       UNIT=(DISCO,07),                                 
//       SPACE=(TRK,(006000,1200),RLSE),                  
//       DCB=(MX.A,LRECL=0182,RECFM=FB)                   
//SRELCTRL DD SYSOUT=*                                    
//SYSOUT   DD SYSOUT=*                                    
//SYSUDUMP DD SYSOUT=Y                                    
//*                                                       
//STEP1A   EXEC PGM=SLIG9050                              
//*                                                       
//* ***    FORMATA TIPO DE PESSO PARA CLIENTE E SACADO    
//*                                                       
//ENTRA001 DD DSN=*.STEP1.SAIDA001,                       
//       DISP=OLD                                         
//SAIDA001 DD DSN=MX.SLIG.PENDCOT6.TPPESS(+1),            
//       DISP=(,CATLG,DELETE),                            
//       UNIT=(DISCO,50),                                 
//       SPACE=(TRK,(018000,5940),RLSE),                  
//       DCB=(MX.A,LRECL=0182,RECFM=FB),                  
//       DATACLAS=PRODX37                               
//SRELCTRL DD SYSOUT=*                                  
//SYSOUT   DD SYSOUT=*                                  
//SYSUDUMP DD SYSOUT=Y                                  
//*                                                     
//STEP1B   EXEC SORTD                                   
//*                                                     
//* ***    ---------------------------------------------
//* ***    GERA O ARQUIVO MX.SLIG.PRV.PENDCOT6          
//* ***    A PARTIR DO ARQUIVO MX.SLIG.PENDCOT6         
//* ***    CLASSIFICA ARQUIVO POR:                      
//* ***    CNPJ DO CLIENTE                              
//* ***    CHAVE DO TITULO                              
//* ***    TIPO DA CARTEIRA                             
//* ***    ---------------------------------------------
//*                                                     
//SORTIN   DD DSN=*.STEP1.SAIDA001,                     
//       DISP=(OLD,DELETE,KEEP)                         
//SORTOUT  DD DSN=MX.SLIG.PRV.PENDCOT6(+1),             
//       DISP=(,CATLG,DELETE),                          
//       UNIT=(DISCO,50),                               
//       SPACE=(TRK,(018000,5940),RLSE),                
//       DCB=(MX.A,LRECL=0182,RECFM=FB),                
//       DATACLAS=PRODX37                               
//SORTLIB  DD DSN=SYS1.SORTLIB,                         
//       DISP=SHR                                       
//SORTWK01 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00330))                            
//SORTWK02 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00330))                            
//SORTWK03 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00330))                            
//SORTWK04 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00330))                            
//SORTWK05 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00330))                            
//SORTWK06 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00330))                            
//SORTWK07 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK08 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK09 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK10 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK11 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK12 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK13 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK14 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK15 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))    
//SORTWK16 DD UNIT=DISCO,       
//       SPACE=(CYL,(00330))       
//SORTWK17 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK18 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK19 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK20 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK21 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK22 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK23 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK24 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK25 DD UNIT=DISCO,          
//       SPACE=(CYL,(00330))       
//SORTWK26 DD UNIT=DISCO,         
//       SPACE=(CYL,(00330))      
//SORTWK27 DD UNIT=DISCO,         
//       SPACE=(CYL,(00330))      
//SORTWK28 DD UNIT=DISCO,         
//       SPACE=(CYL,(00330))      
//SORTWK29 DD UNIT=DISCO,         
//       SPACE=(CYL,(00330))      
//SORTWK30 DD UNIT=DISCO,         
//       SPACE=(CYL,(00330))      
//SORTWK31 DD UNIT=DISCO,         
//       SPACE=(CYL,(00330))      
//SORTWK32 DD UNIT=DISCO,         
//       SPACE=(CYL,(00330))      
//SORTWK33 DD UNIT=DISCO,         
//       SPACE=(CYL,(00330))      
//SYSOUT   DD SYSOUT=*            
//SYSUDUMP DD SYSOUT=Y            
//SYSIN    DD *                   
 SORT FIELDS=(143,9,CH,A,152,5,CH,A,157,2,ZD,A,                   
              141,2,ZD,A,104,6,PD,A,110,6,PD,A,                   
              13,2,PD,A)                                          
 END                                                              
//*                                                               
//STEP2    EXEC SORTD                                             
//*                                                               
//* ***    CLASSIFICA POR CGC-CLIENTE, FILIAL-CLIENTE E CONTROLE, 
//* ***    CGC-SACADO, FILIAL-SACADO, CONTROLE-SACADO E CARTEIRA. 
//*                                                               
//SORTIN   DD DSN=*.STEP1A.SAIDA001,                              
//       DISP=OLD                                                 
//SORTOUT  DD DSN=MX.SLIG.PENCOT6C(+1),                           
//       DISP=(,CATLG,DELETE),                                    
//       UNIT=(DISCO,50),                                         
//       SPACE=(TRK,(018000,5940),RLSE),                          
//       DCB=(MX.A,LRECL=0182,RECFM=FB),                          
//       DATACLAS=PRODX37                                         
//SORTLIB  DD DSN=SYS1.SORTLIB,                                   
//       DISP=SHR                      
//SORTWK01 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK02 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK03 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK04 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK05 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK06 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK07 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK08 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK09 DD UNIT=DISCO,              
//       SPACE=(CYL,(00330))           
//SORTWK10 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK11 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK12 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK13 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK14 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK15 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK16 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK17 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK18 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))            
//SORTWK19 DD UNIT=DISCO,               
//       SPACE=(CYL,(00330))     
//SORTWK20 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK21 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK22 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK23 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK24 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK25 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK26 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK27 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK28 DD UNIT=DISCO,        
//       SPACE=(CYL,(00330))     
//SORTWK29 DD UNIT=DISCO,                                        
//       SPACE=(CYL,(00330))                                     
//SORTWK30 DD UNIT=DISCO,                                        
//       SPACE=(CYL,(00330))                                     
//SORTWK31 DD UNIT=DISCO,                                        
//       SPACE=(CYL,(00330))                                     
//SORTWK32 DD UNIT=DISCO,                                        
//       SPACE=(CYL,(00330))                                     
//SORTWK33 DD UNIT=DISCO,                                        
//       SPACE=(CYL,(00330))                                     
//SYSOUT   DD SYSOUT=*                                           
//SYSUDUMP DD SYSOUT=Y                                           
//SYSIN    DD *                                                  
 SORT FIELDS=(143,9,A,139,1,A,159,9,A,140,1,A,13,1.4,A),FORMAT=BI
 END                                                             
//*                                                              
//STEP3    EXEC PGM=SLIG4010                                     
//*                                                              
//* ***    UNIFICA REGISTRO                                      
//* ***    GERA ARQUIVO TOTAL CLIENTE / SACADO E ARQUIVO TOTAL CLIENTE
//*                                                                   
//CADACLIE DD DSN=MX.SLIG.PRV.CADACLIE(0),                            
//       DISP=SHR                                                     
//CLIENDUP DD DSN=*.STEP2.SORT.SORTOUT,                               
//       DISP=(OLD,DELETE,KEEP)                                       
//CLIENUNI DD DSN=MX.SLIG.PENDCOT7(+1),                               
//       DISP=(,CATLG,DELETE),                                        
//       UNIT=(DISCO,20),                                             
//       SPACE=(TRK,(027363,5473),RLSE),                              
//       DCB=(MX.A,LRECL=0130,RECFM=FB),                              
//       DATACLAS=PRODX37                                             
//TOTCLIE  DD DSN=MX.SLIG.PENDCOT8(+1),                               
//       DISP=(,CATLG,DELETE),                                        
//       UNIT=DISCO,                                                  
//       SPACE=(TRK,(000135,00027),RLSE),                             
//       DCB=(MX.A,LRECL=0130,RECFM=FB)                               
//SRELCTRL DD SYSOUT=*                                                
//SYSOUT   DD SYSOUT=*                                                
//SYSUDUMP DD SYSOUT=Y      
//*                         
