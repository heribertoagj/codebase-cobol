//SLIG0371 JOB 'SLIG,4510,PR11','B031531',MSGCLASS=Z,SCHENV=BATCH   
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                              
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                         
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                         
//STEP1    EXEC SORTD                                               
//*                                                                 
//* ***    CLASSIFICA POR CGC-SACADO, PESSOA E CARTEIRA.            
//*                                                                 
//SORTIN   DD DSN=MX.SLIG.PENDCOT6.TPPESS(0),                       
//       DISP=OLD                                                   
//SORTOUT  DD DSN=MX.SLIG.SACTIT6C(+1),                             
//       DISP=(,CATLG,DELETE),                                      
//       UNIT=(DISCO,50),                                           
//       SPACE=(TRK,(018000,5940),RLSE),                            
//       DCB=(MX.A,LRECL=0182,RECFM=FB),                            
//       DATACLAS=PRODX37                                           
//SORTLIB  DD DSN=SYS1.SORTLIB,                                     
//       DISP=SHR                                                   
//SORTWK01 DD UNIT=DISCO,                                           
//       SPACE=(CYL,(00540))                                  
//SORTWK02 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK03 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK04 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK05 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK06 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK07 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK08 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK09 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK10 DD UNIT=DISCO,                                     
//       SPACE=(CYL,(00540))                                  
//SORTWK11 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK12 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK13 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK14 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK15 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK16 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK17 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK18 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK19 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))                                       
//SORTWK20 DD UNIT=DISCO,                                          
//       SPACE=(CYL,(00540))             
//SORTWK21 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK22 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK23 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK24 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK25 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK26 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK27 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK28 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK29 DD UNIT=DISCO,                
//       SPACE=(CYL,(00540))             
//SORTWK30 DD UNIT=DISCO,                                    
//       SPACE=(CYL,(00540))                                 
//SYSIN    DD *                                              
 SORT FIELDS=(159,9,A,140,1,A,13,1.4,A),FORMAT=BI            
 END                                                         
//*                                                          
//STEP2    EXEC PGM=SLIG5540                                 
//*                                                          
//* ***    UNIFICA REGISTRO                                  
//* ***    GERA ARQUIVO TOTAL BANCO E ARQUIVO TOTAL SACADO   
//*                                                          
//EMITITLR DD DSN=*.STEP1.SORT.SORTOUT,                      
//       DISP=(OLD,DELETE,KEEP)                              
//CONSSAC  DD DSN=MX.SLIG.SACTIT07(+1),                      
//       DISP=(,CATLG,DELETE),                               
//       UNIT=(DISCO,07),                                    
//       SPACE=(TRK,(008562,01712),RLSE),                    
//       DCB=(MX.A,LRECL=0039,RECFM=FB)                      
//TOTBCO   DD DSN=MX.SLIG.SACTIT08(+1),                      
//       DISP=(,CATLG,DELETE),          
//       UNIT=DISCO,                    
//       SPACE=(TRK,(000005,0002),RLSE),
//       DCB=(MX.A,LRECL=0024,RECFM=FB) 
//SRELCTRL DD SYSOUT=*                  
//SYSOUT   DD SYSOUT=*                  
//SYSUDUMP DD SYSOUT=Y                  
//*                                     
