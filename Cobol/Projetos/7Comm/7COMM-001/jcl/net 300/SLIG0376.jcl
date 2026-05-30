//SLIG0376 JOB 'SLIG,4510,PR11','B29253',MSGCLASS=Z,SCHENV=BATCH    
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                              
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                         
//STEP1    EXEC SORTD                                               
//*                                                                 
//* ***    CLASSIFICA POR CGC-SACADO, TIPO SACADO ,                 
//* ***    VLR-DESCONTO , VLR-CAUCIONADA , VLR-SIMPLES.             
//*                                                                 
//SORTIN   DD DSN=MX.SLIG.SACTIT07(0),                              
//       DISP=OLD                                                   
//SORTOUT  DD DSN=MX.SLIG.SACTIT7C(+1),                             
//       DISP=(,CATLG,DELETE),                                      
//       UNIT=(DISCO,07),                                           
//       SPACE=(TRK,(008562,01712),RLSE),                           
//       DCB=(MX.A,LRECL=0039,RECFM=FB)                             
//SORTLIB  DD DSN=SYS1.SORTLIB,                                     
//       DISP=SHR                                                   
//SORTWK01 DD UNIT=DISCO,                                           
//       SPACE=(CYL,(00210))                                        
//SORTWK02 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK03 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK04 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK05 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK06 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK07 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK08 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK09 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK10 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                              
//SORTWK11 DD UNIT=DISCO,                                 
//       SPACE=(CYL,(00210))                
//SORTWK12 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK13 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK14 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK15 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK16 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK17 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK18 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK19 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK20 DD UNIT=DISCO,                   
//       SPACE=(CYL,(00210))                
//SORTWK21 DD UNIT=DISCO,                                            
//       SPACE=(CYL,(00210))                                         
//SORTWK22 DD UNIT=DISCO,                                            
//       SPACE=(CYL,(00210))                                         
//SORTWK23 DD UNIT=DISCO,                                            
//       SPACE=(CYL,(00210))                                         
//SYSIN    DD *                                                      
 SORT FIELDS=(31,9,A,06,1,A,07,8,D,15,8,D,23,8,D),FORMAT=BI          
 END                                                                 
//*                                                                  
//STEP2    EXEC PGM=SLIG5510,                                        
//       PARM='01'                                                   
//*                                                                  
//* ***    CALCULA INDICE CONCENTRACAO GERANDO POR CGC SACADO        
//* ***    PARM = 01 - TITULOS                                       
//*                                                                  
//CONSSACE DD DSN=*.STEP1.SORT.SORTOUT,                              
//       DISP=SHR                                                    
//TOTBCO   DD DSN=MX.SLIG.SACTIT08(0),                               
//       DISP=OLD                                
//CONSSACS DD DSN=MX.SLIG.SACTIT09(+1),          
//       DISP=(,CATLG,DELETE),                   
//       UNIT=(DISCO,10),                        
//       SPACE=(TRK,(012390,02478),RLSE),        
//       DCB=(MX.A,LRECL=0051,RECFM=FB)          
//SRELCTRL DD SYSOUT=*                           
//SYSOUT   DD SYSOUT=*                           
//SYSUDUMP DD SYSOUT=Y                           
//*                                              
