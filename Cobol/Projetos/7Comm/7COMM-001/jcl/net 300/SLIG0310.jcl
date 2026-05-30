//SLIG0310 JOB 'SLIG,4510,PR11','C085384',MSGCLASS=Z,SCHENV=BATCH  
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                             
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                        
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                        
//STEP2    EXEC PGM=SLIG6011                                       
//*                                                                
//* ***    SELECIONA CAMPOS PARA GERAR ARQUIVO REDUZIDO..          
//*                                                                
//TITPENDE DD DSN=MX.SLIG.PRV.TTLTOTAL(0),                         
//       DISP=SHR                                                  
//IDSACDO  DD DSN=MX.SLIG.SACDOCOT(+1),                            
//       DISP=(,CATLG,DELETE),                                     
//       UNIT=(DISCO,18),                                          
//       SPACE=(TRK,(019650,3932),RLSE),                           
//       DATACLAS=PRODX37,                                         
//       DCB=(MX.A,LRECL=0100,RECFM=FB)                            
//ARQUREDU DD DSN=MX.SLIG.PENDCOT1(+1),                            
//       DISP=(,CATLG,DELETE),                                     
//       UNIT=(DISCO,20),                                          
//       SPACE=(TRK,(028308,5662),RLSE),            
//       DCB=(MX.A,LRECL=0182,RECFM=FB)             
//ARQREDUZ DD DSN=MX.SLIG.ARQREDZD(+1),             
//       DISP=(,CATLG,DELETE),                      
//       UNIT=(DISCO,03),                           
//       SPACE=(TRK,(001695,339),RLSE),             
//       DCB=(MX.A,LRECL=0182,RECFM=FB)             
//QTREGOUT DD DSN=MX.SLIG.QTRECONT(+1),             
//       DISP=(,CATLG,DELETE),                      
//       UNIT=DISCO,                                
//       SPACE=(TRK,(000005,0002),RLSE),            
//       DCB=(MX.A,LRECL=0020,RECFM=FB)             
//SRELCTRL DD SYSOUT=*                              
//PRINTER  DD SYSOUT=*                              
//SYSOUT   DD SYSOUT=*                              
//SYSUDUMP DD SYSOUT=Y                              
//*                                                 
//STEP3    EXEC PGM=BRAD0260                        
//*                                                 
//* ***    CLASSIFICA ARQUIVO REDUZIDO POR :                           
//* ***    BANCO, PRODUTO, NEGOCIACAO E NOSSO NUMERO                   
//*                                                                    
//SORTIN   DD DSN=*.STEP2.ARQUREDU,                                    
//       DISP=(OLD,DELETE,KEEP)                                        
//SORTOUT  DD DSN=MX.SLIG.PDRECT1C(+1),                                
//       DISP=(,CATLG,DELETE),                                         
//       UNIT=(DISCO,20),                                              
//       SPACE=(TRK,(028308,5662),RLSE),                               
//       DCB=(MX.A,LRECL=0182,RECFM=FB)                                
//SYSIN    DD *                                                        
 SORT FIELDS=(1,2,A,102,2,A,104,5.4,A,110,5.4,A),FORMAT=BI             
 END                                                                   
//SORTWK01 DD UNIT=DISCO,                                              
//       SPACE=(CYL,(00555))                                           
//SORTWK02 DD UNIT=DISCO,                                              
//       SPACE=(CYL,(00555))                                           
//SORTWK03 DD UNIT=DISCO,                                              
//       SPACE=(CYL,(00555))                                           
//SORTWK04 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK05 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK06 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK07 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK08 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK09 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK10 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK11 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK12 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                                          
//SORTWK13 DD UNIT=DISCO,                                             
//       SPACE=(CYL,(00555))                            
//SORTWK14 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK15 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK16 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK17 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK18 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK19 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK20 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK21 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK22 DD UNIT=DISCO,                               
//       SPACE=(CYL,(00555))                            
//SORTWK23 DD UNIT=DISCO,                           
//       SPACE=(CYL,(00555))                        
//SORTWK24 DD UNIT=DISCO,                           
//       SPACE=(CYL,(00555))                        
//SORTWK25 DD UNIT=DISCO,                           
//       SPACE=(CYL,(00555))                        
//SORTWK26 DD UNIT=DISCO,                           
//       SPACE=(CYL,(00555))                        
//SORTWK27 DD UNIT=DISCO,                           
//       SPACE=(CYL,(00555))                        
//SORTWK28 DD UNIT=DISCO,                           
//       SPACE=(CYL,(00555))                        
//SORTWK29 DD UNIT=DISCO,                           
//       SPACE=(CYL,(00555))                        
//SORTWK30 DD UNIT=DISCO,                           
//       SPACE=(CYL,(00555))                        
//SYSOUT   DD SYSOUT=*                              
//SYSUDUMP DD SYSOUT=Y                              
//*                                                 
//STEP4    EXEC PGM=SLIG5043                                         
//*                                                                  
//* ***    ATUALIZA CAMPOS CLIENTE/CARTEIRA.                         
//*                                                                  
//ARQUREDU DD DSN=*.STEP3.SORTOUT,                                   
//       DISP=(OLD,DELETE,KEEP)                                      
//QTDREGIN DD DSN=*.STEP2.QTREGOUT,                                  
//       DISP=OLD                                                    
//ARQNEGOC DD DSN=MX.SLIG.PRV.NEGOCLIC(0),                           
//       DISP=SHR                                                    
//ARQPRODU DD DSN=MX.SLIG.PRV.CBGE001C(0),                           
//       DISP=SHR                                                    
//REDUATU1 DD DSN=MX.SLIG.REDPECT1(+1),                              
//       DISP=(,CATLG,DELETE),                                       
//       UNIT=(DISCO,07),                                            
//       SPACE=(TRK,(008517,01703),RLSE),                            
//       DCB=(MX.A,LRECL=0182,RECFM=FB)                              
//REDUATU2 DD DSN=MX.SLIG.REDPECT2(+1),                              
//       DISP=(,CATLG,DELETE),                                       
//       UNIT=(DISCO,08),                          
//       SPACE=(TRK,(009732,01946),RLSE),          
//       DCB=(MX.A,LRECL=0182,RECFM=FB)            
//REDUATU3 DD DSN=MX.SLIG.REDPECT3(+1),            
//       DISP=(,CATLG,DELETE),                     
//       UNIT=(DISCO,06),                          
//       SPACE=(TRK,(007176,01435),RLSE),          
//       DCB=(MX.A,LRECL=0182,RECFM=FB)            
//REDUATU4 DD DSN=MX.SLIG.REDPECT4(+1),            
//       DISP=(,CATLG,DELETE),                     
//       UNIT=(DISCO,07),                          
//       SPACE=(TRK,(008436,01687),RLSE),          
//       DCB=(MX.A,LRECL=0182,RECFM=FB)            
//REDUATU5 DD DSN=MX.SLIG.REDPECT5(+1),            
//       DISP=(,CATLG,DELETE),                     
//       UNIT=(DISCO,08),                          
//       SPACE=(TRK,(009276,01855),RLSE),          
//       DCB=(MX.A,LRECL=0182,RECFM=FB)            
//ARQERROS DD DSN=MX.SLIG.ERROCONT(+1),            
//       DISP=(,CATLG,DELETE),                            
//       UNIT=DISCO,                                      
//       SPACE=(TRK,(000005,0002),RLSE),                  
//       DCB=(MX.A,LRECL=0109,RECFM=FBA)                  
//PRINTER  DD SYSOUT=*                                    
//SYSOUT   DD SYSOUT=*                                    
//SYSUDUMP DD SYSOUT=Y                                    
//SRELCTRL DD SYSOUT=*                                    
//*                                                       
