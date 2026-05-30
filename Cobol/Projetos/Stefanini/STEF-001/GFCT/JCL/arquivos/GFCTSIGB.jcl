//GFCTSIGB JOB 'GFCT,4220,PR14','D115848',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    ADEQUA ARQUIVO SIGB PARA CNPJ ALFANUMERICO
//* ***    EXECUTA NO GFCT1412
//*
//STEP1    EXEC SORTD                                             
//*                                                               
//SORTIN   DD DSN=MX.GFCT.PRP.DADOSIGB(0),         
//       DISP=SHR                                                 
//SORTOUT  DD DSN=MX.GFCT.PRP.DADOSIGB(+1),                       
//         DISP=(,CATLG,DELETE),                                    
//         UNIT=(DISCO,04),                                         
//         SPACE=(TRK,(013570,02714),RLSE),                         
//         DCB=(MX.A,LRECL=0054,RECFM=FB)                           
//SYSIN    DD *                                                   
  INREC BUILD=(1,34,35:19,5,PD,TO=ZD,LENGTH=9,44:24,3,PD,TO=ZD,   
               LENGTH=4,48:27,2,50:5X)                            
  SORT FIELDS=(35,9,A,44,4,A,48,2,A,8,5,PD,A,                     
               1,3,PD,A,29,6,A),FORMAT=BI                         
  END                                                             
//*                                                               
//SYSOUT   DD SYSOUT=*                                            
//SYSUDUMP DD SYSOUT=Y                                            
//*                                                               
