//GFCT2214 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    *---------------------------------------------------------*
//* ***    *  CLIENTES SEM CESTA - PREPARACAO CARGA GFCTB0N9
//* ***    *---------------------------------------------------------*
//*
//STEP1    EXEC SORTD
//*
//* ***    *---------------------------------------------------------*
//* ***    *  PREPARA ARQUIVO DE CONTAS SALARIO
//* ***    *---------------------------------------------------------*
//*
//SORTIN   DD DSN=MJ.CSAL.PRP.CONTASAL(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2214S01.CTASALAR(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,12),
//       SPACE=(TRK,(051500,10300),RLSE),
//       DCB=(MX.A,LRECL=0062,RECFM=FB)
//SYSIN    DD *
 INCLUDE COND=(1,2,PD,EQ,237)
 SORT FIELDS=COPY
 END
//*
