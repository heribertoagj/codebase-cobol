//GFCT2212 JOB 'GFCT,4220,PR14','D115848',MSGCLASS=Z,SCHENV=BATCH
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
//* ***    *  PREPARA ARQUIVO DE CONTAS INATIVAS
//* ***    *---------------------------------------------------------*
//*
//SORTIN   DD DSN=MJ.CCMS.PRP.IMGTAB12.DIANC(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2212S01.CTAINATI(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,07),
//       SPACE=(TRK,(026950,05390),RLSE),
//       DCB=(MX.A,LRECL=0047,RECFM=FB)
//SYSIN    DD *
  INCLUDE COND=(1,3,PD,EQ,237,AND,
                17,2,PD,EQ,007,AND,
                19,2,PD,EQ,050,AND,
               (42,2,PD,EQ,001,OR,
                42,2,PD,EQ,002,OR,
                42,2,PD,EQ,005))
  SORT FIELDS=(4,3,A,10,7,A),FORMAT=BI
  END
//*
