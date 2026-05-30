//SLIG1152 JOB 'IMCLIENT,REFMT','A999036',CLASS=A,MSGCLASS=X,REGION=0M,
//         NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* COPIA GDG MX.SLIG.IMCLIENT(0) VIA DFSORT COM SAIDA LRECL=146 (FB).
//*
//* AJUSTAR: JOBNAME, CLASS, MSGCLASS, NOTIFY, DSN/SPACE DA SAIDA,
//*          STEPLIB (SE O SORT NAO ESTIVER NO LINKLIST).
//*
//* Se o LRECL de entrada for diferente de 146, inclua na SYSIN por
//* exemplo OUTREC BUILD=(1,146) (trunca) ou regra de padding do caso.
//* DSN de saida: ajuste; (+1) so vale se existir GDG com essa base.
//*--------------------------------------------------------------------
//STEP0   EXEC PGM=SORT
//*STEPLIB DD DSN=SYS1.SORTLINK,DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SORTIN   DD DSN=MX.SLIG.IMCLIENT(0),DISP=SHR
//SORTOUT  DD DSN=MX.SLIG.IMCLIENT.QG(+1),
//            DISP=(,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=145,BLKSIZE=27930)
//SYSIN    DD *
 SORT FIELDS=COPY
/*
//
