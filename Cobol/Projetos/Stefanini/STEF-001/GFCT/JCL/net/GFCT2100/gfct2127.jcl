//GFCT2127 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP0    EXEC SORTD
//*
//* ***    CLASSIFICA ARQUIVO DE CLIENTES
//* ***    ORDEM: AGENCIA E CONTA
//* ***    GRAVANDO NA SAIDA APENAS REGISTROS DE POUPANCA
//*
//SORTIN   DD DSN=MX.BISD.PRP.REDUZIDO(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.POUPREDU(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0030,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(2,3,A,5,4,A),FORMAT=BI
 OUTFIL INCLUDE=(5,4,PD,GT,999999)
 END
//*
//STEP1    EXEC SORTD
//*
//* ***    CLASSIFICA ARQUIVO DE CLIENTES
//* ***    ORDEM: AGENCIA E CONTA
//* ***    ALTERANDO LAYOUT DE SAIDA PARA 07 BYTES
//* ***       01   REGISTRO.
//* ***         05 AGENCIA 9(05) COMP-3.
//* ***         05 CONTA   9(07) COMP-3.
//*
//SORTIN   DD DSN=*.STEP0.SORT.SORTOUT,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.CADAPOUP(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0007,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(2,3,A,5,4,A),FORMAT=BI
 OUTREC FIELDS=(2,3,5,4)
 END
//*
//STEP2    EXEC PGM=GFCT2117
//*
//* ***    -----------------------------------------------------
//* ***    CADASTR0 DE CLIENTES POUP SOMENTE COM AGENCIA VALIDAS
//* ***    -----------------------------------------------------
//*
//CADAPOUP DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//POUPADES DD DSN=MX.GFCT.POUPADE0(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0055,RECFM=FB)
//POUPINVA DD DSN=MX.GFCT.POUPINVA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(01000,200),RLSE),
//       DCB=(MX.A,LRECL=0007,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC SORTD
//*
//* ***    ----------------------------------
//* ***    ORDENA POR AGENCIA, CONTA E RAZAO
//* ***    ----------------------------------
//*
//SORTIN   DD DSN=MJ.CLIE.PRP.CADPREDU.ALFA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.CADPREDS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,08),
//       SPACE=(TRK,(037600,07520),RLSE),
//       DCB=(MX.A,LRECL=0029,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(1,3,A,7,4,A,4,3,A),FORMAT=BI
 END
//*
//STEP4    EXEC PGM=GFCT2118
//*
//* ***    -----------------------------------------------------
//* ***    CADASTR0 DE CLIENTES POUP COM INFORMACAO DE CPF/CNPJ
//* ***    -----------------------------------------------------
//*
//POUPADES DD DSN=*.STEP2.POUPADES,
//       DISP=SHR
//CADPREDU DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//POUPADE1 DD DSN=MX.GFCT.POUPADE1(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0055,RECFM=FB)
//POUPSCAD DD DSN=MX.GFCT.POUPSCAD(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(01000,200),RLSE),
//       DCB=(MX.A,LRECL=0055,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5    EXEC SORTD
//*
//* ***    ------------------------------
//* ***    ORDENA POR FILIAL DO CPF/CNPJ
//* ***    ------------------------------
//*
//SORTIN   DD DSN=*.STEP4.POUPADE1,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.POUPADE2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0055,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(16,3,A),FORMAT=BI
 END
//*
//STEP6    EXEC PGM=GFCT2119,
//       PARM='F01237-J01238'
//*
//* ***    ----------------------------------------------
//* ***    ASSOCIA TARIFA DE POUPANCA POR TIPO DE PESSOA
//* ***    ----------------------------------------------
//*
//POUPADE2 DD DSN=*.STEP5.SORT.SORTOUT,
//       DISP=SHR
//POUPADES DD DSN=MX.GFCT.POUPADES(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0050,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
