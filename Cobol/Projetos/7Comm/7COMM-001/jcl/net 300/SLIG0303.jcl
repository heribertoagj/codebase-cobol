   File  Edit  Edit_Settings  Menu  Utilities  Compilers  Test  Help
 sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
 CM2A       MX.JCLFILE(SLIG0303) - 01.00                    Columns 0000
 Command ===>                                                  Scroll ==
 000002 //JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
 000003 //         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
 000004 //         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
 000005 //STEP1    EXEC PGM=PLAN2440,
 000006 //       PARM=001
 000007 //*
 000008 //* ***    RECEBE ARQUIVO TRANSITO DA ROTINA CBON
 000009 //*
 000010 //ENT00001 DD DSN=TM.CBON.SLIG.TTLTOTAL,
 000011 //       DISP=(OLD,DELETE,KEEP)
 000012 //SAI00001 DD DSN=MX.SLIG.PRV.TTLTOTAL(+1),
 000013 //       DISP=(,CATLG,DELETE),
 000014 //       UNIT=(DISCO,40),
 000015 //       SPACE=(TRK,(006000,001980),RLSE),
 000016 //       DCB=(MX.A),
 000017 //       DATACLAS=PRODX37
 000018 //PRINTER  DD SYSOUT=*
   File  Edit  Edit_Settings  Menu  Utilities  Compilers  Test  Help
 sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
 CM2A       MX.JCLFILE(SLIG0303) - 01.00                    Columns 0000
 Command ===>                                                  Scroll ==
 000022 //STEP1A   EXEC PGM=SLIG4615
 000023 //*
 000024 //* ***    GERA TOTAIS DE CONTROLE DO ARQUIVO RECEPCIONADO
 000025 //*
 000026 //ARQDATAS DD DSN=MX.SLIG.PRV.PERTITLO(0),
 000027 //       DISP=OLD
 000028 //TTLTOTAE DD DSN=*.STEP1.SAI00001,
 000029 //       DISP=OLD
 000030 //ARQRECEP DD DSN=MX.SLIG.J0303S02.ARQRECEP(+1),
 000031 //       DISP=(,CATLG,DELETE),
 000032 //       UNIT=DISCO,
 000033 //       SPACE=(TRK,(000010,000002),RLSE),
 000034 //       DCB=(MX.A,LRECL=0142,RECFM=FB)
 000035 //SYSOUT   DD SYSOUT=*
 000036 //SYSUDUMP DD SYSOUT=Y
 000037 //*
 000038 //STEP2    EXEC PGM=SLIG6000,
 000042 //* ***    LE A TABELA:
 000043 //*
 000044 //* ***    CONDE CODE  =  0 DISPARA ROTINA DECENAL /// SLIG1000/
 000045 //* ***    CONDE CODE < > NAO EXECUTA
 000046 //*
 000047 //ARQDATAS DD DSN=MX.SLIG.PRV.PERTITLO(0),
 000048 //       DISP=SHR
 000049 //SYSOUT   DD SYSOUT=*
 000050 //SYSUDUMP DD SYSOUT=Y
 000051 //*
