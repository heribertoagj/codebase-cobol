      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG27                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - DIARIA E DECENAL              ***
      *** CHEQUES  : NORMAL, EXCLUIDOS, COMPENSADOS,                ***
      ***            REAPRESENTADOS E DEVOLVIDOS                    ***
      ***===========================================================***
DTSGP ***                                                           ***
DTSGP *** ALTERADA EM FEV/2005 POR GLAUCO PADOA                     ***
DTSGP *** MOTIVO : INCLUIR OS CAMPOS ABAIXO NO SLIG27-FILLER        ***
DTSGP ***           1. O CODIGO DA DEVOLUCAO                        ***
DTSGP ***           2. O TIPO DE SERVICO                            ***
DTSGP ***                                                           ***
DTSGP ***-----------------------------------------------------------***
      *
       01  REG-CHEQUE-DIARIA-DECENAL.
           05 SLIG27-DT-MOVIMENTO           PIC 9(008).
           05 SLIG27-DT-MOVIMENTO-RED REDEFINES SLIG27-DT-MOVIMENTO.
              10 SLIG27-ANO-MOVIMENTO       PIC 9(004).
              10 SLIG27-MES-MOVIMENTO       PIC 9(002).
              10 SLIG27-DIA-MOVIMENTO       PIC 9(002).
           05 SLIG27-CGC-CPF-EMITT          PIC 9(009) COMP-3.
           05 SLIG27-FILIAL-EMITT           PIC 9(004) COMP-3.
           05 SLIG27-CONTROLE-EMITT         PIC 9(002) COMP-3.
           05 SLIG27-BCO-EMITT              PIC 9(003) COMP-3.
           05 SLIG27-AGENCIA-EMITT          PIC 9(005) COMP-3.
           05 SLIG27-CONTA-EMITT            PIC 9(013) COMP-3.
           05 SLIG27-NUM-CHEQUE             PIC 9(007) COMP-3.
           05 SLIG27-DIG-CHEQUE             PIC X(001).
           05 SLIG27-NUM-LOTE               PIC 9(007) COMP-3.
           05 SLIG27-CARTEIRA               PIC 9(003) COMP-3.
              88 SLIG27-CARTEIRA-SIMPLES    VALUE 1.
              88 SLIG27-CARTEIRA-CAUCIONADA VALUE 2.
              88 SLIG27-CARTEIRA-DESCONTO   VALUE 3.
           05 SLIG27-BCO-CLI                PIC 9(003) COMP-3.
           05 SLIG27-AGENCIA-CLI            PIC 9(005) COMP-3.
           05 SLIG27-CONTA-CLI              PIC 9(013) COMP-3.
           05 SLIG27-DT-DEPOSITO            PIC 9(008).
           05 SLIG27-DT-DEPOSITO-RED REDEFINES SLIG27-DT-DEPOSITO.
              10 SLIG27-ANO-DEPOSITO        PIC 9(004).
              10 SLIG27-MES-DEPOSITO        PIC 9(002).
              10 SLIG27-DIA-DEPOSITO        PIC 9(002).
           05 SLIG27-DT-CONTABIL            PIC 9(008).
           05 SLIG27-DT-CONTABIL-RED REDEFINES SLIG27-DT-CONTABIL.
              10 SLIG27-ANO-CONTABIL        PIC 9(004).
              10 SLIG27-MES-CONTABIL        PIC 9(002).
              10 SLIG27-DIA-CONTABIL        PIC 9(002).
           05 SLIG27-DT-ENTRADA-ORIGEM      PIC 9(008).
           05 SLIG27-DT-ENTRADA-ORIGEM-RED REDEFINES
              SLIG27-DT-ENTRADA-ORIGEM.
              10 SLIG27-ANO-ENTRADA-ORIGEM  PIC 9(004).
              10 SLIG27-MES-ENTRADA-ORIGEM  PIC 9(002).
              10 SLIG27-DIA-ENTRADA-ORIGEM  PIC 9(002).
           05 SLIG27-DT-SITUACAO            PIC 9(008).
           05 SLIG27-DT-SITUACAO-RED REDEFINES SLIG27-DT-SITUACAO.
              10 SLIG27-ANO-SITUACAO        PIC 9(004).
              10 SLIG27-MES-SITUACAO        PIC 9(002).
              10 SLIG27-DIA-SITUACAO        PIC 9(002).
           05 SLIG27-CTPO-CHEQ              PIC 9(003) COMP-3.
      *
      *       EXCLUIDO                   = 001
      *       COMPENSADO                 = 002
      *       REAPRESENTADO              = 003
      *       NORMAL                     = 004
      *       DEVOLVIDO                  = 005
           05 SLIG27-VL-PRINC-CHEQ          PIC 9(013)V99 COMP-3.
DTSGP      05 SLIG27-CODI-DEVOL-CHQ         PIC 9(02).
DTSGP      05 SLIG27-CTPO-SERV              PIC 9(02).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG27-CGC-CPF-EMITT-ST       PIC X(009).
           05 SLIG27-FILIAL-EMITT-ST        PIC X(004).
           05 SLIG27-CONTROLE-EMITT-ST      PIC 9(002) COMP-3.
           05 SLIG27-FILLER                 PIC X(186).
