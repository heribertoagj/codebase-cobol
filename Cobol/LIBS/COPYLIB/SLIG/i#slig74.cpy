      ***===========================================================***
      *** NOME INC                                     LENGTH=00300 ***
      *** I#SLIG74                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - EMITENTE - MOVIMENTO          ***
      ***            PARA RELATORIO                                 ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CHEQUE-EMITENTE-RELATO.
           05 SLIG74-CGC-CPF-EMITT          PIC 9(009) COMP-3.
           05 SLIG74-FILIAL-EMITT           PIC 9(004) COMP-3.
           05 SLIG74-CONTROLE-EMITT         PIC 9(002) COMP-3.
           05 SLIG74-TIPO-PESSOA-EMITT      PIC X(001).
           05 SLIG74-BCO-EMITT              PIC 9(003) COMP-3.
           05 SLIG74-AGENCIA-EMITT          PIC 9(005) COMP-3.
           05 SLIG74-CONTA-EMITT            PIC 9(013) COMP-3.
           05 SLIG74-NUM-CHEQUE             PIC 9(007) COMP-3.
           05 SLIG74-DIG-CHEQUE             PIC X(001).
           05 SLIG74-NUM-LOTE               PIC 9(007) COMP-3.
           05 SLIG74-CARTEIRA               PIC 9(003) COMP-3.
           05 SLIG74-NOME-EMITT             PIC X(040).
           05 SLIG74-BCO-CLI                PIC 9(003) COMP-3.
           05 SLIG74-AGENCIA-CLI            PIC 9(005) COMP-3.
           05 SLIG74-CONTA-CLI              PIC 9(013) COMP-3.
           05 SLIG74-CGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG74-FILIAL-CLI             PIC 9(004) COMP-3.
           05 SLIG74-CONTROLE-CLI           PIC 9(002) COMP-3.
           05 SLIG74-TIPO-PESSOA-CLI        PIC X(001).
           05 SLIG74-NOME-CLI               PIC X(040).
           05 SLIG74-DT-DEPOSITO            PIC 9(008).
           05 SLIG74-DT-DEPOSITO-RED REDEFINES SLIG74-DT-DEPOSITO.
              10 SLIG74-ANO-DEPOSITO        PIC 9(004).
              10 SLIG74-MES-DEPOSITO        PIC 9(002).
              10 SLIG74-DIA-DEPOSITO        PIC 9(002).
           05 SLIG74-DT-CONTABIL            PIC 9(008).
           05 SLIG74-DT-CONTABIL-RED REDEFINES SLIG74-DT-CONTABIL.
              10 SLIG74-ANO-CONTABIL        PIC 9(004).
              10 SLIG74-MES-CONTABIL        PIC 9(002).
              10 SLIG74-DIA-CONTABIL        PIC 9(002).
           05 SLIG74-DT-ENTRADA-ORIGEM      PIC 9(008).
           05 SLIG74-DT-ENTRADA-ORIGEM-RED REDEFINES
              SLIG74-DT-ENTRADA-ORIGEM.
              10 SLIG74-ANO-ENTRADA-ORIGEM  PIC 9(004).
              10 SLIG74-MES-ENTRADA-ORIGEM  PIC 9(002).
              10 SLIG74-DIA-ENTRADA-ORIGEM  PIC 9(002).
           05 SLIG74-DT-SITUACAO            PIC 9(008).
           05 SLIG74-DT-SITUACAO-RED REDEFINES SLIG74-DT-SITUACAO.
              10 SLIG74-ANO-SITUACAO        PIC 9(004).
              10 SLIG74-MES-SITUACAO        PIC 9(002).
              10 SLIG74-DIA-SITUACAO        PIC 9(002).
           05 SLIG74-CTPO-CHEQ              PIC 9(003) COMP-3.
           05 SLIG74-VL-PRINC-CHEQ          PIC 9(013)V99 COMP-3.
           05 SLIG74-BCO-DIR-REGIONAL       PIC 9(003) COMP-3.
           05 SLIG74-DIR-REGIONAL           PIC 9(005) COMP-3.
           05 SLIG74-BCO-GERC-REGIONAL      PIC 9(003) COMP-3.
           05 SLIG74-GERC-REGIONAL          PIC 9(005) COMP-3.
           05 SLIG74-GRP-ECONOMICO-EMITT    PIC 9(009) COMP-3.
           05 SLIG74-GRP-ECONOMICO-CLI      PIC 9(009) COMP-3.
           05 SLIG74-CODI-DEVOL-CHQ         PIC 9(02).
      *
      *    IDENTIFICA NOME DO EMITENTE COLETADO DO SERASA
           05 SLIG74-NOME-EMITT-SERASA      PIC X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG74-CGC-CPF-EMITT-ST       PIC X(009).
           05 SLIG74-FILIAL-EMITT-ST        PIC X(004).
           05 SLIG74-CONTROLE-EMITT-ST      PIC 9(002) COMP-3.
           05 SLIG74-CGC-CPF-CLI-ST         PIC X(009).
           05 SLIG74-FILIAL-CLI-ST          PIC X(004).
           05 SLIG74-CONTROLE-CLI-ST        PIC 9(002) COMP-3.
           05 SLIG74-FILLER                 PIC X(68).
